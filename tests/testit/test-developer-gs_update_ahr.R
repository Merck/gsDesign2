# A two-analysis asymmetric design with beta-spending, non-binding futility
x <- gs_design_ahr(
  enroll_rate = define_enroll_rate(duration = c(2, 2, 10), rate = (1:3) / 3),
  fail_rate = define_fail_rate(duration = c(3, Inf), fail_rate = log(2) / 9,
                               hr = c(1, 0.6), dropout_rate = .0001),
  alpha = 0.025, beta = 0.1, ratio = 1, info_scale = "h0_info",
  info_frac = NULL, analysis_time = c(20, 36),
  upper = gs_spending_bound, upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  test_upper = TRUE,
  lower = gs_spending_bound, lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1),
  test_lower = c(TRUE, FALSE), binding = FALSE
) |> to_integer()

# ---- Increase the number of analyses from 2 to 3 ---------------------------
event_tbl <- data.frame(analysis = c(1, 1, 2, 2, 3, 3),
                        event = c(20, 80, 30, 150, 40, 250))
obs <- as.numeric(tapply(event_tbl$event, event_tbl$analysis, sum))
ustime <- obs / max(obs)

xu <- gs_update_ahr(
  x = x, ustime = ustime, lstime = ustime, event_tbl = event_tbl,
  test_upper = c(FALSE, TRUE, TRUE), test_lower = c(TRUE, TRUE, FALSE)
)

assert("the updated design has the requested number of analyses", {
  (nrow(xu$analysis) %==% 3L)
})

assert("efficacy is not reported where test_upper is FALSE", {
  (!any(xu$bound$bound == "upper" & xu$bound$analysis == 1))
})

assert("futility is not reported where test_lower is FALSE", {
  (!any(xu$bound$bound == "lower" & xu$bound$analysis == 3))
})

assert("no infinite (untested) bounds are reported", {
  (is.finite(xu$bound$z))
})

# ---- Decrease the number of analyses from 2 to 1 ---------------------------
xd <- gs_update_ahr(
  x = x, ustime = 1, event_tbl = data.frame(analysis = c(1, 1), event = c(40, 260)),
  test_upper = TRUE, test_lower = FALSE
)

assert("the updated design can drop interim analyses", {
  (nrow(xd$analysis) %==% 1L)
})

# ---- Testing selections default to the original design ---------------------
xa <- gs_update_ahr(x = x, alpha = 0.05)
assert("alpha-only update keeps the original number of analyses", {
  (nrow(xa$analysis) %==% 2L)
})

# ---- Errors ----------------------------------------------------------------
assert("a testing vector whose length disagrees with the number of analyses errors", {
  (has_error(gs_update_ahr(
    x = x, ustime = ustime, lstime = ustime, event_tbl = event_tbl,
    test_upper = c(TRUE, TRUE)  # length 2 but event_tbl has 3 analyses
  )))
})

assert("an analysis beyond the original design with no observed events errors", {
  # original design has 2 analyses; ask for 4 but omit events for analysis 3
  (has_error(gs_update_ahr(
    x = x,
    ustime = c(0.4, 0.6, 0.8, 1), lstime = c(0.4, 0.6, 0.8, 1),
    event_tbl = data.frame(analysis = c(1, 1, 2, 2, 4, 4),
                           event = c(20, 80, 30, 150, 40, 250)),
    test_upper = c(FALSE, TRUE, TRUE, TRUE),
    test_lower = c(TRUE, TRUE, FALSE, FALSE),
    test_harm = FALSE
  )))
})
