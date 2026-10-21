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

# ---- Add or delete analyses --------------------------------------------------
# Designs with 3 analyses at months 12, 24, 36 and different lower bounds: none
# (efficacy only, i.e. gs_b with -Inf, #684), beta-spending, and fixed (gs_b).
er <- define_enroll_rate(duration = c(2, 2, 10), rate = (1:3) / 3)
fr <- define_fail_rate(duration = c(3, Inf), fail_rate = log(2) / 9,
                       hr = c(1, 0.6), dropout_rate = .0001)
fr1 <- transform(fr, fail_rate = fail_rate * hr)
make_design <- function(lower, lpar, test_lower) {
  gs_design_ahr(
    enroll_rate = er, fail_rate = fr, alpha = 0.025, beta = 0.1, ratio = 1,
    info_scale = "h0_info", info_frac = NULL, analysis_time = c(12, 24, 36),
    upper = gs_spending_bound, upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    test_upper = TRUE, lower = lower, lpar = lpar, test_lower = test_lower,
    binding = FALSE
  )
}
designs <- list(
  efficacy_only = make_design(gs_b, rep(-Inf, 3), FALSE),
  beta_spending = make_design(
    gs_spending_bound, list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -4), TRUE
  ),
  fixed_lower = make_design(gs_b, c(-1, -Inf, -Inf), c(TRUE, FALSE, FALSE))
)

# Update `x` with observed events equal to the expected events (both arms) at the
# analysis times `tm`, and compute the same design directly via gs_power_ahr()
update_vs_power <- function(x, tm) {
  k <- length(tm)
  ev <- lapply(tm, function(t) {
    (expected_event(er, fr, t, simple = FALSE)$event +
       expected_event(er, fr1, t, simple = FALSE)$event) / 2
  })
  event_tbl <- data.frame(analysis = rep(seq_len(k), each = length(ev[[1]])), event = unlist(ev))
  obs <- as.numeric(tapply(event_tbl$event, event_tbl$analysis, sum))
  ustime <- obs / max(obs)
  spending <- is.list(x$input$lpar)
  two_sided <- !all(x$bound$bound == "upper")
  test_lower <- rep(two_sided, k)
  xu <- gs_update_ahr(
    x = x, ustime = ustime, lstime = if (spending) ustime, event_tbl = event_tbl,
    test_upper = rep(TRUE, k), test_lower = test_lower
  )
  lpar <- if (spending) c(x$input$lpar, list(timing = ustime)) else
    pad_fixed_par(x$input$lpar, k)[seq_len(k)]
  xp <- gs_power_ahr(
    enroll_rate = er, fail_rate = fr, ratio = 1, info_scale = "h0_info",
    analysis_time = tm, event = NULL, binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, timing = ustime),
    test_upper = TRUE, lower = x$input$lower, lpar = lpar, test_lower = test_lower
  )
  # gs_update_ahr() lists all upper bounds first, so sort rows to compare
  sort_bound <- function(b) b[order(b$analysis, b$bound), ]
  list(update = sort_bound(xu$bound), power = sort_bound(xp$bound), n = nrow(xu$analysis))
}

for (nm in names(designs)) for (tm in list(c(12, 20, 28, 36), c(24, 36))) {
  res <- update_vs_power(designs[[nm]], tm)
  assert(sprintf("%s design updated to analyses at %s matches gs_power_ahr()", nm, toString(tm)), {
    (res$n %==% length(tm))
    (is.finite(res$update$z))
    (all.equal(res$update$z, res$power$z))
    (all.equal(res$update$probability, res$power$probability))
  })
}

assert("a length mismatch between per-analysis inputs errors", {
  x <- designs$efficacy_only
  (has_error(gs_update_ahr(
    x, ustime = c(0.5, 0.8, 1), event_tbl = data.frame(analysis = c(1, 2, 3, 4), event = c(5, 6, 7, 8)),
    test_upper = rep(TRUE, 4)
  )))
})

assert("a design with fixed efficacy and futility bounds notes that they are unchanged", {
  x <- gs_design_ahr(
    enroll_rate = er, fail_rate = fr, alpha = 0.025, beta = 0.1, ratio = 1,
    analysis_time = c(24, 36), upper = gs_b, upar = c(3, 2), lower = gs_b, lpar = c(-1, 1)
  )
  (grepl("unchanged", tryCatch(gs_update_ahr(x, alpha = 0.05), message = conditionMessage)))
})

# ---- pad_fixed_par() ---------------------------------------------------------
assert("pad_fixed_par() pads a fixed par vector by repeating its last value", {
  (pad_fixed_par(rep(-Inf, 3), 4) %==% rep(-Inf, 4))
  (pad_fixed_par(c(-1, -Inf, -Inf), 5) %==% c(-1, -Inf, -Inf, -Inf, -Inf))
})

assert("pad_fixed_par() leaves a long enough vector and spending lists unchanged", {
  (pad_fixed_par(c(-1, -Inf, -Inf), 3) %==% c(-1, -Inf, -Inf))
  (pad_fixed_par(c(-1, -Inf, -Inf), 2) %==% c(-1, -Inf, -Inf))
  sp <- list(sf = gsDesign::sfLDOF, total_spend = 0.025)
  (pad_fixed_par(sp, 4) %==% sp)
})
