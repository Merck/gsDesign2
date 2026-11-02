# --------------------- Example 1 ----------------------------------------- #
# A two-analysis asymmetric design with beta-spending, non-binding futility #
# ------------------------------------------------------------------------- #
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
  (!"n" %in% names(xu$analysis))
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
  (!"n" %in% names(xd$analysis))
})

# ---- Testing selections default to the original design ---------------------
xa <- gs_update_ahr(x = x, alpha = 0.05)
assert("alpha-only update keeps the original number of analyses", {
  (nrow(xa$analysis) %==% 2L)
  (!"n" %in% names(xa$analysis))
})

assert("updated designs can be summarized without reporting sample size", {
  for (updated_design in list(xu, xd, xa)) {
    summary_headers <- attr(summary(updated_design), "groups")$Analysis
    (!any(grepl("N:", summary_headers, fixed = TRUE)))
    (all(grepl("Events:", summary_headers, fixed = TRUE)))
    bound_headers <- gs_bound_summary(updated_design)$Analysis
    (!any(grepl("N:", bound_headers, fixed = TRUE)))
    (sum(grepl("Events:", bound_headers, fixed = TRUE)) %==% nrow(updated_design$analysis))
  }
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

# --------------------- Example 2 ----------------------------------------- #
# A three-analysis efficacy-only design with non futility                     #
# ------------------------------------------------------------------------- #
x <- gs_design_ahr(
  enroll_rate = define_enroll_rate(duration = c(2, 2, 10), rate = (1:3) / 3),
  fail_rate = define_fail_rate(duration = c(3, Inf), fail_rate = log(2) / 9,
                               hr = c(1, 0.6), dropout_rate = .0001),
  alpha = 0.025, beta = 0.1, ratio = 1, info_scale = "h0_info",
  info_frac = NULL, analysis_time = c(20, 28, 36),
  upper = gs_spending_bound, upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  test_upper = TRUE,
  lower = gs_b, lpar = rep(-Inf, 3),
  test_lower = FALSE
) |> to_integer()

assert("Increase the number of analyses from 3 to 4", {
  event_tbl <- data.frame(analysis = c(1, 1, 2, 2, 3, 3, 4, 4),
                          event = c(20, 80, 30, 150, 40, 250, 50, 300))
  obs <- as.numeric(tapply(event_tbl$event, event_tbl$analysis, sum))
  ustime <- obs / max(obs)
  
  xu <- gs_update_ahr(x = x, ustime = ustime, lstime = NULL, event_tbl = event_tbl,
                      test_upper = TRUE, test_lower = FALSE)

  xp <- gs_power_ahr(enroll_rate = x$enroll_rate, fail_rate = x$fail_rate, 
                     ratio = x$input$ratio, 
                     info_scale = x$input$info_scale,
                     event = xu$analysis$event,
                     upper = gs_spending_bound,
                     upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025,
                                 timing = ustime),
                     lower = x$input$lower,
                     lpar = rep(-Inf, 4))
  # the updated design has the requested number of analyses
  (nrow(xu$analysis) %==% 4L)
  # the efficacy bounds match gs_power_ahr
  (all.equal(xp$bound$z, xu$bound$z, tolerance = 1e-7, scale = 1))
})

assert("Decrease the number of analyses from 3 to 2", {
  event_tbl <- data.frame(analysis = c(1, 1, 2, 2),
                          event = c(30, 100, 50, 300))
  obs <- as.numeric(tapply(event_tbl$event, event_tbl$analysis, sum))
  ustime <- obs / max(obs)

  xu <- gs_update_ahr(x = x, ustime = ustime, lstime = NULL, event_tbl = event_tbl,
                      test_upper = TRUE, test_lower = FALSE)
  
  xp <- gs_power_ahr(enroll_rate = x$enroll_rate, fail_rate = x$fail_rate, 
                     ratio = x$input$ratio, 
                     info_scale = x$input$info_scale,
                     event = xu$analysis$event,
                     upper = gs_spending_bound,
                     upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025,
                                 timing = ustime),
                     lower = x$input$lower,
                     lpar = rep(-Inf, 2))

  # the updated design has the requested number of analyses
  (nrow(xu$analysis) %==% 2L)
  # the efficacy bounds match gs_power_ahr
  (all.equal(xp$bound$z, xu$bound$z, tolerance = 1e-7, scale = 1))
})


# --------------------- Example 3 ----------------------------------------- #
# A three-analysis spending efficacy bound and fixed futility bound         #
# ------------------------------------------------------------------------- #
x <- gs_design_ahr(
  enroll_rate = define_enroll_rate(duration = c(2, 2, 10), rate = (1:3) / 3),
  fail_rate = define_fail_rate(duration = c(3, Inf), fail_rate = log(2) / 9,
                               hr = c(1, 0.6), dropout_rate = .0001),
  alpha = 0.025, beta = 0.1, ratio = 1, info_scale = "h0_info",
  info_frac = NULL, analysis_time = c(20, 28, 36),
  upper = gs_spending_bound, upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  test_upper = TRUE,
  lower = gs_b, lpar = c(-2, -Inf, -Inf),
  test_lower = c(TRUE, FALSE, FALSE)
) |> to_integer()

assert("the fixed futility bound does not require the input of lstime", {
  (has_error(gs_update_ahr(x = x, 
                           ustime = c(0.3, 0.5, 0.8, 1), 
                           lstime = c(0.3, 0.5, 0.8, 1), 
                           event_tbl = data.frame(analysis = c(1, 1, 2, 2, 3, 3, 4, 4),
                                                  event = c(20, 80, 30, 150, 40, 250, 50, 300)),
                           test_upper = TRUE, 
                           test_lower = c(TRUE, FALSE, FALSE, FALSE)
  )))
})

assert("the fixed futility bound requires the input of test_lower", {
  (has_error(gs_update_ahr(x = x, 
                           ustime = c(0.3, 0.5, 0.8, 1), 
                           lstime = c(0.3, 0.5, 0.8, 1), 
                           event_tbl = data.frame(analysis = c(1, 1, 2, 2, 3, 3, 4, 4),
                                                  event = c(20, 80, 30, 150, 40, 250, 50, 300)),
                           test_upper = TRUE, 
                           test_lower = NULL
  )))
})

assert("Increase the number of analyses from 3 to 4", {
  event_tbl <- data.frame(analysis = c(1, 1, 2, 2, 3, 3, 4, 4),
                        event = c(20, 80, 30, 150, 40, 250, 50, 300))
  obs <- as.numeric(tapply(event_tbl$event, event_tbl$analysis, sum))
  ustime <- obs / max(obs)
  
  xu <- gs_update_ahr(x = x, ustime = ustime, lstime = NULL, event_tbl = event_tbl,
                     test_upper = TRUE, test_lower = c(TRUE, FALSE, FALSE, FALSE))

  xp <- gs_power_ahr(enroll_rate = x$enroll_rate, fail_rate = x$fail_rate, 
                     ratio = x$input$ratio, 
                     info_scale = x$input$info_scale,
                     event = xu$analysis$event,
                     upper = gs_spending_bound,
                     upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025,
                                 timing = ustime),
                     lower = x$input$lower,
                     lpar = rep(-Inf, 4))
  # the updated design has the requested number of analyses
  (nrow(xu$analysis) %==% 4L)
  # the updated efficacy bounds match gs_power_ahr
  (all.equal(xp$bound$z[which(xp$bound$bound == "upper")],
             xu$bound$z[which(xu$bound$bound == "upper")],
             tolerance = 1e-7, scale = 1))
  # the updated futility bounds remain unchanged
  (xu$bound$z[which(xu$bound$bound == "lower")] %==% -2)
})

assert("Decrease the number of analyses from 3 to 2", {
  event_tbl <- data.frame(analysis = c(1, 1, 2, 2),
                        event = c(30, 100, 50, 300))
  obs <- as.numeric(tapply(event_tbl$event, event_tbl$analysis, sum))
  ustime <- obs / max(obs)
  
  xu <- gs_update_ahr(x = x, ustime = ustime, lstime = NULL, event_tbl = event_tbl,
                      test_upper = TRUE, test_lower = c(TRUE, FALSE))

  xp <- gs_power_ahr(enroll_rate = x$enroll_rate, fail_rate = x$fail_rate, 
                     ratio = x$input$ratio, 
                     info_scale = x$input$info_scale,
                     event = xu$analysis$event,
                     upper = gs_spending_bound,
                     upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025,
                                 timing = ustime),
                     lower = x$input$lower,
                     lpar = rep(-Inf, 2))
  # the updated design has the requested number of analyses
  (nrow(xu$analysis) %==% 2L)
  # the updated efficacy bounds match gs_power_ahr
  (all.equal(xp$bound$z[which(xp$bound$bound == "upper")],
             xu$bound$z[which(xu$bound$bound == "upper")],
             tolerance = 1e-7, scale = 1))
  # the updated futility bounds remain unchanged
  (xu$bound$z[which(xu$bound$bound == "lower")] %==% -2)
})

# --------------------- Example 4 ----------------------------------------- #
# A three-analysis spending efficacy bound and fixed futility bound         #
#             At time of IA1, we update the design as below                 #
# ------------------------------------------------------------------------- #
x <- gs_design_ahr(
  enroll_rate = define_enroll_rate(duration = c(2, 2, 10), rate = (1:3) / 3),
  fail_rate = define_fail_rate(duration = c(3, Inf), fail_rate = log(2) / 9,
                               hr = c(1, 0.6), dropout_rate = .0001),
  alpha = 0.025, beta = 0.1, ratio = 1, info_scale = "h0_info",
  info_frac = NULL, analysis_time = c(20, 28, 36),
  upper = gs_spending_bound, upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  test_upper = TRUE,
  lower = gs_b, lpar = c(-2, -Inf, -Inf),
  test_lower = c(TRUE, FALSE, FALSE)
) |> to_integer()

event_tbl <- data.frame(analysis = c(1, 1),
                        event = c(60, 150))
# event overrun and take minimal alpha spending
ustime <- c(min(210, x$analysis$event[1]), x$analysis$event[2:3]) / x$analysis$event[3]

xu <- gs_update_ahr(x = x, ustime = ustime, lstime = NULL, event_tbl = event_tbl,
                    test_upper = NULL, test_lower = NULL)

assert("The futility bounds remain unchanged", {
  (xu$bound$z[which(xu$bound$bound == "lower")] %==% -2)
})

assert("The efficacy bound at IA1 remain unchanged", {
  (xu$bound$z[which(xu$bound$bound == "upper" & xu$bound$analysis == 1)] %==% x$bound$z[which(x$bound$bound == "upper" & x$bound$analysis == 1)])
})

# --------------------- Example 5 ----------------------------------------- #
# A three-analysis spending efficacy bound and beta-spending futility bound #
#             At time of IA1, we update the design as below                 #
# ------------------------------------------------------------------------- #
x <- gs_design_ahr(
  enroll_rate = define_enroll_rate(duration = c(2, 2, 10), rate = (1:3) / 3),
  fail_rate = define_fail_rate(duration = c(3, Inf), fail_rate = log(2) / 9,
                               hr = c(1, 0.6), dropout_rate = .0001),
  alpha = 0.025, beta = 0.1, ratio = 1, info_scale = "h0_info",
  info_frac = NULL, analysis_time = c(20, 28, 36),
  upper = gs_spending_bound, upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  test_upper = TRUE,
  lower = gs_spending_bound, lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -4),
  test_lower = c(TRUE, FALSE, FALSE)
) |> to_integer()

event_tbl <- data.frame(analysis = c(1, 1),
                        event = c(60, 150))
# event overrun and take minimal alpha spending
ustime <- c(min(210, x$analysis$event[1]), x$analysis$event[2:3]) / x$analysis$event[3]

xu <- gs_update_ahr(x = x, ustime = ustime, lstime = NULL, event_tbl = event_tbl,
                    test_upper = NULL, test_lower = NULL)

assert("The efficacy bound matches with gs_power_ahr", {
  xp <- gs_power_ahr(enroll_rate = x$enroll_rate, fail_rate = x$fail_rate, 
                     ratio = x$input$ratio, 
                     info_scale = x$input$info_scale,
                     event = xu$analysis$event,
                     upper = gs_spending_bound,
                     upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025,
                                 timing = ustime),
                     lower = gs_b,
                     lpar = rep(-Inf, 3))
                     
  # the efficacy bounds match gs_power_ahr                   
  (all.equal(xp$bound$z[which(xp$bound$bound == "upper")],
             xu$bound$z[which(xu$bound$bound == "upper")],
             tolerance = 1e-7, scale = 1))
})

assert("issue #677 leaves sample size unreported at all updated analyses", {
  original_design <- gs_design_ahr(
    enroll_rate = define_enroll_rate(
      duration = diff(c(0, 2, 4, 6, 24)), rate = c(1, 2, 3, 4), stratum = "All"
    ),
    fail_rate = define_fail_rate(
      duration = diff(c(0, 3, 103)), fail_rate = log(2) / c(6, 6),
      dropout_rate = c(0.001, 0.001), hr = c(1, 0.7), stratum = "All"
    ),
    alpha = 0.025, beta = 0.1, info_frac = c(0.4, 0.8, 1),
    analysis_time = 30, ratio = 1, binding = FALSE,
    upper = "gs_spending_bound",
    upar = list(sf = "sfLDOF", total_spend = 0.025, param = NULL),
    lower = "gs_spending_bound",
    lpar = list(sf = "sfLDOF", total_spend = 0.1, param = NULL),
    h1_spending = TRUE, info_scale = "h0_info"
  ) |> to_integer()

  observed_events <- data.frame(
    analysis = rep(1:4, each = 2),
    event = c(213, 192, 250, 400, 300, 500, 366, 644)
  )
  spending_time <- with(observed_events, tapply(event, analysis, sum)) /
    max(original_design$analysis$event)
  spending_time[length(spending_time)] <- 1

  updated_design <- gs_update_ahr(
    x = original_design, ustime = spending_time, lstime = spending_time,
    event_tbl = observed_events, test_upper = rep(TRUE, 4),
    test_lower = rep(TRUE, 4), test_harm = FALSE
  )

  (!"n" %in% names(updated_design$analysis))
  (updated_design$analysis$analysis %==% 1:4)
  (updated_design$analysis$event %==% c(405, 650, 800, 1010))
  ("n" %in% names(original_design$analysis))
})