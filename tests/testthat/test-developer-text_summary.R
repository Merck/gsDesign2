test_that("Text summary of a 1-sided design", {
  x <- gs_design_ahr(info_frac = 1:3 / 3, test_lower = FALSE) |> to_integer()

  expect_identical(
    text_summary(x),
    "One-sided group sequential design with 3 analyses, time-to-event outcome with sample size 490 and 301 events, 90 percent power, 2.5 percent (1-sided) Type I error to detect hazard ratio of 0.9 during the first 3 months and 0.6 thereafter. Enrollment and total study durations are assumed to be 14 and 36.1 months, respectively. Efficacy bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters)."
  )
})

test_that("Text summary of a 2-sided symmetric design", {
  x <- gs_design_ahr(
    info_frac = 1:3 / 3,
    upper = gs_spending_bound,
    lower = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    binding = TRUE,
    h1_spending = FALSE
  ) |> to_integer()

  expect_identical(
    text_summary(x),
    "Symmetric two-sided group sequential design with 3 analyses, time-to-event outcome with sample size 490 and 301 events, 90 percent power, 2.5 percent (1-sided) Type I error to detect hazard ratio of 0.9 during the first 3 months and 0.6 thereafter. Enrollment and total study durations are assumed to be 14 and 36.1 months, respectively. Bounds derived using a  Lan-DeMets O'Brien-Fleming approximation spending function (no parameters)."
  )
})

test_that("Text summary of a asymmetric 2-sided design with beta-spending and non-binding futility bound", {
  x <- gs_design_ahr(
    info_frac = 1:3 / 3,
    alpha = 0.025,
    beta = 0.1,
    upper = gs_spending_bound,
    lower = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -4),
    binding = FALSE,
    h1_spending = TRUE
  ) |> to_integer()

  expect_identical(
    text_summary(x),
    "Asymmetric two-sided group sequential design with non-binding futility bound, 3 analyses, time-to-event outcome with sample size 500 and 306 events, 90 percent power, 2.5 percent (1-sided) Type I error to detect hazard ratio of 0.9 during the first 3 months and 0.6 thereafter. Enrollment and total study durations are assumed to be 14 and 35.9 months, respectively. Efficacy bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). Futility bounds derived using a Hwang-Shih-DeCani spending function with gamma = -4."
  )
})

test_that("Text summary of a asymmetric 2-sided design with fixed non-binding futility bound", {
  x <- gs_design_ahr(
    info_frac = 1:3 / 3,
    alpha = 0.025,
    beta = 0.1,
    upper = gs_spending_bound,
    lower = gs_b,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    test_upper = c(FALSE, TRUE, TRUE),
    lpar = c(-1, -Inf, -Inf),
    test_lower = c(TRUE, FALSE, FALSE),
    binding = FALSE,
    h1_spending = TRUE
  ) |> to_integer()

  expect_identical(
    text_summary(x),
    "Asymmetric two-sided group sequential design with non-binding futility bound, 3 analyses, time-to-event outcome with sample size 506 and 311 events, 90 percent power, 2.5 percent (1-sided) Type I error to detect hazard ratio of 0.9 during the first 3 months and 0.6 thereafter. Enrollment and total study durations are assumed to be 14 and 36.1 months, respectively. Efficacy bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters), tested at tested at IA2, FA. Futility bounds is fixed as -1, -Inf, -Inf, tested at tested at IA1."
  )
})

test_that("If there are >5 pieces of HRs, we provide a brief summary of HR.", {
  x <- gs_design_ahr(
    fail_rate = define_fail_rate(
      duration = c(rep(3, 5), Inf),
      hr = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4),
      fail_rate = log(2) / 10,
      dropout_rate = 0.001
    ),
    info_frac = 1:3 / 3,
    test_lower = FALSE
  )

  expect_identical(
    text_summary(x),
    "One-sided group sequential design with 3 analyses, time-to-event outcome with sample size 290.5 and 218 events, 90 percent power, 2.5 percent (1-sided) Type I error to detect piecewise hazard ratio. Enrollment and total study durations are assumed to be 14 and 36 months, respectively. Efficacy bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters)."
  )
})

test_that("Text summary of a fixed design created with gs_design_ahr()", {
  x <- gs_design_ahr(
    upper = gs_b,
    lower = gs_b,
    upar = qnorm(1 - 0.025),
    lpar = -Inf
  )

  expect_identical(
    text_summary(x),
    "One-sided group sequential design with 1 analyses, time-to-event outcome with sample size 476 and 291.9 events, 90 percent power, 2.5 percent (1-sided) Type I error to detect hazard ratio of 0.9 during the first 3 months and 0.6 thereafter. Enrollment and total study durations are assumed to be 14 and 36 months, respectively."
  )
})

test_that("Text summary for two strata with proportional hazards", {
  x <- gs_design_ahr(
    enroll_rate = define_enroll_rate(
      stratum = c("A", "B"),
      duration = c(12, 12),
      rate = c(1, 1)
    ),
    fail_rate = define_fail_rate(
      stratum = c("A", "B"),
      duration = c(Inf, Inf),
      fail_rate = log(2) / c(9, 18),
      hr = c(0.8, 0.6),
      dropout_rate = 0.001
    ),
    alpha = 0.025,
    beta = 0.1,
    info_frac = NULL,
    analysis_time = c(24, 36),
    ratio = 1,
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1),
    h1_spending = TRUE,
    test_upper = TRUE,
    test_lower = TRUE,
    info_scale = "h0_h1_info"
  )

  expect_identical(
    text_summary(x),
    "Asymmetric two-sided group sequential design with non-binding futility bound, 2 analyses, time-to-event outcome with sample size 538.3 and 388.2 events, 90 percent power, 2.5 percent (1-sided) Type I error to detect a hazard ratio of 0.8 in stratum A and a hazard ratio of 0.6 in stratum B. Enrollment and total study durations are assumed to be 12 and 36 months, respectively. Efficacy bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). Futility bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters)."
  )

  y <- gs_power_ahr(
    enroll_rate = x$enroll_rate,
    fail_rate = x$fail_rate,
    event = NULL,
    analysis_time = x$analysis$time,
    ratio = 1,
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1),
    h1_spending = TRUE,
    test_upper = TRUE,
    test_lower = TRUE,
    info_scale = "h0_h1_info"
  )

  expect_identical(
    text_summary(y),
    "Asymmetric two-sided group sequential design with non-binding futility bound, 2 analyses, time-to-event outcome with sample size 538.3 and 388.2 events, 2.5 percent (1-sided) Type I error. Enrollment and total study durations are assumed to be 12 and 36 months, respectively. Efficacy bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). Futility bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). With a hazard ratio of 0.8 in stratum A and a hazard ratio of 0.6 in stratum B, the power is 90 percent."
  )
})

test_that("Text summary for two strata with two-piece non-proportional hazards", {
  x <- gs_design_ahr(
    enroll_rate = define_enroll_rate(
      stratum = c("A", "B"),
      duration = c(12, 12),
      rate = c(1, 1)
    ),
    fail_rate = define_fail_rate(
      stratum = c("A", "A", "B", "B"),
      duration = c(3, Inf, 3, Inf),
      fail_rate = log(2) / c(9, 9, 18, 18),
      hr = c(1, 0.8, 1, 0.6),
      dropout_rate = 0.001
    ),
    alpha = 0.025,
    beta = 0.1,
    info_frac = NULL,
    analysis_time = c(24, 36),
    ratio = 1,
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1),
    h1_spending = TRUE,
    test_upper = TRUE,
    test_lower = TRUE,
    info_scale = "h0_h1_info"
  )

  expect_identical(
    text_summary(x),
    "Asymmetric two-sided group sequential design with non-binding futility bound, 2 analyses, time-to-event outcome with sample size 848.8 and 618.5 events, 90 percent power, 2.5 percent (1-sided) Type I error to detect hazard ratio of 1 during the first 3 months and 0.8 thereafter in stratum A and hazard ratio of 1 during the first 3 months and 0.6 thereafter in stratum B. Enrollment and total study durations are assumed to be 12 and 36 months, respectively. Efficacy bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). Futility bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters)."
  )

  y <- gs_power_ahr(
    enroll_rate = x$enroll_rate,
    fail_rate = x$fail_rate,
    event = NULL,
    analysis_time = x$analysis$time,
    ratio = 1,
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1),
    h1_spending = TRUE,
    test_upper = TRUE,
    test_lower = TRUE,
    info_scale = "h0_h1_info"
  )

  expect_identical(
    text_summary(y),
    "Asymmetric two-sided group sequential design with non-binding futility bound, 2 analyses, time-to-event outcome with sample size 848.8 and 618.5 events, 2.5 percent (1-sided) Type I error. Enrollment and total study durations are assumed to be 12 and 36 months, respectively. Efficacy bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). Futility bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). With hazard ratio of 1 during the first 3 months and 0.8 thereafter in stratum A and hazard ratio of 1 during the first 3 months and 0.6 thereafter in stratum B, the power is 90 percent."
  )
})

test_that("Text summary for two strata with 6 pieces non-proportional hazards", {
  x <- gs_design_ahr(
    enroll_rate = define_enroll_rate(
      stratum = c("A", "B"),
      duration = c(12, 12),
      rate = c(1, 1)
    ),
    fail_rate = define_fail_rate(
      stratum = rep(c("A", "B"), each = 6),
      duration = rep(c(rep(3, 5), Inf), 2),
      hr = rep(c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4), 2),
      fail_rate = log(2) / 10,
      dropout_rate = 0.001
    ),
    alpha = 0.025,
    beta = 0.1,
    info_frac = NULL,
    analysis_time = c(24, 36),
    ratio = 1,
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1),
    h1_spending = TRUE,
    test_upper = TRUE,
    test_lower = TRUE,
    info_scale = "h0_h1_info"
  )

  expect_identical(
    text_summary(x),
    "Asymmetric two-sided group sequential design with non-binding futility bound, 2 analyses, time-to-event outcome with sample size 283.6 and 217.8 events, 90 percent power, 2.5 percent (1-sided) Type I error to detect piecewise hazard ratio in stratum A and piecewise hazard ratio in stratum B. Enrollment and total study durations are assumed to be 12 and 36 months, respectively. Efficacy bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). Futility bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters)."
  )

  y <- gs_power_ahr(
    enroll_rate = x$enroll_rate,
    fail_rate = x$fail_rate,
    event = NULL,
    analysis_time = x$analysis$time,
    ratio = 1,
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1),
    h1_spending = TRUE,
    test_upper = TRUE,
    test_lower = TRUE,
    info_scale = "h0_h1_info"
  )

  expect_identical(
    text_summary(y),
    "Asymmetric two-sided group sequential design with non-binding futility bound, 2 analyses, time-to-event outcome with sample size 283.6 and 217.8 events, 2.5 percent (1-sided) Type I error. Enrollment and total study durations are assumed to be 12 and 36 months, respectively. Efficacy bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). Futility bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). With piecewise hazard ratio in stratum A and piecewise hazard ratio in stratum B, the power is 90 percent."
  )
})

test_that("Text summary for >2 strata with >=3 pieces non-proportional hazards", {
  x <- gs_design_ahr(
    enroll_rate = define_enroll_rate(
      stratum = c("A", "B", "C"),
      duration = c(12, 12, 12),
      rate = c(1, 1, 1)
    ),
    fail_rate = define_fail_rate(
      stratum = rep(c("A", "B", "C"), each = 6),
      duration = rep(c(rep(3, 5), Inf), 3),
      hr = rep(c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4), 3),
      fail_rate = log(2) / 10,
      dropout_rate = 0.001
    ),
    alpha = 0.025,
    beta = 0.1,
    info_frac = NULL,
    analysis_time = c(24, 36),
    ratio = 1,
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1),
    h1_spending = TRUE,
    test_upper = TRUE,
    test_lower = TRUE,
    info_scale = "h0_h1_info"
  )

  expect_identical(
    text_summary(x),
    "Asymmetric two-sided group sequential design with non-binding futility bound, 2 analyses, time-to-event outcome with sample size 283.6 and 217.8 events, 90 percent power, 2.5 percent (1-sided) Type I error to detect piecewise hazard ratio in 3 strata. Enrollment and total study durations are assumed to be 12 and 36 months, respectively. Efficacy bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). Futility bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters)."
  )

  y <- gs_power_ahr(
    enroll_rate = x$enroll_rate,
    fail_rate = x$fail_rate,
    event = NULL,
    analysis_time = x$analysis$time,
    ratio = 1,
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1),
    h1_spending = TRUE,
    test_upper = TRUE,
    test_lower = TRUE,
    info_scale = "h0_h1_info"
  )

  expect_identical(
    text_summary(y),
    "Asymmetric two-sided group sequential design with non-binding futility bound, 2 analyses, time-to-event outcome with sample size 283.6 and 217.8 events, 2.5 percent (1-sided) Type I error. Enrollment and total study durations are assumed to be 12 and 36 months, respectively. Efficacy bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). Futility bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). With piecewise hazard ratio in 3 strata, the power is 90 percent."
  )
})

test_that("Text summary for >2 strata with <3 pieces non-proportional hazards", {
  x <- gs_design_ahr(
    enroll_rate = define_enroll_rate(
      stratum = c("A", "B", "C"),
      duration = c(12, 12, 12),
      rate = c(1, 1, 1)
    ),
    fail_rate = define_fail_rate(
      stratum = rep(c("A", "B", "C"), each = 2),
      duration = rep(c(3, Inf), 3),
      hr = rep(c(0.9, 0.8), 3),
      fail_rate = log(2) / 10,
      dropout_rate = 0.001
    ),
    alpha = 0.025,
    beta = 0.1,
    info_frac = NULL,
    analysis_time = c(24, 36),
    ratio = 1,
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1),
    h1_spending = TRUE,
    test_upper = TRUE,
    test_lower = TRUE,
    info_scale = "h0_h1_info"
  )

  expect_identical(
    text_summary(x),
    "Asymmetric two-sided group sequential design with non-binding futility bound, 2 analyses, time-to-event outcome with sample size 1402.7 and 1167.7 events, 90 percent power, 2.5 percent (1-sided) Type I error to detect hazard ratio of 0.9 during the first 3 months and 0.8 thereafter in stratum A and hazard ratio of 0.9 during the first 3 months and 0.8 thereafter in stratum B and hazard ratio of 0.9 during the first 3 months and 0.8 thereafter in stratum C. Enrollment and total study durations are assumed to be 12 and 36 months, respectively. Efficacy bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). Futility bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters)."
  )

  y <- gs_power_ahr(
    enroll_rate = x$enroll_rate,
    fail_rate = x$fail_rate,
    event = NULL,
    analysis_time = x$analysis$time,
    ratio = 1,
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1),
    h1_spending = TRUE,
    test_upper = TRUE,
    test_lower = TRUE,
    info_scale = "h0_h1_info"
  )

  expect_identical(
    text_summary(y),
    "Asymmetric two-sided group sequential design with non-binding futility bound, 2 analyses, time-to-event outcome with sample size 1402.7 and 1167.7 events, 2.5 percent (1-sided) Type I error. Enrollment and total study durations are assumed to be 12 and 36 months, respectively. Efficacy bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). Futility bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). With hazard ratio of 0.9 during the first 3 months and 0.8 thereafter in stratum A and hazard ratio of 0.9 during the first 3 months and 0.8 thereafter in stratum B and hazard ratio of 0.9 during the first 3 months and 0.8 thereafter in stratum C, the power is 90 percent."
  )
})

test_that("Text summary for two strata with piecewise enrollment rates", {
  # The two strata have very different enrollment lengths. Meant to test code
  # logic, not be a realistic example
  #
  # stratum A = 1 + 2 + 3 =  6
  # stratum B = 4 + 5 + 6 = 15
  enroll_rate <- define_enroll_rate(
    stratum = rep(c("A", "B"), each = 3),
    duration = 1:6,
    rate = rep(c(0.5, 0.75, 1), 2)
  )

  x <- gs_design_ahr(
    enroll_rate = enroll_rate,
    fail_rate = define_fail_rate(
      stratum = c("A", "A", "B", "B"),
      duration = c(3, Inf, 3, Inf),
      fail_rate = log(2) / c(9, 9, 18, 18),
      hr = c(1, 0.8, 1, 0.6),
      dropout_rate = 0.001
    ),
    alpha = 0.025,
    beta = 0.1,
    info_frac = NULL,
    analysis_time = c(24, 36),
    ratio = 1,
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1),
    h1_spending = TRUE,
    test_upper = TRUE,
    test_lower = TRUE,
    info_scale = "h0_h1_info"
  )

  expect_identical(
    text_summary(x),
    "Asymmetric two-sided group sequential design with non-binding futility bound, 2 analyses, time-to-event outcome with sample size 682.4 and 450.1 events, 90 percent power, 2.5 percent (1-sided) Type I error to detect hazard ratio of 1 during the first 3 months and 0.8 thereafter in stratum A and hazard ratio of 1 during the first 3 months and 0.6 thereafter in stratum B. Enrollment and total study durations are assumed to be 15 and 36 months, respectively. Efficacy bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). Futility bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters)."
  )

  y <- gs_power_ahr(
    enroll_rate = x$enroll_rate,
    fail_rate = x$fail_rate,
    event = NULL,
    analysis_time = x$analysis$time,
    ratio = 1,
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1),
    h1_spending = TRUE,
    test_upper = TRUE,
    test_lower = TRUE,
    info_scale = "h0_h1_info"
  )

  expect_identical(
    text_summary(y),
    "Asymmetric two-sided group sequential design with non-binding futility bound, 2 analyses, time-to-event outcome with sample size 682.4 and 450.1 events, 2.5 percent (1-sided) Type I error. Enrollment and total study durations are assumed to be 15 and 36 months, respectively. Efficacy bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). Futility bounds derived using a Lan-DeMets O'Brien-Fleming approximation spending function (no parameters). With hazard ratio of 1 during the first 3 months and 0.8 thereafter in stratum A and hazard ratio of 1 during the first 3 months and 0.6 thereafter in stratum B, the power is 90 percent."
  )
})
