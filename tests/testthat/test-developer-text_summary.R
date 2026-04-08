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
