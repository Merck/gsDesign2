# weighted log rank test with 3 options of weights

test_that("Validate the function based on simple calculation", {
  enroll_rate <- tibble::tibble(stratum = "All", duration = 12, rate = 500 / 12)
  fail_rate <- tibble::tibble(
    stratum = "All",
    duration = c(4, 100),
    fail_rate = log(2) / 15, # Median survival 15 months
    hr = c(1, .6), # Delay effect after 4 months
    dropout_rate = 0.001
  )
  # Define study design object in each arm
  gs_arm <- gs_create_arm(
    enroll_rate,
    fail_rate,
    ratio = 2, # Randomization ratio
    total_time = 36 # Total study duration
  )
  arm0 <- gs_arm[["arm0"]]
  arm1 <- gs_arm[["arm1"]]

  # calculate theoretical results
  # Tarone-Ware weight is the (N at risk)^factor

  wlrn <- (npsurvSS::psurv(1:36, arm0, lower.tail = FALSE) *
    npsurvSS::ploss(1:36, arm0, lower.tail = FALSE) *
    npsurvSS::paccr(pmin(arm0$accr_time, 36 - 1:36), arm0) +
    npsurvSS::psurv(1:36, arm1, lower.tail = FALSE) *
      npsurvSS::ploss(1:36, arm1, lower.tail = FALSE) *
      npsurvSS::paccr(pmin(arm1$accr_time, 36 - 1:36), arm1) * 2)^0.666



  # calculate FH weights
  survprob <- 1 - npsurvSS::psurv(1:36, arm0) / 3 - npsurvSS::psurv(1:36, arm1) * 2 / 3
  fhwei <- survprob^0.666 * (1 - survprob)^0.888

  # FH
  pckfhwei <- gsDesign2::wlr_weight_fh(x = 1:36, arm0, arm1, rho = 0.666, gamma = 0.888, tau = NULL)
  # wlr_weight_1
  FH00wt <- gsDesign2::wlr_weight_1(x = 1:36, arm0, arm1)
  # wlr_weight_n()
  pckwlrn <- gsDesign2::wlr_weight_n(x = 1:36, arm0, arm1, power = 0.666)



  expect_equal(object = as.numeric(pckwlrn), expected = wlrn, tolerance = 0.0001)
  expect_equal(object = as.numeric(fhwei), expected = pckfhwei, tolerance = 0.0001)
  expect_equal(object = as.numeric(FH00wt), expected = 1, tolerance = 0)
})
