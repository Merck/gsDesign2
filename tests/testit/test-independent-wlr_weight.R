# weighted log rank test with 3 options of weights

assert("Validate the function based on simple calculation", {
  enroll_rate <- define_enroll_rate(duration = 12, rate = 500 / 12)
  fail_rate <- define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 15, # Median survival 15 months
    dropout_rate = 0.001,
    hr = c(1, .6) # Delay effect after 4 months
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

  # Calculate theoretical results
  # Tarone-Ware weight is the (N at risk)^factor
  wlrn <- (
    npsurvSS::psurv(1:36, arm0, lower.tail = FALSE) *
      npsurvSS::ploss(1:36, arm0, lower.tail = FALSE) *
      npsurvSS::paccr(pmin(arm0$accr_time, 36 - 1:36), arm0) +
      npsurvSS::psurv(1:36, arm1, lower.tail = FALSE) *
      npsurvSS::ploss(1:36, arm1, lower.tail = FALSE) *
      npsurvSS::paccr(pmin(arm1$accr_time, 36 - 1:36), arm1) * 2
  )^0.666

  # Calculate FH weights
  survprob <- 1 - npsurvSS::psurv(1:36, arm0) / 3 - npsurvSS::psurv(1:36, arm1) * 2 / 3
  fhwei <- survprob^0.666 * (1 - survprob)^0.888

  # FH
  pckfhwei <- gsDesign2::wlr_weight_fh(x = 1:36, arm0, arm1, rho = 0.666, gamma = 0.888, tau = NULL)
  # wlr_weight_1
  FH00wt <- gsDesign2::wlr_weight_1(x = 1:36, arm0, arm1)
  # wlr_weight_n()
  pckwlrn <- gsDesign2::wlr_weight_n(x = 1:36, arm0, arm1, power = 0.666)

  (all.equal(as.numeric(pckwlrn), wlrn, tolerance = 0.0001))
  (all.equal(as.numeric(fhwei), pckfhwei, tolerance = 0.0001))
  (as.numeric(FH00wt) %==% 1)
})

assert("test wlr_weight_1", {
  (gsDesign2::wlr_weight_1() %==% 1)
})

assert("test wlr_weight_n", {
  enroll_rate <- define_enroll_rate(
    duration = c(2, 2, 30),
    rate = c(3, 6, 9)
  )
  fail_rate <- define_fail_rate(
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    dropout_rate = rep(.001, 2),
    hr = c(.9, .6)
  )
  total_time <- 36
  analysis_time <- 12

  arm <- gs_create_arm(enroll_rate, fail_rate, ratio = 1, total_time = total_time)
  arm0 <- arm$arm0
  arm1 <- arm$arm1

  prob0 <- gsDesign2:::prob_risk(arm0, analysis_time, total_time)
  prob1 <- gsDesign2:::prob_risk(arm1, analysis_time, total_time)

  expected <- (2 * (0.5 * prob0 + 0.5 * prob1))^2
  res <- gsDesign2::wlr_weight_n(x = analysis_time, arm0 = arm0, arm1 = arm1, power = 2)
  (res %==% expected)
})
