test_that("given sample size, the output power arrives at the target", {
  # set enrollment rates
  enroll_rate <- define_enroll_rate(duration = 12, rate = 500 / 12)

  # set failure rates
  fail_rate <- define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 15, # median survival 15 month
    dropout_rate = 0.001,
    hr = c(1, .6)
  )

  # output from gsDesign2
  x1 <- gsDesign2:::fixed_design_size_rmst(enroll_rate, fail_rate,
    ratio = 1, analysis_time = 36,
    alpha = 0.025, test = "rmst_difference"
  )

  # output from npsurvSS
  gs_arm <- gs_create_arm(enroll_rate, fail_rate, ratio = 1, total_time = 36)
  arm0 <- gs_arm[["arm0"]]
  arm1 <- gs_arm[["arm1"]]
  arm0$size <- x1$analysis$n / 2
  arm1$size <- x1$analysis$n / 2
  x2 <- npsurvSS::power_two_arm(arm0, arm1,
    test = list(test = "rmst difference", milestone = arm0$total_time),
    alpha = 0.025
  )

  expect_equal(x1$bound$probability, x2)
})


test_that("given power, the output sample size arrives at the target power", {
  # set enrollment rates
  enroll_rate <- tibble::tibble(stratum = "All", duration = 12, rate = 500 / 12)

  # set failure rates
  fail_rate <- tibble::tibble(
    stratum = "All",
    duration = c(4, 100),
    fail_rate = log(2) / 15, # median survival 15 month
    hr = c(1, .6),
    dropout_rate = 0.001
  )

  # output from gsDesign2
  x1 <- fixed_design_power_rmst(enroll_rate, fail_rate,
    analysis_time = 36,
    test = "rmst_difference"
  )

  # output from npsurvSS
  gs_arm <- gs_create_arm(enroll_rate, fail_rate, ratio = 1, total_time = 36)
  arm0 <- gs_arm[["arm0"]]
  arm1 <- gs_arm[["arm1"]]

  x2 <- npsurvSS::size_two_arm(arm0, arm1,
    alpha = 0.025,
    power = x1$bound$probability,
    test = list(test = "rmst difference", milestone = arm0$total_time)
  )

  expect_equal(x1$analysis$n, x2[["n"]])
})
