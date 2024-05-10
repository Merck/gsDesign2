test_that("calculate analysis number as planned", {
  enroll_rate <- define_enroll_rate(duration = 12, rate = 500 / 12)

  fail_rate <- define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 15, # Median survival 15 month
    dropout_rate = 0.001,
    hr = c(1, .6)
  )

  fh_test <- rbind(
    data.frame(
      rho = 0,
      gamma = 0,
      tau = -1,
      test = 1,
      analysis = 1:3,
      analysis_time = c(12, 24, 36)
    ),
    data.frame(
      rho = c(0, 0.5),
      gamma = 0.5,
      tau = -1,
      test = 2:3,
      analysis = 3,
      analysis_time = 36
    )
  )

  # User-defined bound
  gs_power_combo_test1 <- gsDesign2::gs_power_combo(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    fh_test = fh_test,
    upper = gs_b, upar = c(3, 2, 1),
    lower = gs_b, lpar = c(-1, 0, 1)
  )

  # Minimal Information Fraction derived bound
  gs_power_combo_test2 <- gsDesign2::gs_power_combo(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    fh_test,
    upper = gs_spending_combo,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_combo,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2)
  )

  res <- list(
    "enroll_rate" = enroll_rate,
    "fail_rate" = fail_rate,
    "fh_test" = fh_test,
    "gs_power_combo_test1" = gs_power_combo_test1,
    "gs_power_combo_test2" = gs_power_combo_test2
  )

  # calculate analysis number as planned
  fh_test <- res$fh_test
  gs_power_combo_test1 <- res$gs_power_combo_test1
  expect_equal(max(fh_test$analysis), max(gs_power_combo_test1$analysis$analysis))

  # calculate analysisTimes as planned
  fh_test <- res$fh_test
  gs_power_combo_test1 <- res$gs_power_combo_test1

  expect_equal(unique(fh_test$analysis_time), unique(gs_power_combo_test1$analysis$time))

  # calculate N and each analysis Events N as planned
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  fh_test <- res$fh_test
  gs_power_combo_test1 <- res$gs_power_combo_test1

  for (i in 1:max(fh_test$analysis)) {
    event <- test_event(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      td = unique(fh_test$analysis_time)[i]
    )
    expect_equal(event, unique(gs_power_combo_test1$analysis$event)[i], tolerance = 0.01)
  }

  # calculate analysis number as planned
  fh_test <- res$fh_test
  gs_power_combo_test2 <- res$gs_power_combo_test2

  expect_equal(max(fh_test$analysis), max(gs_power_combo_test2$analysis$analysis))

  # calculate analysisTimes as planned
  fh_test <- res$fh_test
  gs_power_combo_test2 <- res$gs_power_combo_test2

  expect_equal(unique(fh_test$analysis_time), unique(gs_power_combo_test2$analysis$time))

  # calculate N and each analysis Events N as planned
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  fh_test <- res$fh_test
  gs_power_combo_test2 <- res$gs_power_combo_test2

  for (i in 1:max(fh_test$analysis)) {
    event <- test_event(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      td = unique(fh_test$analysis_time)[i]
    )
    expect_equal(event, unique(gs_power_combo_test2$analysis$event)[i], tolerance = 0.01)
  }
})

test_that("arguments are passed via ... to mvtnorm::pmvnorm()", {
  x1 <- gs_power_combo(seed = 1)
  x2 <- gs_power_combo(seed = 1)
  x3 <- gs_power_combo(seed = 2)
  expect_identical(x1, x2)
  expect_false(identical(x1, x3))
})
