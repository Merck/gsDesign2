# Test 1: compare with gsDesign under proportional hazard
test_that("under same number of events, compare the power", {
  res <- test_gs_power_ahr()
  x <- res$x
  y <- res$y

  out <- gs_power_ahr(
    enroll_rate = define_enroll_rate(
      duration = c(2, 2, 2, 6),
      rate = c(6, 12, 18, 24)
    ),
    fail_rate = define_fail_rate(
      duration = 1,
      fail_rate = log(2) / 9,
      hr = 0.65,
      dropout_rate = 0.001
    ),
    ratio = 1,
    # Set number of events the same as the design x above from gsDesign()
    event = x$n.I,
    analysis_time = NULL,
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL, theta = 0),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2, param = NULL, timing = NULL, theta = 0),
    test_upper = TRUE,
    test_lower = FALSE
  )

  expect_equal(out$bound$probability[1:2], y$Efficacy[c(5, 10)], tolerance = 0.02)
})

test_that("under same power setting, compare the number of events", {
  res <- test_gs_power_ahr()
  x <- res$x

  out <- gs_power_ahr(
    enroll_rate = define_enroll_rate(
      duration = c(2, 2, 2, 6),
      rate = c(6, 12, 18, 24)
    ),
    fail_rate = define_fail_rate(
      duration = 1,
      fail_rate = log(2) / 9,
      dropout_rate = 0.001,
      hr = 0.65
    ),
    ratio = 1,
    event = NULL,
    # Adjust the times s.t. power ~= 0.801 and information fraction ~= 0.7
    # (same as the design x above from gsDesign())
    analysis_time = c(21, 34.9),
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL, theta = 0),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2, param = NULL, timing = NULL, theta = 0),
    test_upper = TRUE,
    test_lower = FALSE
  )

  # In case test fails, check whether caused by small tolerance
  expect_equal(out$analysis$event[1:2], x$n.I, tolerance = 0.02)
})
