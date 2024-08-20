test_that("ahr_blinded throws an error for non-numeric or negative hr", {
  expect_error(
    ahr_blinded(hr = c(1, -2, 3)),
    "'hr' must be a vector of positive numbers."
  )
  expect_error(
    ahr_blinded(hr = "abc"),
    "'hr' must be a vector of positive numbers."
  )
})

test_that("ahr_blinded throws an error when intervals and hr are not aligned", {
  expect_error(
    ahr_blinded(intervals = c(3, 6), hr = c(1)),
    "The piecewise model specified 'hr' and 'intervals' differ in length."
  )
})

test_that("ahr_blinded handles piecewise exponential model fitting and calculations correctly", {
  surv <- Surv(
    time = simtrial::ex1_delayed_effect$month,
    event = simtrial::ex1_delayed_effect$evntd
  )
  intervals <- c(3, 6, Inf)
  hr <- c(1, 0.7, 0.5)
  ratio <- 2

  # Run the function
  result <- ahr_blinded(surv = surv, intervals = intervals, hr = hr, ratio = ratio)

  # Test 1: Correct fitting of survival data into piecewise exponential model
  event <- simtrial::fit_pwexp(surv, intervals)[, 3]
  expect_length(event, length(intervals))
  expect_true(all(event >= 0))

  # Test 2: Hazard ratio vector is correctly extended
  nhr <- length(hr)
  nx <- length(event)
  if (length(hr) < length(event)) {
    hr <- c(hr, rep(hr[nhr], nx - nhr))
  }
  expect_equal(length(hr), length(event))
  expect_equal(hr, c(1, 0.7, 0.5)) # Expected extended hr vector

  # Test 3: Blinded AHR (theta) is computed correctly
  theta <- -sum(log(hr[1:nx]) * event) / sum(event)
  expect_true(!is.na(theta))

  # Test 4: Information adjustment (q_e) is computed correctly
  q_e <- ratio / (1 + ratio)
  expect_equal(q_e, 2 / 3)

  # Check the overall result
  expect_true(inherits(result, "tbl_df"))
  expect_equal(result$event, sum(event))
  expect_equal(result$theta, theta)
  expect_equal(result$ahr, exp(-theta))
})

test_that("Correct computation of blinded AHR and information adjustment", {
  surv <- survival::Surv(simtrial::ex2_delayed_effect$month, event = simtrial::ex2_delayed_effect$evntd)
  intervals <- c(3, Inf)
  hr <- c(1, 0.6)
  ratio <- 1
  event <- simtrial::fit_pwexp(surv, intervals)[, 3]

  expected_event <- sum(surv[, "status"])
  expected_theta <- -sum(log(hr[1:length(event)]) * event) / sum(event)
  expected_ahr <- exp(-(-sum(log(hr[1:length(event)]) * event) / sum(event)))
  expected_info0 <- sum(surv[, "status"]) * (1 - ratio / (1 + ratio)) * (ratio / (1 + ratio))

  result <- ahr_blinded(surv = surv, intervals = intervals, hr = hr, ratio = ratio)

  expect_equal(result$event, expected_event)
  expect_equal(result$ahr, expected_ahr, tolerance = 0.001)
  expect_equal(result$theta, expected_theta, tolerance = 0.001)
  expect_equal(result$info0, expected_info0)
})

test_that("ahr_blinded returns a tibble with correct structure", {
  result <- ahr_blinded()
  expect_true(tibble::is_tibble(result))
  expect_named(result, c("event", "ahr", "theta", "info0"))
  expect_true(nrow(result) == 1)
})
