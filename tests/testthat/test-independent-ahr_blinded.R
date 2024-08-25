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

test_that("ahr_blinded computes theta with constant hazard ratios correctly", {
  surv <- survival::Surv(
    simtrial::ex1_delayed_effect$month,
    simtrial::ex1_delayed_effect$evntd
  )
  intervals <- c(3, 6, Inf)
  hr <- c(1, 1, 1)
  result <- ahr_blinded(surv = surv, intervals = intervals, hr = hr)

  # When all hr = 1, theta should be 0
  expect_equal(result$theta, 0)
})

test_that("ahr_blinded handles zero events", {
  surv <- survival::Surv(time = c(1, 2, 3, 4, 5), event = c(0, 0, 0, 0, 0)) # No events
  intervals <- c(2, 3, Inf)
  hr <- c(0.8, 0.9, 1)

  result <- ahr_blinded(surv = surv, intervals = intervals, hr = hr)

  expect_equal(result$event, 0)
  expect_true(is.nan(result$ahr))
  expect_true(is.nan(result$theta))
  expect_equal(result$info0, 0)
})

test_that("ahr_blinded handles all events in the first interval", {
  surv <- survival::Surv(time = c(1, 2, 2.5, 3, 3.5), event = c(1, 1, 1, 1, 1))
  # Only the first interval contains events
  intervals <- c(4, Inf)
  hr <- c(0.5, 0.7)

  result <- ahr_blinded(surv = surv, intervals = intervals, hr = hr)

  # All events are in the first interval, so result should reflect hr[1]
  expected_theta <- -sum(log(hr[1]) * sum(surv[, "status"])) / sum(surv[, "status"])
  expect_equal(result$theta, expected_theta)
  expect_equal(result$event, sum(surv[, "status"]))
})

test_that("ahr_blinded handles very small hazard ratios", {
  surv <- survival::Surv(time = c(1, 2, 3, 4, 5), event = c(1, 1, 1, 1, 1))
  intervals <- c(2, 3, Inf)
  hr <- c(0.1, 0.2, 0.3)

  res <- ahr_blinded(surv = surv, intervals = intervals, hr = hr)

  expect_true(res$theta > 0)
  expect_true(res$ahr < 1)
})

test_that("ahr_blinded handles very high randomization ratio", {
  surv <- survival::Surv(time = c(1, 2, 3, 4, 5), event = c(1, 1, 1, 1, 1))
  intervals <- c(2, 3, Inf)
  hr <- c(0.8, 0.9, 0.95)
  ratio <- 100

  result <- ahr_blinded(surv = surv, intervals = intervals, hr = hr, ratio = ratio)

  # info0 should be near 0
  expect_equal(result$info0, 0, tolerance = 0.05)
})

test_that("ahr_blinded returns a tibble with correct structure and types", {
  result <- ahr_blinded()

  expect_true(tibble::is_tibble(result))
  expect_named(result, c("event", "ahr", "theta", "info0"))
  expect_true(nrow(result) == 1L)
  expect_type(result$event, "double")
  expect_type(result$ahr, "double")
  expect_type(result$theta, "double")
  expect_type(result$info0, "double")
})
