load("fixtures/simulation_test_data.Rdata")

testthat::test_that("AHR results are consistent with simulation results for single stratum and multiple cutoff", {
  enroll_rate <- tibble::tibble(
    stratum = "All",
    duration = c(2, 2, 10),
    rate = c(3, 6, 9)
  )
  fail_rate <- tibble::tibble(
    stratum = "All",
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    hr = c(.9, .6),
    dropout_rate = rep(.001, 2)
  )
  actual <- ahr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    total_duration = c(12, 24, 36)
  )

  testthat::expect_true(all.equal(simulation_AHR1$AHR, actual$ahr, tolerance = 0.005))
  testthat::expect_true(all.equal(simulation_AHR1$Events, actual$event, tolerance = 0.005))
})

testthat::test_that("AHR results are consistent with simulation results for single stratum and single cutoff", {
  enroll_rate <- tibble::tibble(
    stratum = "All",
    duration = c(2, 2, 10),
    rate = c(3, 6, 9)
  )
  fail_rate <- tibble::tibble(
    stratum = "All",
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    hr = c(.9, .6),
    dropout_rate = rep(.001, 2)
  )
  total_duration <- 30
  actual <- ahr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    total_duration = total_duration
  )
  testthat::expect_true(all.equal(simulation_AHR2$AHR, actual$ahr, tolerance = 1e-3))
  testthat::expect_true(all.equal(simulation_AHR2$Events, actual$event, tolerance = 2e-3))
})

testthat::test_that("AHR results are consistent with simulation results for single stratum and multiple cutoff", {
  enroll_rate <- tibble::tibble(
    stratum = "All",
    duration = c(2, 2, 10),
    rate = c(3, 6, 9)
  )
  fail_rate <- tibble::tibble(
    stratum = "All",
    duration = c(3, Inf),
    fail_rate = log(2) / c(9, 18),
    hr = c(0.9, 0.6),
    dropout_rate = rep(0.001, 2)
  )
  total_duration <- c(15, 30)

  actual <- ahr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    total_duration = total_duration
  )
  testthat::expect_true(all.equal(simulation_AHR3$AHR, actual$ahr, tolerance = 5e-3))
  testthat::expect_true(all.equal(simulation_AHR3$Events, actual$event, tolerance = 7e-3))
})
