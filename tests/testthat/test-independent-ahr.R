load("fixtures/simulation_test_data.Rdata")

enroll_rate <- define_enroll_rate(
  duration = c(2, 2, 10),
  rate = c(3, 6, 9)
)

fail_rate <- define_fail_rate(
  stratum = "All",
  duration = c(3, 100),
  fail_rate = log(2) / c(9, 18),
  hr = c(.9, .6),
  dropout_rate = rep(.001, 2)
  )

test_ahr <- list(
  "simulation_ahr1" = simulation_AHR1,
  "simulation_ahr2" = simulation_AHR2,
  "simulation_ahr3" = simulation_AHR3,
  "enroll_rate" = enroll_rate,
  "fail_rate" = fail_rate
)


test_that("AHR results are consistent with simulation results for single stratum and multiple cutoff", {
  res <- test_ahr
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  simulation_ahr1 <- res$simulation_ahr1

  actual <- ahr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    total_duration = c(12, 24, 36)
  )

  expect_true(all.equal(simulation_ahr1$AHR, actual$ahr, tolerance = 0.005))
  expect_true(all.equal(simulation_ahr1$Events, actual$event, tolerance = 0.005))
})

test_that("AHR results are consistent with simulation results for single stratum and single cutoff", {
  res <- test_ahr
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  simulation_ahr2 <- res$simulation_ahr2

  total_duration <- 30
  actual <- ahr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    total_duration = total_duration
  )

  expect_true(all.equal(simulation_ahr2$AHR, actual$ahr, tolerance = 1e-3))
  expect_true(all.equal(simulation_ahr2$Events, actual$event, tolerance = 2e-3))
})

test_that("AHR results are consistent with simulation results for single stratum and multiple cutoff", {
  res <- test_ahr
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  simulation_ahr3 <- res$simulation_ahr3

  total_duration <- c(15, 30)
  actual <- ahr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    total_duration = total_duration
  )

  expect_true(all.equal(simulation_ahr3$AHR, actual$ahr, tolerance = 5e-3))
  expect_true(all.equal(simulation_ahr3$Events, actual$event, tolerance = 7e-3))
})
