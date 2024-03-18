# Helper functions used by test-independent-AHR.R

test_ahr <- function() {
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

  list(
    "simulation_ahr1" = simulation_AHR1,
    "simulation_ahr2" = simulation_AHR2,
    "simulation_ahr3" = simulation_AHR3,
    "enroll_rate" = enroll_rate,
    "fail_rate" = fail_rate
  )
}
