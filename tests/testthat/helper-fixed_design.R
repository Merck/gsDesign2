# Helper functions used by test-independent-fixed_design.R

test_fixed_design <- function() {
  # Enrollment rate
  enroll_rate <- define_enroll_rate(
    duration = 18,
    rate = 20
  )

  # Failure rates
  fail_rate <- define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12,
    dropout_rate = .001,
    hr = c(1, .6)
  )

  # Study duration in months
  study_duration <- 36

  # Experimental / Control randomization ratio
  ratio <- 1

  list(
    "enroll_rate" = enroll_rate,
    "fail_rate" = fail_rate,
    "study_duration" = study_duration,
    "ratio" = ratio
  )
}
