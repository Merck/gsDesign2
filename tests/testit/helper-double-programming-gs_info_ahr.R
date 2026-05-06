# Helper functions used by test-independent-gs_info_ahr.R

test_gs_info_ahr <- function() {
  enroll_rate <- define_enroll_rate(
    duration = c(2, 2, 10),
    rate = c(3, 6, 9)
  )
  fail_rate <- define_fail_rate(
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    hr = c(0.9, 0.6),
    dropout_rate = 0.001
  )

  list("enroll_rate" = enroll_rate, "fail_rate" = fail_rate)
}
