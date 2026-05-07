assert("enroll_rate produces the expected output", {
  expected_result <- tibble::tibble(stratum = "All", duration = 18, rate = 20)

  result <- define_enroll_rate(duration = 18, rate = 20)

  (as.data.frame(result) %==% as.data.frame(expected_result))
})

assert("fail_rate produces the expected output", {
  expected_result <- tibble::tibble(
    stratum = "All",
    duration = c(4, 100),
    fail_rate = 0.0578,
    dropout_rate = 0.001,
    hr = c(1, 0.6)
  )
  result <- define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12,
    dropout_rate = 0.001,
    hr = c(1, 0.6)
  )

  (result$stratum %==% expected_result$stratum)
  (result$duration %==% expected_result$duration)
  (all(abs(result$fail_rate - expected_result$fail_rate) < 0.0001))
  (result$dropout_rate %==% expected_result$dropout_rate)
  (result$hr %==% expected_result$hr)
})
