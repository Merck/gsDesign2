assert("expected_time equal to test_event result", {
  res <- test_expected_time()
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  t1 <- res$t1

  expected <- test_event(enroll_rate = enroll_rate, fail_rate = fail_rate, td = t1$time)
  (t1$event %==% expected)
})

assert("expected_time euqal to AHR's result", {
  res <- test_expected_time()
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  t1 <- res$t1

  expected <- ahr(enroll_rate = enroll_rate, fail_rate = fail_rate, total_duration = t1$time, ratio = 1)$event
  (t1$event %==% expected)
})
