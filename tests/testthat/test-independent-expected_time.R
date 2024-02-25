test_that("expected_time equal to test_event result", {
  res <- test_expected_time()
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  t1 <- res$t1

  expect_equal(
    t1$event,
    test_event(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      td = t1$time
    )
  )
})

test_that("expected_time euqal to AHR's result", {
  res <- test_expected_time()
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  t1 <- res$t1

  expect_equal(
    t1$event,
    ahr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      total_duration = t1$time,
      ratio = 1
    )$event
  )
})
