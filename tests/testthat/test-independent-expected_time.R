enroll_rate <- define_enroll_rate(
  duration = c(2, 2, 10),
  rate = c(3, 6, 9) * 5
)

fail_rate <- define_fail_rate(
  duration = c(3, 100),
  fail_rate = log(2) / c(9, 18),
  dropout_rate = rep(.001, 2),
  hr = c(.9, .6)
)

target_event <- 150
interval <- c(.01, 100)

t1 <- expected_time(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  target_event = target_event,
  interval = interval
)

testthat::test_that("expected_time equal to test_event result", {
  testthat::expect_equal(
    t1$event,
    test_event(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      td = t1$time
    )
  )
})

testthat::test_that("expected_time euqal to AHR's result", {
  testthat::expect_equal(
    t1$event,
    ahr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      total_duration = t1$time,
      ratio = 1
    )$event
  )
})
