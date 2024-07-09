test_that("Output column of n matches with expected_accrual with Inf in the fail_rate", {

  enroll_rate <- define_enroll_rate(duration = 24, rate = 1)
  fail_rate <- define_fail_rate(duration = c(3, Inf), hr = c(1, 0.6),
                                fail_rate = log(2)/10, dropout_rate = 0.001)

  # A single time point before the delayed effect at 3 months
  my_time  <-  2
  x1 <- pw_info(enroll_rate = enroll_rate,
                fail_rate = fail_rate,
                total_duration = my_time)
  x2 <- expected_accrual(enroll_rate = enroll_rate,
                         time = my_time)
  expect_equal(cumsum(x1$n), x2)

  # A single time point after the delayed effect at 3 months
  my_time  <-  10
  x1 <- pw_info(enroll_rate = enroll_rate,
                fail_rate = fail_rate,
                total_duration = my_time)
  x2 <- expected_accrual(enroll_rate = enroll_rate,
                         time = c(fail_rate$duration[1], my_time))
  expect_equal(cumsum(x1$n), x2)

  # Two time points, one before and one after the delayed effect at 3 months
  my_time  <- c(2, 10)

  x1 <- pw_info(enroll_rate = enroll_rate,
                fail_rate = fail_rate,
                total_duration = my_time)

  expect_equal(sum(x1$n[x1$time == 2]), expected_accrual(enroll_rate = enroll_rate, time = 2))
  expect_equal(sum(x1$n[x1$time == 10]), expected_accrual(enroll_rate = enroll_rate, time = 10))
})

test_that("Output column of n matches with expected_accrual without Inf in the fail_rate", {

  enroll_rate <- define_enroll_rate(duration = 24, rate = 1)
  fail_rate <- define_fail_rate(duration = c(3, 100), hr = c(1, 0.6),
                                fail_rate = log(2)/10, dropout_rate = 0.001)

  # A single time point before the delayed effect at 3 months
  my_time  <-  2
  x1 <- pw_info(enroll_rate = enroll_rate,
                fail_rate = fail_rate,
                total_duration = my_time)
  x2 <- expected_accrual(enroll_rate = enroll_rate,
                         time = my_time)
  expect_equal(cumsum(x1$n), x2)

  # A single time point after the delayed effect at 3 months
  my_time  <-  10
  x1 <- pw_info(enroll_rate = enroll_rate,
                fail_rate = fail_rate,
                total_duration = my_time)
  x2 <- expected_accrual(enroll_rate = enroll_rate,
                         time = c(fail_rate$duration[1], my_time))
  expect_equal(cumsum(x1$n), x2)

  # Two time points, one before and one after the delayed effect at 3 months
  my_time  <- c(2, 10)

  x1 <- pw_info(enroll_rate = enroll_rate,
                fail_rate = fail_rate,
                total_duration = my_time)

  expect_equal(sum(x1$n[x1$time == 2]), expected_accrual(enroll_rate = enroll_rate, time = 2))
  expect_equal(sum(x1$n[x1$time == 10]), expected_accrual(enroll_rate = enroll_rate, time = 10))

})

test_that("Column order is consistent", {
  observed <- colnames(pw_info())
  expected <- c("time", "stratum", "t", "hr", "n", "event", "info", "info0")
  expect_equal(observed, expected)
})
