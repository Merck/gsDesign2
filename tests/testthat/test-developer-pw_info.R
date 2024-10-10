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

test_that("When there are many pieces of HRs", {
  # Expected enrollment duration is 223 months with constant enrollment rate
  enroll_rate <- tibble(stratum = "All", duration = 24, rate = 1)
  # Control group long-term RFS
  cure_rate_control <- .5
  # Control group RFS at 18 months
  rfs_18_control <- .7

  # Poisson mixture model from
  # https://merck.github.io/gsDesign2/articles/story-arbitrary-distribution.html?q=cure#poisson-mixture-model
  p_pm <- function(x, theta, lambda, lower_tail = FALSE) {
    exp(-theta * (1 - exp(-lambda * x)))
  }
  # Cure rate determines theta
  theta <- -log(cure_rate_control)
  # lambda parameter driven by both cure rate and interim time rfs_18_control
  lambda <- -log((theta + log(rfs_18_control)) / theta) / 18

  # Time points of interest in design rate
  weeks <- c(0, 6, 30)
  months <- c(weeks / (52 + 1.25/7), 1:7) * 12
  # Get control failure rates
  fail_rate_control = s2pwe(times = months[-1], survival = p_pm(months[-1], theta, lambda))
  fail_rate <- tibble(stratum = "All",
                      duration = fail_rate_control$duration,
                      fail_rate = fail_rate_control$rate,
                      hr = c(1, 0.7, rep(.5, 7)),
                      # 0.6% dropout rate per month
                      dropout_rate = -log(.994))

  expect_no_error(
    pw_info(
    enroll_rate = tibble(stratum = "All", duration = 22, rate = 1),
    fail_rate = fail_rate,
    ratio = 2)
  )
})