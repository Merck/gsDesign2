test_that("A single ", {
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
  expect_equal(x1$n, x2)

  # A single time point after the delayed effect at 3 months
  my_time  <-  10
  x1 <- pw_info(enroll_rate = enroll_rate,
                fail_rate = fail_rate,
                total_duration = my_time)
  x2 <- expected_accrual(enroll_rate = enroll_rate,
                         time = c(fail_rate$duration[1], my_time))
  expect_equal(x1$n, x2)

  # Two time points, one before and one after the delayed effect at 3 months
  my_time  <- c(2, 10)
  all_t <- sort(c(fail_rate$duration[1], my_time))
  tbl_time <- data.frame(t_start = c(0, fail_rate$duration)[1:length(fail_rate$duration)],
                         t_end = c(0, cumsum(fail_rate$duration))[-1])

  x1 <- pw_info(enroll_rate = enroll_rate,
                fail_rate = fail_rate,
                total_duration = my_time)

  x2 <- data.frame(time = all_t,
                   n = expected_accrual(enroll_rate = enroll_rate, time = all_t))

  y <- x1 %>%
    left_join(tbl_time, by = join_by(t == t_start)) %>%
    mutate(t_min = pmin(time, t_end)) %>%
    select(time, t_min, n) %>%
    rename(n_obs = n) %>%
    left_join(x2 %>% rename(n_exp = n), by = join_by(t_min == time))
  expect_equal(y$n_obs, y$n_exp)
})