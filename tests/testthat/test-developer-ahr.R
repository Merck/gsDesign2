test_that("unstratified population, compared with old version", {
  # parameters
  enroll_rate <- define_enroll_rate(
    duration = c(2, 10, 4, 4, 8),
    rate = c(5, 10, 0, 3, 6)
  )
  fail_rate <- define_fail_rate(
    stratum = "All",
    duration = c(1, 1, 1, Inf),
    fail_rate = c(.1, .2, .3, .4),
    hr = c(.9, .75, .8, .6),
    dropout_rate = .001
  )

  # latest version
  x1 <- ahr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    total_duration = c(15, 30)
  )
  y1 <- x1 |> dplyr::select(-n)

  # old version
  x2 <- AHR_(
    enrollRates = enroll_rate %>% dplyr::rename(Stratum = stratum),
    failRates = fail_rate %>% dplyr::rename(Stratum = stratum, failRate = fail_rate, dropoutRate = dropout_rate),
    totalDuration = c(15, 30)
  ) %>%
    dplyr::rename(time = Time, ahr = AHR, event = Events)

  # verify columns without n (sample size)
  expect_equal(as.data.frame(y1), as.data.frame(x2))

  # verify the columns of n (sample size)
  expect_equal(x1$n,
               expected_accrual(time = c(15, 30), enroll_rate = enroll_rate))
})

test_that("stratified population, compared with old version", {
  enroll_rate <- define_enroll_rate(
    stratum = c(rep("Low", 2), rep("High", 3)),
    duration = c(2, 10, 4, 4, 8),
    rate = c(5, 10, 0, 3, 6)
  )
  fail_rate <- define_fail_rate(
    stratum = c(rep("Low", 2), rep("High", 2)),
    duration = c(1, Inf, 1, Inf),
    fail_rate = c(.1, .2, .3, .4),
    hr = c(.9, .75, .8, .6),
    dropout_rate = .001
  )
  x1 <- ahr( # latest version
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    total_duration = c(15, 30)
  )
  y1 <- x1 |> dplyr::select(-n)

  x2 <- AHR_( # old version
    enrollRates = enroll_rate %>% dplyr::rename(Stratum = stratum),
    failRates = fail_rate %>% dplyr::rename(Stratum = stratum, failRate = fail_rate, dropoutRate = dropout_rate),
    totalDuration = c(15, 30)
  ) %>%
    dplyr::rename(time = Time, ahr = AHR, event = Events)

  # verify columns without n (sample size)
  expect_equal(as.data.frame(y1), as.data.frame(x2))

  # verify the columns of n (sample size)
  expect_equal(x1$n,
               expected_accrual(time = c(15, 30), enroll_rate = enroll_rate))
})

test_that("stratified population, compared with pw_info", {

  enroll_rate <- define_enroll_rate(
    stratum = c(rep("Low", 2), rep("High", 3)),
    duration = c(2, 10, 4, 4, 8),
    rate = c(5, 10, 0, 3, 6))

  fail_rate <- define_fail_rate(
    stratum = c(rep("Low", 2), rep("High", 2)),
    duration = c(1, Inf, 1, Inf),
    fail_rate = c(.1, .2, .3, .4),
    dropout_rate = .001,
    hr = c(.9, .75, .8, .6))

  # Give results by change-points in the piecewise model
  x <- ahr(enroll_rate = enroll_rate, fail_rate = fail_rate, total_duration = c(15, 30))

  # Same example, give results by strata and time period
  y <- pw_info(enroll_rate = enroll_rate, fail_rate = fail_rate, total_duration = c(15, 30))

  expect_equal(x$event[x$time == 15], sum(y$event[y$time == 15]))
  expect_equal(x$event[x$time == 30], sum(y$event[y$time == 30]))

  expect_equal(x$n[x$time == 15], sum(y$n[y$time == 15]))
  expect_equal(x$n[x$time == 30], sum(y$n[y$time == 30]))

  expect_equal(x$info[x$time == 15], sum(y$info[y$time == 15]))
  expect_equal(x$info[x$time == 30], sum(y$info[y$time == 30]))

  expect_equal(x$info0[x$time == 15], sum(y$info0[y$time == 15]))
  expect_equal(x$info0[x$time == 30], sum(y$info0[y$time == 30]))
})