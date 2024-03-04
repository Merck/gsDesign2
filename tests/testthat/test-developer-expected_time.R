test_that("time to targeted events", {
  enroll_rate <- define_enroll_rate(stratum = "All", duration = c(2, 2, 10), rate = c(3, 6, 9) * 5)
  fail_rate <- define_fail_rate(
    stratum = "All", duration = c(3, 100), fail_rate = log(2) / c(9, 18),
    hr = c(.9, .6), dropout_rate = rep(.001, 2)
  )
  ratio <- 1
  x <- AHR_(
    enrollRates = enroll_rate %>% dplyr::rename(Stratum = stratum),
    failRates = fail_rate %>% dplyr::rename(Stratum = stratum, failRate = fail_rate, dropoutRate = dropout_rate),
    ratio = ratio,
    totalDuration = 20
  )
  y <- expected_time(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = ratio,
    target_event = x$Events
  )
  expect_equal(20, y$time)
})

test_that("default", {
  x1 <- expected_time()
  x2 <- tEvents_() %>% dplyr::rename(time = Time, ahr = AHR, event = Events)
  expect_equal(x1, as.data.frame(x2))
})

test_that("time to targeted events by new/old version", {
  enroll_rate <- define_enroll_rate(stratum = "All", duration = c(2, 2, 10), rate = c(3, 6, 9) * 5)
  fail_rate <- define_fail_rate(
    stratum = "All", duration = c(3, 100),
    fail_rate = log(2) / c(9, 18), hr = c(.9, .6),
    dropout_rate = rep(.001, 2)
  )
  ratio <- 1
  x1 <- expected_time(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = ratio,
    target_event = 200
  )
  x2 <- tEvents_(
    enrollRates = enroll_rate %>% dplyr::rename(Stratum = stratum),
    failRates = fail_rate %>% dplyr::rename(Stratum = stratum, failRate = fail_rate, dropoutRate = dropout_rate),
    ratio = ratio,
    targetEvents = 200
  ) %>%
    dplyr::rename(time = Time, ahr = AHR, event = Events)
  expect_equal(x1, as.data.frame(x2))
})
