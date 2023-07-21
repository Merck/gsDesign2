source_files <- list.files("./old_function/", "*.R$")
sapply(paste0("./old_function/", source_files), source)

library(dplyr)

test_that("unstratified population", {
  enroll_rate <- define_enroll_rate(
    duration = c(2, 10, 4, 4, 8),
    rate = c(5, 10, 0, 3, 6)
  )
  fail_rate <- define_fail_rate(
    stratum = "All",
    duration = 1,
    fail_rate = c(.1, .2, .3, .4),
    hr = c(.9, .75, .8, .6),
    dropout_rate = .001
  )
  x1 <- ahr( # latest version
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    total_duration = c(15, 30)
  )
  x2 <- AHR_( # old version
    enrollRates = enroll_rate %>% rename(Stratum = stratum),
    failRates = fail_rate %>% rename(Stratum = stratum, failRate = fail_rate, dropoutRate = dropout_rate),
    totalDuration = c(15, 30)
  ) %>%
    rename(time = Time, ahr = AHR, event = Events)
  expect_equal(x1, x2)
})

test_that("stratified population", {
  enroll_rate <- define_enroll_rate(
    stratum = c(rep("Low", 2), rep("High", 3)),
    duration = c(2, 10, 4, 4, 8),
    rate = c(5, 10, 0, 3, 6)
  )
  fail_rate <- define_fail_rate(
    stratum = c(rep("Low", 2), rep("High", 2)),
    duration = 1,
    fail_rate = c(.1, .2, .3, .4),
    hr = c(.9, .75, .8, .6),
    dropout_rate = .001
  )
  x1 <- ahr( # latest version
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    total_duration = c(15, 30)
  )
  x2 <- AHR_( # old version
    enrollRates = enroll_rate %>% rename(Stratum = stratum),
    failRates = fail_rate %>% rename(Stratum = stratum, failRate = fail_rate, dropoutRate = dropout_rate),
    totalDuration = c(15, 30)
  ) %>%
    rename(time = Time, ahr = AHR, event = Events)
  expect_equal(x1, x2)
})
