library(dplyr)

source_files <- list.files("./old_function/", "*.R$")
sapply(paste0("./old_function/", source_files), source)

test_that("expected event vs gsDesign", {
  enroll_rate <- define_enroll_rate(duration = c(2, 1, 2), rate = c(5, 10, 20))
  fail_rate <- define_fail_rate(duration = c(1, 1, 1), fail_rate = c(.05, .02, .01), dropout_rate = .01)
  total_duration <- 20
  x1 <- gsDesign::eEvents( # gsDesign
    lambda = fail_rate$fail_rate,
    S = fail_rate$duration[1:(nrow(fail_rate) - 1)],
    eta = fail_rate$dropout_rate,
    gamma = enroll_rate$rate,
    R = enroll_rate$duration,
    T = total_duration
  )$d
  x2 <- eEvents_df_( # gsDesign2 old version
    enrollRates = enroll_rate %>% rename(Stratum = stratum),
    failRates = fail_rate %>% rename(Stratum = stratum, failRate = fail_rate, dropoutRate = dropout_rate),
    total_duration,
    simple = TRUE
  )
  x3 <- expected_event( # gsDesign2 latest version
    enroll_rate, fail_rate, total_duration,
    simple = TRUE
  )
  expect_equal(x1, x2)
  expect_equal(x2, x3)
})

test_that("expected_event returns consistent results (regression tests)", {
  # Note: all expected outputs were computed on 2023-08-11 with gsDesign2
  # version 1.0.9 (installed from CRAN). This required using `tibble::tibble()`
  # instead of the newer `define_enroll_rate()` and `define_fail_rate()`

  # Default arguments, simple output (total event count only)
  observed <- expected_event()
  expected <- 57.3537033783035
  expect_equal(observed, expected)

  # Event count by time period
  observed <- expected_event(simple = FALSE)
  expected <- tibble::tibble(
    t = c(0, 3),
    fail_rate = c(0.0770163533955495, 0.0385081766977747),
    event = c(22.2482399817186, 35.1054633965849)
  )
  expect_equal(observed, expected)

  # Early cutoff
  observed <- expected_event(total_duration = .5)
  expected <- 0.0285092329152224
  expect_equal(observed, expected)

  # Single time period example
  observed <- expected_event(
    enroll_rate = define_enroll_rate(duration = 10, rate = 10),
    fail_rate = define_fail_rate(duration = 100, fail_rate = log(2) / 6, dropout_rate = .01),
    total_duration = 22,
    simple = FALSE
  )
  expected <- tibble::tibble(
    t = 0,
    fail_rate = 0.115524530093324,
    event = 80.4097370913342
  )
  expect_equal(observed, expected)

  # Single time period example, multiple enrollment periods
  observed <- expected_event(
    enroll_rate = define_enroll_rate(duration = c(5, 5), rate = c(10, 20)),
    fail_rate = define_fail_rate(duration = 100, fail_rate = log(2) / 6, dropout_rate = .01),
    total_duration = 22, simple = FALSE
  )
  expected <- tibble::tibble(
    t = 0,
    fail_rate = 0.115524530093324,
    event = 118.848383110223
  )
  expect_equal(observed, expected)

  # example from https://github.com/Merck/gsDesign2/issues/250
  observed <- expected_event(
    enroll_rate = define_enroll_rate(duration = c(2, 1, 2), rate = c(5, 10, 20)),
    fail_rate = define_fail_rate(duration = c(1, 1, 100), fail_rate = c(0.05, 0.02, 0.01), dropout_rate = 0.01),
    total_duration = 12,
    simple = FALSE
  )
  expected <- tibble::tibble(
    t = c(0, 1, 2),
    fail_rate = c(0.05, 0.02, 0.01),
    event = c(2.91177332078756, 1.11333393252082, 3.45481304353542)
  )
  expect_equal(observed, expected)
})
