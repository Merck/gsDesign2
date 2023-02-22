# Test 1: independent test using AHR to check outputs of gs_info_ahr ####

testthat::test_that("results match if only put in targeted analysis times", {
  enroll_rate <- tibble::tibble(
    stratum = "All",
    duration = c(2, 2, 10),
    rate = c(3, 6, 9)
  )
  fail_rate <- tibble::tibble(
    stratum = "All",
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    hr = c(0.9, 0.6),
    dropout_rate = rep(0.001, 2)
  )
  total_duration <- c(18, 27, 36)

  testthat::expect_equal(
    gs_info_ahr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      analysis_time = total_duration
    ) %>% select(time, ahr, event, info, info0),
    ahr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      total_duration = total_duration
    )
  )
})


testthat::test_that("results match if only put in targeted events", {
  enroll_rate <- tibble::tibble(
    stratum = "All",
    duration = c(2, 2, 10),
    rate = c(3, 6, 9)
  )
  fail_rate <- tibble::tibble(
    stratum = "All",
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    hr = c(0.9, 0.6),
    dropout_rate = rep(0.001, 2)
  )
  event <- c(30, 40, 50)

  out1 <- gs_info_ahr(enroll_rate = enroll_rate, fail_rate = fail_rate, event = event)

  total_duration <- out1$time

  testthat::expect_equal(
    out1 %>% select(time, ahr, event, info, info0),
    ahr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      total_duration = total_duration
    )
  )

  # since above test is based on the output "time", here is to check whether the output "Time" is reasonable

  # "Time" should be at the time points when targeted event numbers are achieved
  testthat::expect_equal(round(out1$event), round(event))
})


testthat::test_that("results match if put in both analysis time and targeted events", {
  enroll_rate <- tibble::tibble(
    stratum = "All",
    duration = c(2, 2, 10),
    rate = c(3, 6, 9)
  )
  fail_rate <- tibble::tibble(
    stratum = "All",
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    hr = c(0.9, 0.6),
    dropout_rate = rep(0.001, 2)
  )
  event <- c(30, 40, 50)
  analysis_time <- c(16, 19, 26)

  out1 <- gs_info_ahr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    event = event,
    analysis_time = analysis_time
  )

  total_duration <- out1$time

  testthat::expect_equal(
    out1 %>% select(time, ahr, event, info, info0),
    ahr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      total_duration = total_duration
    )
  )

  # since above test is based on the output "Time",
  # here is to check whether the output "Time" is reasonable

  # either being equal to the corresponding element in the input
  # analysis_time or at the time point when targeted event number achieved
  testthat::expect_equal(
    max((1 - (out1$time == analysis_time)) * (1 - (round(out1$event) == round(event)))),
    0
  )

  # "Time" >= input analysis_time
  testthat::expect_gte(max(out1$time - analysis_time), 0)

  # "Events" >= input events
  testthat::expect_gte(max(round(out1$event) - round(event)), 0)
})
