# Test 1: independent test using AHR to check outputs of gs_info_ahr

test_that("results match if only put in targeted analysis times", {
  res <- test_gs_info_ahr()
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  total_duration <- c(18, 27, 36)

  expect_equal(
    gs_info_ahr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      analysis_time = total_duration
    ) %>% dplyr::select(time, ahr, event, info, info0),
    ahr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      total_duration = total_duration
    ) |> dplyr::select(-n)
  )
})

test_that("results match if only put in targeted events", {
  res <- test_gs_info_ahr()
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  event <- c(30, 40, 50)

  out1 <- gs_info_ahr(enroll_rate = enroll_rate, fail_rate = fail_rate, event = event)

  total_duration <- out1$time

  expect_equal(
    out1 %>% dplyr::select(time, ahr, event, info, info0),
    ahr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      total_duration = total_duration
    ) |> dplyr::select(-n)
  )

  # Since above test is based on the output "time", here is to check whether
  # the output "Time" is reasonable.

  # "Time" should be at the time points when targeted event numbers are achieved
  expect_equal(round(out1$event), round(event))
})

test_that("results match if put in both analysis time and targeted events", {
  res <- test_gs_info_ahr()
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  event <- c(30, 40, 50)
  analysis_time <- c(16, 19, 26)

  out1 <- gs_info_ahr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    event = event,
    analysis_time = analysis_time
  )

  total_duration <- out1$time

  expect_equal(
    out1 %>% dplyr::select(time, ahr, event, info, info0),
    ahr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      total_duration = total_duration
    ) |> dplyr::select(-n)
  )

  # Since above test is based on the output "Time",
  # here is to check whether the output "Time" is reasonable.

  # Either being equal to the corresponding element in the input
  # analysis_time or at the time point when targeted event number achieved.
  expect_equal(
    max((1 - (out1$time == analysis_time)) * (1 - (round(out1$event) == round(event)))),
    0
  )

  # "Time" >= input analysis_time
  expect_gte(max(out1$time - analysis_time), 0)

  # "Events" >= input events
  expect_gte(max(round(out1$event) - round(event)), 0)
})
