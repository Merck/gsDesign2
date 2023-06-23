# Test 1: compare results with AHR ####

testthat::test_that("compare results with AHR in the situation of single analysis", {
  enroll_rate <- define_enroll_rate(
    duration = c(2, 2, 10),
    rate = c(3, 6, 9)
  )
  fail_rate <- define_fail_rate(
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    hr = c(0.9, 0.6),
    dropout_rate = rep(0.001, 2)
  )
  total_duration <- 30
  analysis_time <- total_duration

  out <- gs_design_ahr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    analysis_time = analysis_time
  )

  testthat::expect_equal(
    out$analysis %>% select(time, ahr),
    ahr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      total_duration = total_duration
    ) %>% select(time, ahr)
  )

  # update enroll_rate for AHR to make Events/info/info0 also match in outputs
  enroll_rate1 <- enroll_rate %>% mutate(rate = rate * c(out$analysis$n / (duration %*% rate)))

  testthat::expect_equal(
    out$analysis %>% select(time, ahr, event, info, info0),
    ahr(
      enroll_rate = enroll_rate1,
      fail_rate = fail_rate,
      total_duration = total_duration
    ) %>% select(time, ahr, event, info, info0)
  )
})

testthat::test_that(
  "compare results with gsDesign2::AHR in the situation with IF and multiple analysis times specified",
  {
    enroll_rate <- define_enroll_rate(
      duration = c(2, 2, 10),
      rate = c(3, 6, 9)
    )
    fail_rate <- define_fail_rate(
      duration = c(3, 100),
      fail_rate = log(2) / c(9, 18),
      dropout_rate = rep(0.001, 2),
      hr = c(0.9, 0.6)
    )
    total_duration <- c(12, 25, 36)
    analysis_time <- total_duration

    out <- gs_design_ahr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      analysis_time = analysis_time
    )

    testthat::expect_equal(
      out$analysis %>% select(time, ahr) %>% dplyr::distinct(.keep_all = TRUE),
      ahr(
        enroll_rate = enroll_rate,
        fail_rate = fail_rate,
        total_duration = total_duration
      ) %>% select(time, ahr)
    )

    # update enroll_rate for AHR to make Events/info/info0 also match in outputs
    enroll_rate1 <- enroll_rate %>% mutate(rate = rate * c(max(out$analysis$n) / (duration %*% rate)))

    testthat::expect_equal(
      out$analysis %>%
        select(time, ahr, event, info, info0) %>%
        dplyr::distinct(.keep_all = TRUE),
      ahr(
        enroll_rate = enroll_rate1,
        fail_rate = fail_rate,
        total_duration = total_duration
      ) %>%
        select(time, ahr, event, info, info0)
    )
  }
)
