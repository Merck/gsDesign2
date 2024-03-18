test_that("check enrollments", {
  expect_error(check_enroll_rate(tibble::tibble(rate = c(2, 4))))
  expect_error(check_enroll_rate(tibble::tibble(duration = c(10, 20), rate = c("a", "b"))))
  expect_error(check_enroll_rate(tibble::tibble(duration = c(10, 20), rate = c(2, -4))))

  expect_error(check_enroll_rate(tibble::tibble(duration = c(10, 20))))
  expect_error(check_enroll_rate(tibble::tibble(rate = c(2, 4), duration = c("a", "b"))))
  expect_error(check_enroll_rate(tibble::tibble(rate = c(2, 4), duration = c(10, -20))))
})

test_that("check fail_rate", {
  # lack duration
  expect_error(
    check_fail_rate(tibble::tibble(fail_rate = c(0.2, 0.4), dropout_rate = 0.01))
  )

  # lack fail_rate
  expect_error(
    check_fail_rate(tibble::tibble(duration = c(2, 4), dropout_rate = 0.01))
  )

  # lack dropout_rate
  expect_error(
    check_fail_rate(tibble::tibble(fail_rate = c(0.2, 0.4), duration = c(10, 20)))
  )

  # check of column `duration`
  expect_error(
    check_fail_rate(tibble::tibble(fail_rate = c(2, 4), duration = c("a", "b"), dropout_rate = 0.01))
  )

  expect_error(
    check_fail_rate(tibble::tibble(fail_rate = c(2, 4), duration = c(10, -20), dropout_rate = 0.01))
  )

  # check of column `fail_rate`
  expect_error(
    check_fail_rate(tibble::tibble(duration = c(10, 20), fail_rate = c("a", "b"), dropout_rate = 0.01))
  )

  expect_error(
    check_fail_rate(tibble::tibble(duration = c(10, 20), fail_rate = c(2, -4), dropout_rate = 0.01))
  )

  # check of column `hr`
  expect_error(
    check_fail_rate(
      tibble::tibble(
        duration = c(10, 20),
        fail_rate = c(0.02, 0.04),
        dropout_rate = 0.01, hr = "a"
      )
    )
  )

  expect_error(
    check_fail_rate(
      tibble::tibble(
        duration = c(10, 20),
        fail_rate = c(2, -4),
        dropout_rate = 0.01, hr = -1
      )
    )
  )

  # check of column `dropoutRate`
  expect_error(
    check_fail_rate(
      tibble::tibble(
        duration = c(10, 20),
        fail_rate = c(0.02, 0.04),
        dropout_rate = "a", hr = 0.6
      )
    )
  )

  expect_error(
    check_fail_rate(
      tibble::tibble(
        duration = c(10, 20),
        fail_rate = c(2, -4),
        dropout_rate = -1, hr = 0.6
      )
    )
  )
})

test_that("check enrollments and fail_rate together", {
  expect_error(check_enroll_rate_fail_rate(
    enroll_rate = tibble::tibble(
      duration = c(10, 20),
      rate = c(2, 4),
      stratum = "All"
    ),
    fail_rate = tibble::tibble(
      duration = c(10, 20),
      fail_rate = c(0.02, 0.04),
      dropout_rate = 0.001,
      hr = 0.6,
      stratum = c("S1", "S2")
    )
  ))
})

test_that("check analysis_time", {
  expect_error(check_analysis_time("a"))
  expect_error(check_analysis_time(c(20, 10)))
})

test_that("check event", {
  expect_error(check_event("a"))
  expect_error(check_event(c(20, 10)))
})

test_that("check total_duration", {
  expect_error(check_total_duration("a"))
  expect_error(check_total_duration(c(-10, 10)))
})

test_that("check ratio", {
  expect_error(check_ratio("a"))
  expect_error(check_ratio(-2))
})

test_that("check info", {
  expect_error(check_info(c("a", "b")))
  expect_error(check_info(c(20, 10)))
})

test_that("check theta", {
  expect_error(check_theta(c("a", "b"), K = 2))
  expect_error(check_theta(c(20, 10), K = 1))
  expect_error(check_theta(c(20, -10), K = 2))
})

test_that("check test_upper", {
  expect_error(check_test_upper(c("a", "b"), K = 2))
  expect_error(check_test_upper(c(TRUE, FALSE, FALSE), K = 1))
  expect_error(check_test_upper(c(TRUE, FALSE), K = 2))
})

test_that("check test_lower", {
  expect_error(check_test_lower(c("a", "b"), K = 2))
  expect_error(check_test_lower(c(TRUE, FALSE, FALSE), K = 1))
})

test_that("check check_alpha_beta", {
  expect_error(check_alpha_beta(alpha = "a", beta = 0.2))
  expect_error(check_alpha_beta(alpha = 0.025, beta = "b"))
  expect_error(check_alpha_beta(alpha = c(0.025, 0.05), beta = 0.2))
  expect_error(check_alpha_beta(alpha = 0.025, beta = c(0.2, 0.3)))
  expect_error(check_alpha_beta(alpha = -1, beta = 0.1))
  expect_error(check_alpha_beta(alpha = 0.025, beta = -0.1))
  expect_error(check_alpha_beta(alpha = 0.5, beta = 0.6))
})

test_that("check check_info_frac", {
  expect_error(check_info_frac(c("a", "b")))
  expect_error(check_info_frac(c(2 / 3, 1 / 3, 1)))
  expect_error(check_info_frac(c(2 / 3, 3 / 4)))
})

test_that("check_enroll_rate does not require a specific class", {
  enroll_rate <- define_enroll_rate(
    duration = c(2, 2, 10),
    rate = c(3, 6, 9)
  )
  # remove class "enroll_rate"
  class_orig <- class(enroll_rate)
  class(enroll_rate) <- class_orig[class_orig != "enroll_rate"]
  expect_silent(check_enroll_rate(enroll_rate))
})

test_that("check_fail_rate does not require a specific class", {
  fail_rate <- define_fail_rate(
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    hr = c(0.9, 0.6),
    dropout_rate = 0.001
  )
  # remove class "fail_rate"
  class_orig <- class(fail_rate)
  class(fail_rate) <- class_orig[class_orig != "fail_rate"]
  expect_silent(check_fail_rate(fail_rate))
})
