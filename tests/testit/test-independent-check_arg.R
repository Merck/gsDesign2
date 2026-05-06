assert("check enrollments", {
  (has_error(check_enroll_rate(tibble::tibble(rate = c(2, 4)))))
  (has_error(check_enroll_rate(tibble::tibble(duration = c(10, 20), rate = c("a", "b")))))
  (has_error(check_enroll_rate(tibble::tibble(duration = c(10, 20), rate = c(2, -4)))))

  (has_error(check_enroll_rate(tibble::tibble(duration = c(10, 20)))))
  (has_error(check_enroll_rate(tibble::tibble(rate = c(2, 4), duration = c("a", "b")))))
  (has_error(check_enroll_rate(tibble::tibble(rate = c(2, 4), duration = c(10, -20)))))
})

assert("check fail_rate", {
  # lack duration
  (has_error(check_fail_rate(tibble::tibble(fail_rate = c(0.2, 0.4), dropout_rate = 0.01))))

  # lack fail_rate
  (has_error(check_fail_rate(tibble::tibble(duration = c(2, 4), dropout_rate = 0.01))))

  # lack dropout_rate
  (has_error(check_fail_rate(tibble::tibble(fail_rate = c(0.2, 0.4), duration = c(10, 20)))))

  # check of column `duration`
  (has_error(check_fail_rate(tibble::tibble(fail_rate = c(2, 4), duration = c("a", "b"), dropout_rate = 0.01))))

  (has_error(check_fail_rate(tibble::tibble(fail_rate = c(2, 4), duration = c(10, -20), dropout_rate = 0.01))))

  # check of column `fail_rate`
  (has_error(check_fail_rate(tibble::tibble(duration = c(10, 20), fail_rate = c("a", "b"), dropout_rate = 0.01))))

  (has_error(check_fail_rate(tibble::tibble(duration = c(10, 20), fail_rate = c(2, -4), dropout_rate = 0.01))))

  # check of column `hr`
  (has_error(check_fail_rate(
      tibble::tibble(
        duration = c(10, 20),
        fail_rate = c(0.02, 0.04),
        dropout_rate = 0.01, hr = "a"
      )
    )))

  (has_error(check_fail_rate(
      tibble::tibble(
        duration = c(10, 20),
        fail_rate = c(2, -4),
        dropout_rate = 0.01, hr = -1
      )
    )))

  # check of column `dropoutRate`
  (has_error(check_fail_rate(
      tibble::tibble(
        duration = c(10, 20),
        fail_rate = c(0.02, 0.04),
        dropout_rate = "a", hr = 0.6
      )
    )))

  (has_error(check_fail_rate(
      tibble::tibble(
        duration = c(10, 20),
        fail_rate = c(2, -4),
        dropout_rate = -1, hr = 0.6
      )
    )))
})

assert("check enrollments and fail_rate together", {
  (has_error(check_enroll_rate_fail_rate(
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
  )))
})

assert("check analysis_time", {
  (has_error(check_analysis_time("a")))
  (has_error(check_analysis_time(c(20, 10))))
})

assert("check event", {
  (has_error(check_event("a")))
  (has_error(check_event(c(20, 10))))
})

assert("check total_duration", {
  (has_error(check_total_duration("a")))
  (has_error(check_total_duration(c(-10, 10))))
})

assert("check ratio", {
  (has_error(check_ratio("a")))
  (has_error(check_ratio(-2)))
})

assert("check info", {
  (has_error(check_info(c("a", "b"))))
  (has_error(check_info(c(20, 10))))
})

assert("check theta", {
  (has_error(check_theta(c(20, 10), K = 1)))
  (has_error(check_theta(c(20, -10), K = 2)))
})

assert("check test_upper", {
  (has_error(check_test_upper(c("a", "b"), K = 2)))
  (has_error(check_test_upper(c(TRUE, FALSE, FALSE), K = 1)))
  (has_error(check_test_upper(c(TRUE, FALSE), K = 2)))
})

assert("check test_lower", {
  (has_error(check_test_lower(c(TRUE, FALSE, FALSE), K = 1)))
})

assert("check check_alpha_beta", {
  (has_error(check_alpha_beta(alpha = "a", beta = 0.2)))
  (has_error(check_alpha_beta(alpha = 0.025, beta = "b")))
  (has_error(check_alpha_beta(alpha = c(0.025, 0.05), beta = 0.2)))
  (has_error(check_alpha_beta(alpha = 0.025, beta = c(0.2, 0.3))))
  (has_error(check_alpha_beta(alpha = -1, beta = 0.1)))
  (has_error(check_alpha_beta(alpha = 0.025, beta = -0.1)))
  (has_error(check_alpha_beta(alpha = 0.5, beta = 0.6)))
})

assert("check check_info_frac", {
  (has_error(check_info_frac(c("a", "b"))))
  (has_error(check_info_frac(c(2 / 3, 1 / 3, 1))))
  (has_error(check_info_frac(c(2 / 3, 3 / 4))))
})

assert("check_enroll_rate does not require a specific class", {
  enroll_rate <- define_enroll_rate(
    duration = c(2, 2, 10),
    rate = c(3, 6, 9)
  )
  # remove class "enroll_rate"
  class_orig <- class(enroll_rate)
  class(enroll_rate) <- class_orig[class_orig != "enroll_rate"]
  (!has_error(check_enroll_rate(enroll_rate)))
})

assert("check_fail_rate does not require a specific class", {
  fail_rate <- define_fail_rate(
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    hr = c(0.9, 0.6),
    dropout_rate = 0.001
  )
  # remove class "fail_rate"
  class_orig <- class(fail_rate)
  class(fail_rate) <- class_orig[class_orig != "fail_rate"]
  (!has_error(check_fail_rate(fail_rate)))
})
