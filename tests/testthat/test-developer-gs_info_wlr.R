test_that("statistcial information of WLR under logrank test is approximately same as that from AHR", {
  enroll_rate <- define_enroll_rate(duration = c(2, 2, 2, 18), rate = c(1, 2, 3, 4), stratum = "All")

  duration <- diff(c(0, 4, 6, 44))
  control_rate <- log(2)/c(6, 6, 6)
  fail_rate <- define_fail_rate(duration, fail_rate = control_rate,
                                dropout_rate = c(0.001, 0.001, 0.001),
                                hr = c(1, 0.8, 0.6))

  x1 <- gs_info_ahr(enroll_rate = enroll_rate,
                    fail_rate = fail_rate,
                    ratio = 1,
                    event = NULL,
                    analysis_time = c(9, 27, 36))

  x2 <- gs_info_wlr(enroll_rate = enroll_rate,
                    fail_rate = fail_rate,
                    ratio = 1,
                    event = NULL,
                    analysis_time = c(9, 27, 36),
                    weight = "logrank")

  # info0 of AHR and WLR is approximately close
  expect_equal(x1$info0, x2$info0, tolerance = 1e-2)
  # info0 of WLR is approximately proportional to events
  expect_equal(x2$event / x2$info0, c(4, 4, 4), tolerance = 1e-2)
})
