test_that("AHR", {
  res <- test_fixed_design()
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  study_duration <- res$study_duration
  ratio <- res$ratio

  x <- fixed_design_ahr(
    alpha = 0.025,
    power = 0.9,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio
  )

  y <- fixed_design_ahr(
    alpha = 0.025,
    enroll_rate = enroll_rate %>% dplyr::mutate(rate = x$analysis$n / duration),
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio
  )

  expect_equal(y$analysis$power, 0.9)
})

test_that("FH", {
  res <- test_fixed_design()
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  study_duration <- res$study_duration
  ratio <- res$ratio

  x <- fixed_design_fh(
    alpha = 0.025,
    power = 0.9,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio,
    rho = 0.5,
    gamma = 0.5
  ) |> to_integer()

  y <- fixed_design_fh(
    alpha = 0.025,
    enroll_rate = enroll_rate %>% dplyr::mutate(rate = x$analysis$n / duration),
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio,
    rho = 0.5,
    gamma = 0.5
  )

  expect_true(y$analysis$power >= 0.9)
  expect_equal(y$analysis$power, 0.9, tolerance = 0.01)
})

test_that("MB", {
  res <- test_fixed_design()
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  study_duration <- res$study_duration
  ratio <- res$ratio

  x <- fixed_design_mb(
    alpha = 0.025,
    power = 0.9,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio,
    tau = 8
  ) |> to_integer()

  y <- fixed_design_mb(
    alpha = 0.025,
    enroll_rate = enroll_rate %>% dplyr::mutate(rate = x$analysis$n / duration),
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio,
    tau = 8
  )

  expect_true(y$analysis$power >= 0.9)
  expect_equal(y$analysis$power, 0.9, tolerance = 0.01)
})

test_that("LF", {
  res <- test_fixed_design()
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  study_duration <- res$study_duration
  ratio <- res$ratio

  x <- fixed_design_lf(
    alpha = 0.025,
    power = 0.9,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio
  ) |> to_integer()

  y <- fixed_design_lf(
    alpha = 0.025,
    enroll_rate = enroll_rate %>% dplyr::mutate(rate = x$analysis$n / duration),
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio
  )

  expect_equal(y$analysis$power, 0.9, tolerance = 0.01)
})

test_that("MaxCombo", {
  res <- test_fixed_design()
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  study_duration <- res$study_duration
  ratio <- res$ratio

  x <- fixed_design_maxcombo(
    alpha = 0.025,
    power = 0.9,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio,
    rho = c(0, 0.5, 0.5),
    gamma = c(0, 0, 0.5),
    tau = c(-1, 4, 6)
  )

  y <- fixed_design_maxcombo(
    alpha = 0.025,
    enroll_rate = enroll_rate %>% dplyr::mutate(rate = x$analysis$n / duration),
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio,
    rho = c(0, 0.5, 0.5),
    gamma = c(0, 0, 0.5),
    tau = c(-1, 4, 6)
  )

  expect_equal(y$analysis$power, 0.9, tolerance = testthat_tolerance() * 100)
})

test_that("RMST", {
  res <- test_fixed_design()
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  study_duration <- res$study_duration
  ratio <- res$ratio

  x <- fixed_design_rmst(
    alpha = 0.025,
    power = 0.9,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio,
    tau = 18
  )

  y <- fixed_design_rmst(
    alpha = 0.025,
    enroll_rate = enroll_rate %>% dplyr::mutate(rate = x$analysis$n / duration),
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio,
    tau = 18
  )

  expect_equal(y$analysis$power, 0.9)
})

test_that("RD", {
  res <- test_fixed_design()
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  study_duration <- res$study_duration
  ratio <- res$ratio

  x <- fixed_design_rd(
    alpha = 0.025,
    power = 0.9,
    p_c = .15,
    p_e = .1,
    rd0 = 0,
    ratio = ratio
  )

  y <- fixed_design_rd(
    alpha = 0.025,
    n = x$analysis$n,
    p_c = .15,
    p_e = .1,
    rd0 = 0,
    ratio = ratio
  )

  expect_equal(y$analysis$power, 0.9, tolerance = testthat_tolerance() * 2e+5)
})
