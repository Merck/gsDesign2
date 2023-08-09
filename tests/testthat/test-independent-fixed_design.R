# Enrollment rate
enroll_rate <- define_enroll_rate(
  duration = 18,
  rate = 20
)

# Failure rates
fail_rate <- define_fail_rate(
  duration = c(4, 100),
  fail_rate = log(2) / 12,
  dropout_rate = .001,
  hr = c(1, .6)
)

# Study duration in months
study_duration <- 36

# Experimental / Control randomization ratio
ratio <- 1

test_that("AHR", {
  x <- fixed_design_ahr(
    alpha = 0.025, power = 0.9,
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio
  )

  y <- fixed_design_ahr(
    alpha = 0.025,
    enroll_rate = enroll_rate %>% mutate(rate = x$analysis$n / duration), fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio
  )

  expect_equal(y$analysis$power, 0.9)
})

test_that("FH", {
  x <- fixed_design_fh(
    alpha = 0.025, power = 0.9,
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio,
    rho = 0.5, gamma = 0.5
  )

  y <- fixed_design_fh(
    alpha = 0.025,
    enroll_rate = enroll_rate %>% mutate(rate = x$analysis$n / duration), fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio,
    rho = 0.5, gamma = 0.5
  )

  expect_equal(y$analysis$power, 0.9)
})

test_that("MB", {
  x <- fixed_design_mb(
    alpha = 0.025, power = 0.9,
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio,
    tau = 8
  )

  y <- fixed_design_mb(
    alpha = 0.025,
    enroll_rate = enroll_rate %>% mutate(rate = x$analysis$n / duration), fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio,
    tau = 8
  )

  expect_equal(y$analysis$power, 0.9)
})

test_that("LF", {
  x <- fixed_design_lf(
    alpha = 0.025, power = 0.9,
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio
  )

  y <- fixed_design_lf(
    alpha = 0.025,
    enroll_rate = enroll_rate %>% mutate(rate = x$analysis$n / duration), fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio
  )

  expect_equal(y$analysis$power, 0.9)
})

test_that("MaxCombo", {
  x <- fixed_design_maxcombo(
    alpha = 0.025, power = 0.9,
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio,
    rho = c(0, 0.5, 0.5),
    gamma = c(0, 0, 0.5),
    tau = c(-1, 4, 6)
  )

  y <- fixed_design_maxcombo(
    alpha = 0.025,
    enroll_rate = enroll_rate %>% mutate(rate = x$analysis$n / duration), fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio,
    rho = c(0, 0.5, 0.5),
    gamma = c(0, 0, 0.5),
    tau = c(-1, 4, 6)
  )

  expect_equal(y$analysis$power, 0.9, tolerance = testthat_tolerance() * 100)
})

test_that("RMST", {
  x <- fixed_design_rmst(
    alpha = 0.025, power = 0.9,
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio,
    tau = 18
  )

  y <- fixed_design_rmst(
    alpha = 0.025,
    enroll_rate = enroll_rate %>% mutate(rate = x$analysis$n / duration), fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio,
    tau = 18
  )

  expect_equal(y$analysis$power, 0.9)
})

test_that("RD", {
  x <- fixed_design_rd(
    alpha = 0.025, power = 0.9,
    p_c = .15, p_e = .1, rd0 = 0, ratio = ratio
  )

  y <- fixed_design_rd(
    alpha = 0.025, n = x$analysis$n,
    p_c = .15, p_e = .1, rd0 = 0, ratio = ratio
  )

  expect_equal(y$analysis$power, 0.9, tolerance = testthat_tolerance() * 2e+5)
})
