# Enrollment rate
enroll_rate <- tibble::tibble(
  stratum = "All",
  duration = 18,
  rate = 20
)

# Failure rates
fail_rate <- tibble::tibble(
  stratum = "All",
  duration = c(4, 100),
  fail_rate = log(2) / 12,
  hr = c(1, .6),
  dropout_rate = .001
)

# Study duration in months
study_duration <- 36

# Experimental / Control randomization ratio
ratio <- 1

test_that("input checking", {
  # miss enroll_rate
  expect_error(
    fixed_design(
      "ahr",
      alpha = 0.025,
      power = 0.9,
      fail_rate = fail_rate,
      study_duration = study_duration,
      ratio = ratio
    )
  )

  # miss fail_rate
  expect_error(
    fixed_design(
      "ahr",
      alpha = 0.025,
      power = 0.9,
      enroll_rate = enroll_rate,
      study_duration = study_duration,
      ratio = ratio
    )
  )

  # multiple rho for FH/MB
  expect_error(fixed_design("fh",
    alpha = 0.025, power = 0.9, enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio, rho = c(0.5, 0)
  ))
  expect_error(fixed_design("mb",
    alpha = 0.025, power = 0.9, enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio, rho = c(0.5, 0)
  ))

  # multiple tau for FH/MB
  expect_error(fixed_design("fh",
    alpha = 0.025, power = 0.9, enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio, tau = c(0.5, 0)
  ))
  expect_error(fixed_design("mb",
    alpha = 0.025, power = 0.9, enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio, tau = c(0.5, 0)
  ))

  # redundant tau in FH
  expect_error(fixed_design("fh",
    alpha = 0.025, power = 0.9, enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio, tau = 0.5
  ))

  # redundant rho/gamma in MB
  expect_error(fixed_design("mb",
    alpha = 0.025, power = 0.9, enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio, rho = 0.5, gamma = 0.5
  ))

  # p_c/p_e/rd0 not input in RD
  expect_error(fixed_design("rd", alpha = 0.025, power = 0.9, p_e = 0.1, rd0 = 0, ratio = ratio))
  expect_error(fixed_design("rd", alpha = 0.025, power = 0.9, p_c = 0.1, rd0 = 0, ratio = ratio))
  expect_error(fixed_design("rd", alpha = 0.025, power = 0.9, p_c = 0.2, p_e = 0.1, ratio = ratio))
  expect_error(fixed_design("rd", alpha = 0.025, p_c = 0.2, p_e = 0.1, rd0 = 0, ratio = ratio))
})

test_that("AHR", {
  x <- fixed_design("ahr",
    alpha = 0.025, power = 0.9,
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio
  )

  y <- fixed_design("ahr",
    alpha = 0.025,
    enroll_rate = enroll_rate %>% mutate(rate = x$analysis$n / duration), fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio
  )

  expect(y$analysis$power, 0.9)
})

test_that("FH", {
  x <- fixed_design("fh",
    alpha = 0.025, power = 0.9,
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio,
    rho = 0.5, gamma = 0.5
  )

  y <- fixed_design("fh",
    alpha = 0.025,
    enroll_rate = enroll_rate %>% mutate(rate = x$analysis$n / duration), fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio,
    rho = 0.5, gamma = 0.5
  )

  expect(y$analysis$power, 0.9)
})

test_that("MB", {
  x <- fixed_design("mb",
    alpha = 0.025, power = 0.9,
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio,
    tau = 8
  )

  y <- fixed_design("mb",
    alpha = 0.025,
    enroll_rate = enroll_rate %>% mutate(rate = x$analysis$n / duration), fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio,
    tau = 8
  )

  expect(y$analysis$power, 0.9)
})

test_that("LF", {
  x <- fixed_design("lf",
    alpha = 0.025, power = 0.9,
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio
  )

  y <- fixed_design("lf",
    alpha = 0.025,
    enroll_rate = enroll_rate %>% mutate(rate = x$analysis$n / duration), fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio
  )

  expect(y$analysis$power, 0.9)
})

test_that("MaxCombo", {
  x <- fixed_design("maxcombo",
    alpha = 0.025, power = 0.9,
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio,
    rho = c(0, 0.5, 0.5),
    gamma = c(0, 0, 0.5),
    tau = c(-1, 4, 6)
  )

  y <- fixed_design("maxcombo",
    alpha = 0.025,
    enroll_rate = enroll_rate %>% mutate(rate = x$analysis$n / duration), fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio,
    rho = c(0, 0.5, 0.5),
    gamma = c(0, 0, 0.5),
    tau = c(-1, 4, 6)
  )

  expect(y$analysis$power, 0.9)
})

test_that("RMST", {
  x <- fixed_design("rmst",
    alpha = 0.025, power = 0.9,
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio,
    tau = 18
  )

  y <- fixed_design("rmst",
    alpha = 0.025,
    enroll_rate = enroll_rate %>% mutate(rate = x$analysis$n / duration), fail_rate = fail_rate,
    study_duration = study_duration, ratio = ratio,
    tau = 18
  )

  expect(y$analysis$power, 0.9)
})

test_that("RD", {
  x <- fixed_design("rd",
    alpha = 0.025, power = 0.9,
    p_c = .15, p_e = .1, rd0 = 0, ratio = ratio,
    tau = 18
  )

  y <- fixed_design("rd",
    alpha = 0.025, n = x$analysis$n,
    p_c = .15, p_e = .1, rd0 = 0, ratio = ratio,
    tau = 18
  )

  expect(y$analysis$power, 0.9)
})
