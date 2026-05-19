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

assert("AHR", {

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
    enroll_rate = enroll_rate |> dplyr::mutate(rate = x$analysis$n / duration),
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio
  )

  (all.equal(y$analysis$power, 0.9))
})

assert("FH", {

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
    enroll_rate = enroll_rate |> dplyr::mutate(rate = x$analysis$n / duration),
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio,
    rho = 0.5,
    gamma = 0.5
  )

  (y$analysis$power >= 0.9)
  (all.equal(y$analysis$power, 0.9, tolerance = 0.002))
})

assert("MB", {

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
    enroll_rate = enroll_rate |> dplyr::mutate(rate = x$analysis$n / duration),
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio,
    tau = 8
  )

  (y$analysis$power >= 0.9)
  (all.equal(y$analysis$power, 0.9, tolerance = 0.0005))
})

assert("LF", {

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
    enroll_rate = enroll_rate |> dplyr::mutate(rate = x$analysis$n / duration),
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio
  )

  (all.equal(y$analysis$power, 0.9, tolerance = 0.0002))
})

assert("MaxCombo", {

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
    enroll_rate = enroll_rate |> dplyr::mutate(rate = x$analysis$n / duration),
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio,
    rho = c(0, 0.5, 0.5),
    gamma = c(0, 0, 0.5),
    tau = c(-1, 4, 6)
  )

  (all.equal(y$analysis$power, 0.9, tolerance = 7e-7))
})

assert("RMST", {

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
    enroll_rate = enroll_rate |> dplyr::mutate(rate = x$analysis$n / duration),
    fail_rate = fail_rate,
    study_duration = study_duration,
    ratio = ratio,
    tau = 18
  )

  (all.equal(y$analysis$power, 0.9))
})

assert("RD", {

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

  (all.equal(y$analysis$power, 0.9))
})
