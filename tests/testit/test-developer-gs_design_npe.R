assert("verify by gs_power_npe", {
  beta <- 0.1
  # new version
  x <- gs_design_npe(
    theta = c(.1, .2, .3), info = (1:3) * 40, beta = 0.1,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL), binding = TRUE
  )
  # The power is 0.9. If we re-use these bounds under alternate hypothesis, then we can get a power close to 0.9.
  y <- gs_power_npe(
    theta = c(.1, .2, .3), info = (1:3) * 40,
    upper = gs_b, upar = (x |> dplyr::filter(bound == "upper"))$z,
    lower = gs_b, lpar = -(x |> dplyr::filter(bound == "upper"))$z,
    binding = TRUE # Always use binding = TRUE for power calculations
  )
  (all.equal(y$probability[y$analysis == 3 & y$bound == "upper"], 1 - beta, tolerance = 0.003))
  # old version
  x <- gs_design_npe_(
    theta = c(.1, .2, .3), info = (1:3) * 40, beta = 0.1,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL), binding = TRUE
  )
  # The power is 0.9. If we re-use these bounds under alternate hypothesis, then we can get a power close to 0.9.
  y <- gs_power_npe_(
    theta = c(.1, .2, .3), info = (1:3) * 40,
    upper = gs_b, upar = (x |> dplyr::filter(Bound == "Upper"))$Z,
    lower = gs_b, lpar = -(x |> dplyr::filter(Bound == "Upper"))$Z,
    binding = TRUE # Always use binding = TRUE for power calculations
  )
  (all.equal(y$Probability[y$Analysis == 3 & y$Bound == "Upper"], 1 - beta, tolerance = 0.003))
})

assert("examples in spec - Lachin book p71", {
  pc <- .28 # Control response rate
  pe <- .40 # Experimental response rate
  p0 <- (pc + pe) / 2 # Ave response rate under H0
  # Information per increment of 1 in sample size
  info0 <- 1 / (p0 * (1 - p0) * 4)
  info <- 1 / (pc * (1 - pc) * 2 + pe * (1 - pe) * 2)
  # Result should round up to next even number = 652
  # Divide information needed under H1 by information per patient added
  x1_a <- gs_design_npe(theta = pe - pc, info = info, info0 = info0, info_scale = "h0_info") |>
    dplyr::select(-c(info_frac, probability0, info1))
  x1_b <- gs_design_npe(theta = pe - pc, info = info, info0 = info0, info_scale = "h1_info") |>
    dplyr::select(-c(info_frac, probability0, info1))
  x1_c <- gs_design_npe(theta = pe - pc, info = info, info0 = info0, info_scale = "h0_h1_info") |>
    dplyr::select(-c(info_frac, probability0, info1))
  x2 <- gs_design_npe_(theta = pe - pc, info = info, info0 = info0) |>
    dplyr::rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) |>
    dplyr::mutate(bound = tolower(bound))
  (all.equal(x1_c, x2, check.attributes = FALSE))
})

assert("fixed design with 3 equal info", {
  x1_a <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info_scale = "h0_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) |>
    dplyr::select(-c(info_frac, probability0, info1)) |>
    dplyr::arrange(analysis, bound)
  x1_b <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info_scale = "h1_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) |>
    dplyr::select(-c(info_frac, probability0, info1)) |>
    dplyr::arrange(analysis, bound)
  x1_c <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info_scale = "h0_h1_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) |>
    dplyr::select(-c(info_frac, probability0, info1)) |>
    dplyr::arrange(analysis, bound)
  x2 <- gs_design_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 80,
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) |>
    dplyr::rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) |>
    dplyr::mutate(bound = tolower(bound)) |>
    dplyr::select(-c(theta1, info1)) |>
    dplyr::arrange(analysis, bound)
  (as.data.frame(x1_c) %==% as.data.frame(x2))
})

assert("fixed design with 3 unequal info", {
  x1_a <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info0 = (1:3) * 90 + 10, info1 = (1:3) * 70 - 5, info_scale = "h0_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) |>
    dplyr::select(-c(info_frac, probability0, info1)) |>
    dplyr::arrange(analysis, bound)
  x1_b <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info0 = (1:3) * 90 + 10, info1 = (1:3) * 70 - 5, info_scale = "h1_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) |>
    dplyr::select(-c(info_frac, probability0, info1)) |>
    dplyr::arrange(analysis, bound)
  x1_c <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info0 = (1:3) * 90 + 10, info1 = (1:3) * 70 - 5, info_scale = "h0_h1_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) |>
    dplyr::select(-c(info_frac, probability0, info1)) |>
    dplyr::arrange(analysis, bound)
  x2 <- gs_design_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info0 = (1:3) * 90 + 10, info1 = (1:3) * 70 - 5,
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) |>
    dplyr::rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) |>
    dplyr::mutate(bound = tolower(bound)) |>
    dplyr::select(-c(theta1, info1)) |>
    dplyr::arrange(analysis, bound)
  (as.data.frame(x1_c) %==% as.data.frame(x2))
})

assert("futility at IA1; efficacy only at IA2 +FA", {
  x1_a <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info0 = (1:3) * 40, info_scale = "h0_info",
    upper = gs_spending_bound, upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_b, lpar = c(-1, -Inf, -Inf),
    test_upper = c(FALSE, TRUE, TRUE)
  ) |>
    dplyr::select(-c(info_frac, probability0, info1)) |>
    dplyr::arrange(analysis, bound)
  x1_b <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info0 = (1:3) * 40, info_scale = "h1_info",
    upper = gs_spending_bound, upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_b, lpar = c(-1, -Inf, -Inf),
    test_upper = c(FALSE, TRUE, TRUE)
  ) |>
    dplyr::select(-c(info_frac, probability0, info1)) |>
    dplyr::arrange(analysis, bound)
  x1_c <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info0 = (1:3) * 40, info_scale = "h0_h1_info",
    upper = gs_spending_bound, upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_b, lpar = c(-1, -Inf, -Inf),
    test_upper = c(FALSE, TRUE, TRUE)
  ) |>
    dplyr::select(-c(info_frac, probability0, info1)) |>
    dplyr::arrange(analysis, bound)
  x2 <- gs_design_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info0 = (1:3) * 40,
    upper = gs_spending_bound, upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_b, lpar = c(-1, -Inf, -Inf),
    test_upper = c(FALSE, TRUE, TRUE)
  ) |>
    dplyr::rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) |>
    dplyr::mutate(bound = tolower(bound)) |>
    dplyr::select(-c(theta1, info1)) |>
    dplyr::arrange(analysis, bound)
  (as.data.frame(x1_c) %==% as.data.frame(x2))
})

assert("spending bounds", {
  x1_a <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info0 = (1:3) * 50, info_scale = "h0_info",
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
  ) |>
    dplyr::select(analysis, bound, z, probability, theta, info, info0, info1) |>
    dplyr::arrange(analysis, bound)
  x1_b <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info0 = (1:3) * 50, info_scale = "h1_info",
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
  ) |>
    dplyr::select(analysis, bound, z, probability, theta, info, info0, info1) |>
    dplyr::arrange(analysis, bound)
  x1_c <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info0 = (1:3) * 50, info_scale = "h0_h1_info",
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
  ) |>
    dplyr::select(analysis, bound, z, probability, theta, info, info0, info1) |>
    dplyr::arrange(analysis, bound)
  x2 <- gs_design_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info0 = (1:3) * 50,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
  ) |>
    dplyr::rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) |>
    dplyr::mutate(bound = tolower(bound)) |>
    dplyr::select(analysis, bound, z, probability, theta, info, info0, info1) |>
    dplyr::arrange(analysis, bound)
  legacy_rows <- x1_c$analysis < 3 | x1_c$bound != "lower"
  (as.data.frame(x1_c[legacy_rows, ]) %==% as.data.frame(x2[legacy_rows, ]))
  (x1_c$z[x1_c$analysis == 3 & x1_c$bound == "lower"] ==
    x1_c$z[x1_c$analysis == 3 & x1_c$bound == "upper"])
})

assert("2-sided symmetric spend", {
  x1_a <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info_scale = "h0_info",
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  ) |>
    dplyr::select(analysis, bound, z, probability, theta, info, info0, info1) |>
    dplyr::arrange(analysis, bound)
  x1_b <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info_scale = "h1_info",
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  ) |>
    dplyr::select(analysis, bound, z, probability, theta, info, info0, info1) |>
    dplyr::arrange(analysis, bound)
  x1_c <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info_scale = "h0_h1_info",
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  ) |>
    dplyr::select(analysis, bound, z, probability, theta, info, info0, info1) |>
    dplyr::arrange(analysis, bound)
  x2 <- gs_design_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  ) |>
    dplyr::rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) |>
    dplyr::mutate(bound = tolower(bound)) |>
    dplyr::select(analysis, bound, z, probability, theta, info, info0, info1) |>
    dplyr::arrange(analysis, bound)
  legacy_rows <- x1_c$analysis < 3 | x1_c$bound != "lower"
  (as.data.frame(x1_c[legacy_rows, ]) %==% as.data.frame(x2[legacy_rows, ]))
  (x1_c$z[x1_c$analysis == 3 & x1_c$bound == "lower"] ==
    x1_c$z[x1_c$analysis == 3 & x1_c$bound == "upper"])
})

assert("Harm bound is not provided for fixed designs", {
  (has_error(
    gs_design_npe(
      theta = 0.1, info = 40,
      upper = gs_b, upar = -qnorm(0.025), test_upper = TRUE,
      lower = gs_b, lpar = -Inf, test_lower = FALSE,
      harm = gs_b, hpar = -2, test_harm = TRUE)
  ))
})

assert("Comparison with gsDesign when test.type = 4", {
  timing <- 1:3/3
  alpha <- 0.025
  beta <- 0.1
  effect <- 0.5
  standard_deviation <- 1

  fixed_normal <- nNormal(
    delta1 = effect, sd = standard_deviation,
    alpha = alpha, beta = beta, ratio = 1, outtype = 3)

  gsdesign_normal <- gsDesign(
    k = 3, test.type = 4, alpha = alpha, beta = beta,
    n.fix = fixed_normal$n, timing = timing,
    sfu = sfLDOF, sfl = sfLDOF, delta0 = 0, delta1 = effect)

  fixed_information <- fixed_normal$n / (4 * standard_deviation^2)

  gsdesign2_normal <- gs_design_npe(
    theta = effect, theta0 = 0, theta1 = effect,
    info = fixed_information * timing, info_scale = "h0_info",
    alpha = alpha, beta = beta, binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = sfLDOF, total_spend = alpha),
    lower = gs_spending_bound,
    lpar = list(sf = sfLDOF, total_spend = beta))

    # the efficacy bounds from gsDesign match gsDesign2
    (all.equal(
      gsdesign2_normal$z[gsdesign2_normal$bound == "upper"],
      gsdesign_normal$upper$bound,
      tolerance = 7e-6,
      scale = 1
    ))

    # the futility bounds from gsDesign match gsDesign2
    (all.equal(
      gsdesign2_normal$z[gsdesign2_normal$bound == "lower"],
      gsdesign_normal$lower$bound,
      tolerance = 1e-5,
      scale = 1
    ))

    # the FA efficacy bound match with futility bound in gsDesign2
    (all.equal(
      gsdesign2_normal$z[gsdesign2_normal$bound == "upper" & gsdesign2_normal$analysis == 3],
      gsdesign2_normal$z[gsdesign2_normal$bound == "lower" & gsdesign2_normal$analysis == 3],
      tolerance = 1e-8,
      scale = 1
    ))

})

assert("Comparison with gsDesign when test.type = 3", {
  timing <- 1:3/3
  alpha <- 0.025
  beta <- 0.1
  effect <- 0.5
  standard_deviation <- 1

  fixed_normal <- nNormal(
    delta1 = effect, sd = standard_deviation,
    alpha = alpha, beta = beta, ratio = 1, outtype = 3)

  gsdesign_normal <- gsDesign(
    k = 3, test.type = 3, alpha = alpha, beta = beta,
    n.fix = fixed_normal$n, timing = timing,
    sfu = sfLDOF, sfl = sfLDOF, delta0 = 0, delta1 = effect)

  fixed_information <- fixed_normal$n / (4 * standard_deviation^2)

  gsdesign2_normal <- gs_design_npe(
    theta = effect, theta0 = 0, theta1 = effect,
    info = fixed_information * timing, info_scale = "h0_info",
    alpha = alpha, beta = beta, binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = sfLDOF, total_spend = alpha),
    lower = gs_spending_bound,
    lpar = list(sf = sfLDOF, total_spend = beta))

    # the efficacy bounds from gsDesign match gsDesign2
    (all.equal(
      gsdesign2_normal$z[gsdesign2_normal$bound == "upper"],
      gsdesign_normal$upper$bound,
      tolerance = 7e-6,
      scale = 1
    ))

    # the futility bounds from gsDesign match gsDesign2
    (all.equal(
      gsdesign2_normal$z[gsdesign2_normal$bound == "lower"],
      gsdesign_normal$lower$bound,
      tolerance = 1e-5,
      scale = 1
    ))

    # the FA efficacy bound match with futility bound in gsDesign2
    (all.equal(
      gsdesign2_normal$z[gsdesign2_normal$bound == "upper" & gsdesign2_normal$analysis == 3],
      gsdesign2_normal$z[gsdesign2_normal$bound == "lower" & gsdesign2_normal$analysis == 3],
      tolerance = 1e-8,
      scale = 1
    ))

})

assert("gs_design_npe() output object is assigned a unique class", {
  # fixed design
  (inherits(gs_design_npe(), "gs_design_npe"))

  # group sequential design
  x <- gs_design_npe(
    alpha = 0.0125,
    theta = c(.1, .2, .3),
    info = (1:3) * 80,
    info0 = (1:3) * 80,
    upper = gs_b,
    upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF, alpha = 0.0125)$upper$bound,
    lower = gs_b,
    lpar = c(-1, 0, 0)
  )
  (inherits(x, "gs_design_npe"))
})
