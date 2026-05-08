assert("The IA nominal p-value is the same as the IA alpha spending.", {
  x <- gs_design_ahr(
    upper = gs_spending_bound,
    analysis_time = c(18, 30),
    upar = list(
      sf = gsDesign::sfLDOF,
      total_spend = 0.025,
      param = NULL,
      timing = c(18, 30) / 30
    ),
    lower = gs_b,
    lpar = c(-Inf, -Inf)
  ) |> to_integer()

  (all.equal(x$bound$`nominal p`[1], gsDesign::sfLDOF(alpha = 0.025, t = 18/30)$spend[1]))
})

assert("The statistcial information under null equals to event/4 udner equal randomization.", {
  enroll_rate <- define_enroll_rate(duration = c(2, 2, 2, 6),
                                    rate = 1:4)
  fail_rate <- define_fail_rate(duration = Inf,
                                fail_rate = log(2) / 10,
                                hr = .7,
                                dropout_rate = 0.001)

  alpha <- 0.025
  beta <- 0.1
  ratio <- 1

  x <- gs_design_ahr(
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    ratio = ratio,
    beta = beta,
    alpha = alpha,
    # Information fraction at analyses and trial duration
    info_frac = c(0.6, 0.8, 1),
    analysis_time = 48,
    # Function and parameter(s) for upper spending bound
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = alpha, param = NULL),
    test_upper = c(FALSE, TRUE, TRUE),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = beta, param = -4) ,
    test_lower = c(TRUE, FALSE,FALSE),
    binding = FALSE
    ) |>
    to_integer()

  (all(x$analysis$info0 - x$analysis$event / 4 == 0))
})

assert("Validate the sample size rounding under equal randomization (1:1) for TTE endpoint. -- GSD", {

  x <- gs_design_ahr(analysis_time = c(24, 36))

  # ----------------------- #
  # round_up_final = TRUE   #
  # ----------------------- #
  y1 <- x |> to_integer(round_up_final = TRUE)
  # test the sample size at FA is rounded up and multiple of 2
  expected <- ceiling(x$analysis$n[2]/2) * 2
  (expected %==% y1$analysis$n[2])
  # test the event at FA is rounded up
  (ceiling(x$analysis$event[2]) %==% y1$analysis$event[2])
  # test the event at IA is rounded
  (round(x$analysis$event[1], 0) %==% y1$analysis$event[1])


  # ----------------------- #
  # round_up_final = FALSE  #
  # ----------------------- #
  y2 <- x |> to_integer(round_up_final = FALSE)
  # test the sample size at FA is rounded up and multiple of 2
  expected <- round(x$analysis$n[2]/2, 0) * 2
  (expected %==% y2$analysis$n[2])
  # test the event at FA is rounded
  (round(x$analysis$event[2], 0) %==% y2$analysis$event[2])
  # test the event at IA is rounded
  (round(x$analysis$event[1], 0) %==% y2$analysis$event[1])

  (has_error(x |> to_integer(ratio = -2)))
})

assert("Validate the sample size rounding under unequal randomization (3:2) for TTE endpoint. -- GSD", {

  x <- gs_design_ahr(analysis_time = c(24, 36), ratio = 1.5,
                     alpha = 0.025,
                     upper = gs_spending_bound,
                     upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025))

  # ---------------------------------------------- #
  # round_up_final = TRUE & ratio is NOT integer   #
  # ---------------------------------------------- #
  y1 <- x |> to_integer(round_up_final = TRUE)
  # test the FA sample size is rounded up, but may not be a multiplier of 5
  (ceiling(x$analysis$n[2]) %==% y1$analysis$n[2])
  # test the FA events is rounded up
  (ceiling(x$analysis$event[2]) %==% y1$analysis$event[2])
  # test the IA events is rounded
  (round(x$analysis$event[1], 0) %==% y1$analysis$event[1])

  # ---------------------------------------------- #
  # round_up_final = TRUE & ratio is integer       #
  # ---------------------------------------------- #
  y2 <- x |> to_integer(round_up_final = TRUE, ratio = 4)
  # test the FA sample size is round up, and is a multiplier of 5
  expected <- ceiling(x$analysis$n[2]/5) * 5
  (expected %==% y2$analysis$n[2])
  # test the FA events is rounded up
  (ceiling(x$analysis$event[2]) %==% y2$analysis$event[2])
  # test the IA events is rounded
  (round(x$analysis$event[1], 0) %==% y2$analysis$event[1])

  # ---------------------------------------------- #
  # round_up_final = FALSE & ratio is NOT integer  #
  # ---------------------------------------------- #
  y3 <- x |> to_integer(round_up_final = FALSE)
  # test the sample size at FA is rounded, but may not a multiplier of 5
  (round(x$analysis$n[2]) %==% y3$analysis$n[2])
  # test the FA events is rounded
  (round(x$analysis$event[2], 0) %==% y2$analysis$event[2])
  # test the IA events is rounded
  (round(x$analysis$event[1], 0) %==% y2$analysis$event[1])

  # ---------------------------------------------- #
  # round_up_final = FALSE & ratio is integer      #
  # ---------------------------------------------- #
  y4 <- x |> to_integer(round_up_final = FALSE, ratio = 4)
  # test the FA sample size is rounded, but may not is a multiplier of 5
  expected <- round(x$analysis$n[2]/5, 0) * 5
  (expected %==% y4$analysis$n[2])
  # test the FA events is rounded
  (round(x$analysis$event[2], 0) %==% y2$analysis$event[2])
  # test the IA events is rounded
  (round(x$analysis$event[1], 0) %==% y2$analysis$event[1])

  # error when ratio is negative
  (has_error(x |> to_integer(ratio = -2)))
})

assert("Validate the sample size rounding under equal randomization (1:1) for binary endpoint. -- GSD", {

  x <- gs_design_rd(ratio = 1,
                    alpha = 0.025,
                    info_frac = 1:3/3,
                    upper = gs_spending_bound,
                    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025))

  y1 <- x |> to_integer(round_up_final = TRUE)
  y2 <- x |> to_integer(round_up_final = FALSE)

  expected <- c(round(x$analysis$n[1:2], 0), ceiling(x$analysis$n[3]/2) * 2)
  (expected %==% y1$analysis$n)
  expected <- c(round(x$analysis$n[1:2], 0), round(x$analysis$n[3]/2, 0) * 2)
  (expected %==% y2$analysis$n)

  (has_error(x |> to_integer(ratio = -2)))
})

assert("Validate the sample size rounding under unequal randomization (3:2) for binary endpoint. -- GSD", {

  x <- gs_design_rd(ratio = 1.5,
                    alpha = 0.025,
                    info_frac = 1:3/3,
                    upper = gs_spending_bound,
                    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025))

  # ceiling the sample size at FA, but may not be a multiplier of 5
  y1 <- x |> to_integer(round_up_final = TRUE)
  (ceiling(x$analysis$n[3]) %==% y1$analysis$n[3])

  # ceiling the sample size at FA, and is a multiplier of 5
  y2 <- x |> to_integer(round_up_final = TRUE, ratio = 4)
  expected <- ceiling(x$analysis$n[3]/4) * 4
  (expected %==% y2$analysis$n[3])

  # round the sample size at FA, but may not a multiplier of 5
  y3 <- x |> to_integer(round_up_final = FALSE)
  (round(x$analysis$n[3], 0) %==% y3$analysis$n[3])

  # round the sample size at FA, and is a multiplier of 5
  y4 <- x |> to_integer(round_up_final = FALSE, ratio = 4)
  expected <- round(x$analysis$n[3]/5, 0) * 5
  (expected %==% y4$analysis$n[3])

  # error when ratio is negative
  (has_error(x |> to_integer(ratio = -2)))
})

assert("Validate the sample size rounding under equal randomization (1:1) for TTE endpoint -- fixed design.", {

  x <- fixed_design_ahr(alpha = .025, power = .9, ratio = 1,
                        enroll_rate = define_enroll_rate(duration = 18, rate = 1),
                        fail_rate = define_fail_rate(duration = c(4, 100),
                                                     fail_rate = log(2) / 10, hr = c(1, .6),
                                                     dropout_rate = .001),
                        study_duration = 36)

  y1 <- x |> to_integer(round_up_final = TRUE)
  y2 <- x |> to_integer(round_up_final = FALSE)

  expected <- ceiling(x$analysis$n/2) * 2
  (expected %==% y1$analysis$n)
  expected <- round(x$analysis$n/2, 0) * 2
  (expected %==% y2$analysis$n)

  (has_error(x |> to_integer(ratio = -2)))
})

assert("Validate the sample size rounding under unequal randomization (3:2) for TTE endpoint.", {

  x <- fixed_design_ahr(alpha = .025, power = .9, ratio = 1.5,
                        enroll_rate = define_enroll_rate(duration = 18, rate = 1),
                        fail_rate = define_fail_rate(duration = c(4, 100),
                                                     fail_rate = log(2) / 10, hr = c(1, .6),
                                                     dropout_rate = .001),
                        study_duration = 36)

  # ---------------------------------------------- #
  # round_up_final = TRUE & ratio is NOT integer   #
  # ---------------------------------------------- #
  y1 <- x |> to_integer(round_up_final = TRUE)
  # test the sample size is rounded up, but may not be a multiplier of 5
  (ceiling(x$analysis$n) %==% y1$analysis$n)
  # test the event is rounded up
  (ceiling(x$analysis$event) %==% y1$analysis$event)

  # ---------------------------------------------- #
  # round_up_final = TRUE & ratio is integer       #
  # ---------------------------------------------- #
  y2 <- x |> to_integer(round_up_final = TRUE, ratio = 4)
  # test the sample size is rounded up, and is a multiplier of 5
  expected <- ceiling(x$analysis$n/5) * 5
  (expected %==% y2$analysis$n)
  # test the event is rounded up
  (ceiling(x$analysis$event) %==% y2$analysis$event)

  # ---------------------------------------------- #
  # round_up_final = FALSE & ratio is NOT integer  #
  # ---------------------------------------------- #
  y3 <- x |> to_integer(round_up_final = FALSE)
  # test the sample size is rounded, but may not a multiplier of 5
  (round(x$analysis$n) %==% y3$analysis$n)
  # test the event is rounded
  (round(x$analysis$event, 0) %==% y3$analysis$event)

  # ---------------------------------------------- #
  # round_up_final = FALSE & ratio is integer      #
  # ---------------------------------------------- #
  y4 <- x |> to_integer(round_up_final = FALSE, ratio = 4)
  # test the sample size is rounded, and is a multiplier of 5
  expected <- round(x$analysis$n/5, 0) * 5
  (expected %==% y4$analysis$n)
  # test the event is rounded
  (ceiling(x$analysis$event) %==% y4$analysis$event)

  # error when ratio is negative
  (has_error(x |> to_integer(ratio = -2)))
})

assert("Validate the boundary is symmetric in symmetric designs.", {
  x <- gs_design_ahr(analysis_time = 36, info_frac = 1:3/3,
                     upper = gs_spending_bound,
                     upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
                     lower = gs_spending_bound,
                     lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
                     binding = TRUE, h1_spending = FALSE) |>
  to_integer()

  upper_z <- x$bound$z[x$bound$bound == "upper"]
  lower_z <- x$bound$z[x$bound$bound == "lower"]
  (all.equal(upper_z, -lower_z))
})

assert("verify the crossing prob of a MB design at IA1 under null", {
  x <- gs_power_wlr(enroll_rate = define_enroll_rate(duration = 12, rate = 35.8),
                    fail_rate = define_fail_rate(duration = c(4, 100),
                                                 fail_rate = log(2)/12,
                                                 dropout_rate = 0.001,
                                                 hr = c(1, 0.6)),
                    analysis_time = c(20, 28, 36),
                    weight = list(method = "mb", param = list(tau = NULL, w_max = 2)),
                    upper = gs_spending_bound,
                    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
                    lpar = rep(-Inf, 3),
                    lower = gs_b,
                    test_lower = FALSE) |> to_integer()

  res <- filter(x$bounds, bound == "upper", analysis == 1)$probability0
  expected <- sfLDOF(alpha = 0.025, t = x$analysis$info_frac0)$spend[1]
  (all.equal(res, expected, tolerance = 1e-5))
})

assert("The attribute `uninteger_is_from` matches the input design object", {
  design_funcs <- c("gs_design_ahr", "gs_design_rd", "gs_design_wlr")
  res <- vapply(design_funcs, function(f) {
    fun <- getFromNamespace(f, "gsDesign2")
    attr(to_integer(fun()), "uninteger_is_from")
  }, character(1))
  (unname(res) %==% design_funcs)

  lpar <- list(sf = gsDesign::sfLDOF, total_spend = 0.1)
  power_funcs <- c("gs_power_ahr", "gs_power_rd", "gs_power_wlr")
  res <- vapply(power_funcs, function(f) {
    fun <- getFromNamespace(f, "gsDesign2")
    x <- if (f == "gs_power_rd") fun() else fun(lpar = lpar)
    attr(to_integer(x), "uninteger_is_from")
  }, character(1))
  (unname(res) %==% power_funcs)
})
