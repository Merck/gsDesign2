test_that("Compare the gs_cp_ahr with Shiyu Zhang's double programming", {
  # ------------------------------ #
  #         parameters             #
  # ------------------------------ #
  alpha <- 0.025
  beta <- 0.1
  ratio <- 1

  # Enrollment
  enroll_rate <- define_enroll_rate(
    duration = c(2, 2, 10),
    rate = (1:3) / 3)

  # Failure and dropout
  fail_rate <- define_fail_rate(
    duration = Inf, fail_rate = log(2) / 9,
    hr = 0.6, dropout_rate = .0001)

  # IA and FA analysis time
  analysis_time <- c(12, 24, 36)

  # Randomization ratio
  ratio <- 1

  # Spending
  upper <- gs_spending_bound
  lower <- gs_b
  upar <- list(sf = sfLDOF, total_spend = alpha)
  lpar <- rep(-Inf, 3)

  # ------------------------------ #
  # original design                #
  # ------------------------------ #
  x <- gs_design_ahr(
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    alpha = alpha, beta = beta, ratio = ratio,
    info_scale = "h0_h1_info",
    info_frac = NULL,
    analysis_time = c(12, 24, 36),
    upper = upper, upar = upar, test_upper = TRUE,
    lower = lower, lpar = lpar, test_lower = FALSE,
  ) |>
    to_integer()

  # ------------------------------ #
  # updated design                 #
  # ------------------------------ #
  set.seed(123)

  observed_data <- simtrial::sim_pw_surv(
    n = max(x$analysis$n),
    stratum = data.frame(stratum = "All", p = 1),
    block = c(rep("control", 2), rep("experimental", 2)),
    enroll_rate = x$enroll_rate,
    fail_rate = (fail_rate |> simtrial::to_sim_pw_surv())$fail_rate,
    dropout_rate = (fail_rate |> simtrial::to_sim_pw_surv())$dropout_rate)

  observed_data_ia1 <- observed_data |> simtrial::cut_data_by_date(x$analysis$time[1])
  observed_event_ia1 <- sum(observed_data_ia1$event)
  planned_event_ia1 <- x$analysis$event[1]
  planned_event_fa <- x$analysis$event[3]

  xu <- gs_update_ahr(
    x = x,
    ustime = c(observed_event_ia1 / planned_event_fa, x$analysis$info_frac[2], 1),
    observed_data = list(observed_data_ia1, NULL, NULL))

  # ------------------------------ #
  #  conditional power by gs_cp_ahr#
  # ------------------------------ #
  cp12 <- gs_cp_ahr(x = x, xu = xu,
                    i = 1, z_i = -qnorm(0.04),
                    j = 2, z_j = x$bound$z[x$bound$bound == "upper" & x$bound$analysis == 2],
                    local_alternative = TRUE)

  cp13 <- gs_cp_ahr(x = x, xu = xu,
                    i = 1, z_i = -qnorm(0.04),
                    j = 3, z_j = x$bound$z[x$bound$bound == "upper" & x$bound$analysis == 3],
                    local_alternative = TRUE)

  # ------------------------------ #
  #  conditional power by Shiyu's  #
  #   double programming          #
  # ------------------------------ #
  sz12 <- gs_cp(x = x, x_updated = xu, i = 1, zi = -qnorm(0.4), j = 2, local_alternative = TRUE)
  sz13 <- gs_cp(x = x, x_updated = xu, i = 1, zi = -qnorm(0.4), j = 3, local_alternative = TRUE)

  # ------------------------------ #
  #       comparison               #
  # ------------------------------ #
  # theta = H0 ??
  expect_equal(cp12$cond_power[cp12$scenario == "Under H0"], sz12$upper_prob$prob0, tolerance = 1e-1)
  expect_equal(cp13$cond_power[cp13$scenario == "Under H0"], sz13$upper_prob$prob0, tolerance = 1e-1)

  # theta = H1
  expect_equal(cp12$cond_power[cp12$scenario == "Under H1"], sz12$upper_prob$prob1, tolerance = 1e-1)
  expect_equal(cp13$cond_power[cp13$scenario == "Under H1"], sz13$upper_prob$prob1, tolerance = 1e-2)

  # theta = IA estimated theta
  expect_equal(cp12$cond_power[cp12$scenario == "Under interim estimation"], sz12$upper_prob$prob1, tolerance = 1e-1)
  expect_equal(cp13$cond_power[cp13$scenario == "Under interim estimation"], sz13$upper_prob$prob1, tolerance = 1e-2)
})
