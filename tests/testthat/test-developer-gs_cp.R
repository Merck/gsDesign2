test_that("Compare the conditional power of gsDesign and gsDesign2 under PH with efficacy only", {
  # ------------------------------ #
  #         parameters             #
  # ------------------------------ #
  alpha <- 0.025
  beta <- 0.1
  k <- 3
  test.type <- 1
  astar <- 0
  timing <- c(0.5, 0.7)
  sfu <- sfLDOF
  sfupar <- c(0)
  sfl <- sfLDOF
  sflpar <- c(0)
  hr <- 0.6
  hr0 <- 1
  eta <- 0.01
  gamma <- c(2.5, 5, 7.5, 10)
  R <- c(2, 2, 2, 6)
  S <- NULL
  T <- 18
  minfup <- 6
  ratio <- 1
  lambdaC <- log(2) / 6

  enroll_rate <- define_enroll_rate(duration = R, rate = gamma)
  fail_rate <- define_fail_rate(duration = Inf, fail_rate = lambdaC, hr = hr, dropout_rate = eta)

  # ------------------------------ #
  # conditional power of gsDesign  #
  # ------------------------------ #
  # reference: https://keaven.github.io/gsDesign/articles/ConditionalPowerPlot.html
  # original design
  x0 <- gsSurv(k = k, test.type = test.type, alpha = alpha, beta = beta,
               astar = astar, timing = timing, sfu = sfu, sfupar = sfupar,
               sfl = sfl, sflpar = sflpar, lambdaC = lambdaC, hr = hr, hr0 = hr0,
               eta = eta, gamma = gamma, R = R, S = S, T = T, minfup = minfup, ratio = ratio)

  # update the design at IA1
  x1 <- gsDesign(k = x0$k, test.type = x0$test.type,
                 alpha = x0$alpha, beta = x0$beta,
                 delta = x0$delta, delta1 = x0$delta1, delta0 = x0$delta0,
                 n.I = c(85, x0$n.I[2:3]),
                 maxn.IPlan = x0$n.I[3],
                 sfu = sfu, sfupar = sfupar,
                 sfl = sfl, sflpar = sflpar)
  # we assume an interim p-value of 0.04, one-sided.
  # this does not come close to the first efficacy or futility bound above. However, it is a trend in the right direction.
  p <- 0.04
  x2 <- gsCP(x1, i = 1, zi = -qnorm(p))
  # ------------------------------ #
  # conditional power of gsDesign2 #
  # ------------------------------ #
  # original design
  y0 <- gs_power_ahr(enroll_rate = enroll_rate |> mutate(rate = rate * (x0$eNC[3] + x0$eNE[3]) / sum(R * gamma)),
                     fail_rate = fail_rate,
                     upper = gs_spending_bound, upar = list(sf = sfu, total_spend = alpha),
                     lower = gs_b, lpar = rep(-Inf, 3),
                     event = x0$n.I, analysis_time = x0$T)

  set.seed(123)

  observed_data <- simtrial::sim_pw_surv(
    n = y0$analysis$n[y0$analysis$analysis == 3],
    stratum = data.frame(stratum = "All", p = 1),
    block = c(rep("control", 2), rep("experimental", 2)),
    enroll_rate = y0$enroll_rate,
    fail_rate = (fail_rate |> simtrial::to_sim_pw_surv())$fail_rate,
    dropout_rate = (fail_rate |> simtrial::to_sim_pw_surv())$dropout_rate)

  observed_data_ia1 <- observed_data |> simtrial::cut_data_by_event(85)
  # cut_data_by_event
  observed_event_ia1 <- sum(observed_data_ia1$event)
  planned_event_ia2 <- y0$analysis$event[2]
  planned_event_fa <- y0$analysis$event[3]

  y1 <- gs_update_ahr(x = y0,
                      ustime = c(85, y0$analysis$event[2:3])/y0$analysis$event[3],
                      observed_data = list(observed_data_ia1, NULL, NULL))

  y1$bound <- y1$bound %>%
    filter(bound != "lower")

  # blinded <- ahr_blinded(
  #   surv = survival::Surv(
  #     time = observed_data_ia1$tte,
  #     event = observed_data_ia1$event),
  #   intervals = c(Inf),
  #   hr = 0.6,
  #   ratio = 1)
  #
  # theta_hat <- blinded$theta

  theta_hat <- x2$theta[1] # observed HR
  y2 <- gs_cp(x = y1, i = 1, zi = -qnorm(p), j = 2, theta_hat = theta_hat)
  y3 <- gs_cp(x = y1, i = 1, zi = -qnorm(p), j = 3, theta_hat = theta_hat)


  # ------------------------------ #
  #       comparison               #
  # ------------------------------ #
  # comparison of sample size of the original design
  expect_equal((x0$eNC + x0$eNE) |> as.vector(), y0$analysis$n, tolerance = 1e-5)

  # comparison of events of the original design and updated design
  expect_equal(x0$n.I, y0$analysis$event, tolerance = 1e-5)
  expect_equal(x1$n.I, y1_updated$analysis$event, tolerance = 1e-5)

  # comparison of timing of the original design and updated design
  expect_equal((x0$T) |> as.vector(), y0$analysis$time, tolerance = 1e-5)
  expect_equal((x1$timing) |> as.vector(), y1$analysis$info_frac0, tolerance = 1e-5)

  # comparison of crossing probability under H1
  expect_equal(x0$upper$prob[, 2] |> cumsum(), y0$bound$probability, tolerance = 1e-2)
  expect_equal(x1$upper$prob[, 2] |> cumsum(), y1$bound$probability, tolerance = 1e-2)

  # comparison of crossing probability under H0
  expect_equal(x0$upper$prob[, 1] |> cumsum(), y0$bound$probability0, tolerance = 1e-2)
  expect_equal(x1$upper$prob[, 1] |> cumsum(), y1$bound$probability0, tolerance = 1e-2)

  # comparison of conditional power
  # theta = H0
  expect_equal(x2$upper$prob[1, 2], y2$upper_prob$prob0, tolerance = 1e-3)
  expect_equal(sum(x2$upper$prob[, 2]), y3$upper_prob$prob0, tolerance = 1e-1)
  # theta = H1
  expect_equal(x2$upper$prob[1, 3], y2$upper_prob$prob1, tolerance = 1e-2)
  expect_equal(sum(x2$upper$prob[, 3]), y3$upper_prob$prob1, tolerance = 1e-2)
  # theta = IA estimated theta
  ## !!! Not pass the tests: 1. consider the theta_hat  2. consider info0_hat
  expect_equal(sum(x2$upper$prob[1, 1]), y2$upper_prob$prob_est, tolerance = 1e-2)
  expect_equal(sum(x2$upper$prob[, 1]), y3$upper_prob$prob_est, tolerance = 1e-2)
})








