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
  temp <- gsSurv(k = k, test.type = test.type, alpha = alpha, beta = beta,
                 astar = astar, timing = timing, sfu = sfu, sfupar = sfupar,
                 sfl = sfl, sflpar = sflpar, lambdaC = lambdaC, hr = hr, hr0 = hr0,
                 eta = eta, gamma = gamma, R = R, S = S, T = T, minfup = minfup, ratio = ratio)

  x1 <- gsDesign(k = k, test.type = test.type,
                 alpha = alpha,
                 beta = beta,
                 astar = astar,
                 delta = temp$delta,
                 delta1 = temp$delta1,
                 delta0 = temp$delta0,
                 timing = temp$timing,
                 sfu = sfu, sfupar = sfupar,
                 sfl = sfl, sflpar = sflpar)
  y1 <- gsCP(x1, i = 1, zi = -1)

  # ------------------------------ #
  # conditional power of gsDesign2 #
  # ------------------------------ #
  x2 <- gs_power_ahr(enroll_rate = enroll_rate |> mutate(rate = rate * (temp$eNC[3] + temp$eNE[3]) / sum(R * gamma)),
                     fail_rate = fail_rate,
                     upper = gs_spending_bound, upar = list(sf = sfu, total_spend = alpha),
                     lower = gs_b, lpar = rep(-Inf, 3),
                     event = x1$n.I,
                     analysis_time = NULL)

  y2 <- gs_cp(x = x2, i = 1, zi = -1, j = 2)
  # ------------------------------ #
  #       comparison              #
  # ------------------------------ #
  # comparison of sample size
  expect_equal((temp$eNC + temp$eNE) |> as.vector(), x2$analysis$n, tolerance = 1e-5)

  # comparison of events
  expect_equal(x1$n.I, x2$analysis$event, tolerance = 1e-5)

  # comparison of timing
  expect_equal((temp$T) |> as.vector(), x2$analysis$time, tolerance = 1e-5)

  # comparison of crossing probability under H1
  expect_equal(x1$upper$prob[, 2] |> cumsum(), x2$bound$probability, tolerance = 1e-2)

  # comparison of crossing probability under H0
  expect_equal(x1$upper$prob[, 1] |> cumsum(), x2$bound$probability0, tolerance = 1e-2)

  # comparison of conditional power
  # ??? scenario 1: conditional power after the first interim analysis based on the interim estimate of theta
  sum(y1$upper$prob[, 1])
  y2$upper_prob$prob1

  # ??? scenario 2: conditional error, ie.e, based on theta = 0
  sum(y1$upper$prob[, 2])
  y2$upper_prob$prob0

  # ???  scenario 3: conditional power based on theta = H1's theta
  sum(y1$upper$prob[, 3])
  y2$upper_prob$prob1
})