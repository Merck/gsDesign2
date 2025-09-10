library(gsDesign)

test_that("Compare the gs_cp_npe with gsDesign::gsCP", {
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
  # original design of gsDesign2   #
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
  # original design of gsDesign    #
  # ------------------------------ #
  x_gsd <- gsSurv(k = 3, test.type = 1, alpha = alpha, beta = beta,
                  astar = 0, timing = x$analysis$info_frac,
                  sfu = sfLDOF, sfupar = 0,
                  sfl = sfLDOF, sflpar = 0,
                  lambdaC = log(2) / 9, hr = 0.6, hr0 = 1,
                  eta = fail_rate$dropout_rate |> unique(),
                  gamma = enroll_rate$rate,
                  R = enroll_rate$duration,
                  S = NULL, T = analysis_time[3],
                  minfup = analysis_time[3] - sum(enroll_rate$duration),
                  ratio = ratio)

  # ----------------------------------------- #
  #  conditional power by gs_cp_npe under H0  #
  # ----------------------------------------- #
  cp12_0 <- gs_cp_npe(theta = c(0,0),
                      info = x$analysis$info0[c(1,2)],
                      a = -qnorm(0.04),
                      b = x$bound$z[x$bound$bound == "upper" & x$bound$analysis == 2])

  cp13_0 <- gs_cp_npe(theta = c(0,0),
                      info = x$analysis$info0[c(1,3)],
                      a = -qnorm(0.04),
                      b = x$bound$z[x$bound$bound == "upper" & x$bound$analysis == 3])
  # ----------------------------------------- #
  #  conditional power by gs_cp_npe under H1  #
  # ----------------------------------------- #
  cp12_1 <- gs_cp_npe(theta = x$analysis$theta[c(1,2)],
                    info = x$analysis$info[c(1,2)],
                    a = -qnorm(0.04),
                    b = x$bound$z[x$bound$bound == "upper" & x$bound$analysis == 2])

  cp13_1 <- gs_cp_npe(theta = x$analysis$theta[c(1,3)],
                    info = x$analysis$info[c(1,3)],
                    a = -qnorm(0.04),
                    b = x$bound$z[x$bound$bound == "upper" & x$bound$analysis == 3])

  # ------------------------------ #
  #  conditional power by gsDesign #
  # ------------------------------ #
  xcp_gsd <- gsCP(x_gsd, i = 1, zi = -qnorm(0.04))

  # ------------------------------ #
  #       comparison               #
  # ------------------------------ #
  # under H0
  # given IA1 assumed blinded data and compute IA2 conditional power
  expect_equal(xcp_gsd$upper$prob[1, 2],
               cp12_0,
               tolerance = 5e-2)
  # given IA1 assumed blinded data and compute FA conditional power
  expect_equal(sum(xcp_gsd$upper$prob[, 2]),
               cp13_0,
               tolerance = 5e-2)
  # under H1
  # given IA1 assumed blinded data and compute IA2 conditional power
  expect_equal(xcp_gsd$upper$prob[1, 3],
               cp12_1,
               tolerance = 5e-2)
  # given IA1 assumed blinded data and compute FA conditional power
  expect_equal(sum(xcp_gsd$upper$prob[, 3]),
               cp13_1,
               tolerance = 5e-2)

})
