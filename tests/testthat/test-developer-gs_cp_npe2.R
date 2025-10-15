library(gsDesign)

test_that("Compare the gs_cp_npe2 with gsDesign::gsCP", {
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
  # original design of gsDesign    #
  # ------------------------------ #
  x_gsd <- gsSurv(k = 3, test.type = 1, alpha = alpha, beta = beta,
                  astar = 0, timing = 1:3/3,
                  sfu = sfLDOF, sfupar = 0,
                  sfl = sfLDOF, sflpar = 0,
                  lambdaC = log(2) / 9, hr = 0.6, hr0 = 1,
                  eta = fail_rate$dropout_rate |> unique(),
                  gamma = enroll_rate$rate,
                  R = enroll_rate$duration,
                  S = NULL, T = analysis_time[3],
                  minfup = analysis_time[3] - sum(enroll_rate$duration),
                  ratio = ratio)

  # --------------------------------------------------- #
  #               case 1                                #
  # currently at IA1, compute conditional power at IA2  #
  # --------------------------------------------------- #
  gsDesign_cp <- gsCP(x_gsd, i = 1, zi = -qnorm(0.04), theta = -log(0.8)/2)

  gsDesign2_cp <- gs_cp_npe2(# IA1's Z-score
                             c = -qnorm(0.04),
                             # IA1, IA2 and FA's theta
                             theta = c(-log(0.8), -log(0.8), -log(0.8)),
                             # IA1, IA2 and FA's information fraction
                             t = x_gsd$timing[1:3],
                             # IA1, IA2 and FA's statistical information
                             info = x_gsd$n.I[1:3] / 4,
                             # IA2 and FA's futility bound
                             a = c(-Inf, -Inf),
                             # IA2 and FA's efficacy bound
                             b = x_gsd$upper$bound[2:3]
                             )
  # IA2's CP given IA1
  expect_equal(gsDesign_cp$upper$prob[1],
               gsDesign2_cp$prob_alpha[1])

  # FA's CP given IA1
  expect_equal(gsDesign_cp$upper$prob[2],
               gsDesign2_cp$prob_alpha[2])

  # --------------------------------------------------- #
  #               case 2                                #
  # currently at IA2, compute conditional power at FA   #
  # --------------------------------------------------- #
  gsDesign_cp <- gsCP(x_gsd, i = 2, zi = -qnorm(0.04), theta = -log(0.8)/2)

  gsDesign2_cp <- gs_cp_npe2(# IA2's Z-score
                             c = -qnorm(0.04),
                             # IA2 and FA's theta
                             theta = c(-log(0.8), -log(0.8)),
                             # IA2 and FA's information fraction
                             t = x_gsd$timing[2:3],
                             # IA2 and FA's statistical information
                             info = x_gsd$n.I[2:3] / 4,
                             # FA's futility bound
                             a = -Inf,
                             # FA's efficacy bound
                             b = x_gsd$upper$bound[3]
                             )

  # FA's CP given IA2
  expect_equal(gsDesign_cp$upper$prob[1],
               gsDesign2_cp$prob_alpha[1])
})
