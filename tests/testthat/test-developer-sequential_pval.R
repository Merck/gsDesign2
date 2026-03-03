test_that("Comparision with gsDesign::sequentialPValue", {
  alpha <- 0.025
  beta <- 0.1
  ratio <- 1

  # Enrollment
  enroll_rate <- gsDesign2::define_enroll_rate(
    duration = c(2, 2, 10),
    rate = (1:3) / 3)

  # Failure and dropout
  fail_rate <- gsDesign2::define_fail_rate(
    duration = Inf, fail_rate = log(2) / 9,
    hr = 0.6, dropout_rate = .0001)

  # IA and FA analysis time
  study_duration <- 36

  # Randomization ratio
  ratio <- 1

  # Spending
  upper <- gsDesign2::gs_spending_bound
  lower <- gsDesign2::gs_b
  upar <- list(sf = "sfLDOF", total_spend = alpha)
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
                  S = NULL, T = study_duration,
                  minfup = study_duration - sum(enroll_rate$duration),
                  ratio = ratio)

  # ------------------------------ #
  # original design of gsDesign2  #
  # ------------------------------ #
  x_gsd2 <- gs_design_ahr(enroll_rate = enroll_rate, fail_rate = fail_rate,
                          alpha = alpha, beta = beta, info_frac = 1:3/3, ratio = ratio,
                          upper = upper, upar = upar,
                          lower = lower, lpar = lpar,
                          analysis_time = study_duration)

  seq_pva_gsd <- sequentialPValue(x_gsd, n.I = c(40, 120), Z = c(1.5,2))
  seq_pva_gsd2 <- sequential_pval(x_gsd2, event = c(40, 120) * (x_gsd2$analysis$event[3] / x_gsd$n.I[3]), z = c(1.5, 2))

  expect_equal(seq_pva_gsd, seq_pva_gsd2)
})
