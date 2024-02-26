# Test get_combo_weight ----
test_that("get_combo_weight output correct rho1", {
  res <- test_get_combo_weight()
  expect_equal(res$weight1_rho, "rho =1")
})

test_that("get_combo_weight output correct rho2", {
  res <- test_get_combo_weight()
  expect_equal(res$weight2_rho, "rho =1")
})

test_that("get_combo_weight output correct gamma1", {
  res <- test_get_combo_weight()
  expect_equal(res$weight1_gamma, "gamma =0")
})

test_that("get_combo_weight output correct gamma2", {
  res <- test_get_combo_weight()
  expect_equal(res$weight2_gamma, "gamma =1")
})

# Test get_combo_weight tau not equal to -1 ----
test_that("get_combo_weight output correct tau1", {
  res <- test_get_combo_weight_tau()
  expect_equal(res$weight1_tau, "tau =1")
})

test_that("get_combo_weight output correct tau3", {
  res <- test_get_combo_weight_tau()
  expect_equal(res$weight3_tau, "tau =0")
})

# Test gs_delta_combo ----
test_that("gs_delta_combo correctly use gs_delta_wlr 1", {
  res <- test_gs_delta_combo()
  rho <- res$rho
  gamma <- res$gamma
  tau <- res$tau
  arm <- res$arm
  delta <- res$delta

  for (i in 1:4) {
    weight_test1 <- gsDesign2:::get_combo_weight(rho[i], gamma[i], tau[i])
    delta_test1 <- gsDesign2:::gs_delta_wlr(
      arm0 = arm$arm0, arm1 = arm$arm1,
      tmax = 30, weight = eval(parse(text = weight_test1)),
      approx = "asymptotic", normalization = FALSE
    )

    expect_identical(delta[i], delta_test1)
  }
})

# Test gs_sigma2_combo ----
test_that("gs_sigma2_combo correctly use gs_sigma2_wlr 1", {
  res <- test_gs_sigma2_combo()
  rho1 <- res$rho1
  gamma1 <- res$gamma1
  tau <- res$tau
  arm <- res$arm
  sigma2 <- res$sigma2

  for (i in 1:4) {
    for (j in 1:4) {
      weight_test_ij <- gsDesign2:::get_combo_weight(rho1[i, j], gamma1[i, j], tau[i])
      sigma_ij <- gsDesign2:::gs_sigma2_wlr(
        arm0 = arm$arm0, arm1 = arm$arm1,
        tmax = 30, weight = eval(parse(text = weight_test_ij)),
        approx = "asymptotic"
      )

      expect_equal(sigma2[i, j], sigma_ij)
    }
  }
})

# Test gs_info_combo ----
test_that("gs_info_combo correctly use gs_info_wlr 1", {
  res <- test_gs_info_combo()
  rho <- res$rho
  gamma <- res$gamma
  tau <- res$tau
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  info_combo <- res$info_combo

  for (i in 1:4) {
    weight_test_i <- gsDesign2:::get_combo_weight(rho[i], gamma[i], tau[i])
    info_wlr <- gsDesign2::gs_info_wlr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      ratio = 1, # Experimental:Control randomization ratio
      event = NULL, # Events at analyses
      analysis_time = 30, # Times of analyses
      weight = eval(parse(text = weight_test_i)),
      approx = "asymptotic"
    )

    expect_equal(info_combo$info[i], info_wlr$info[1])
  }
})

# Test gs_prob_combo ----
## 1 analysis scenario ----
test_that("p efficacy", {
  res <- test_gs_prob_combo_1()
  prob <- res$prob
  p_efficacy <- res$p_efficacy

  expect_equal(prob$probability[1], p_efficacy[1], tolerance = 0.001)
})

test_that("p futility", {
  res <- test_gs_prob_combo_1()
  prob <- res$prob
  p_futility <- res$p_futility

  expect_equal(prob$probability[2], p_futility[1], tolerance = 0.001)
})

## 2 analysis scenario ----
test_that("p efficacy1", {
  res <- test_gs_prob_combo_2()
  prob <- res$prob
  p_efficacy_1 <- res$p_efficacy_1

  expect_equal(prob$probability[1], p_efficacy_1[1], tolerance = 0.001)
})

test_that("p futility1", {
  res <- test_gs_prob_combo_2()
  prob <- res$prob
  p_futility_1 <- res$p_futility_1

  expect_equal(prob$probability[3], p_futility_1[1], tolerance = 0.001)
})

test_that("p efficacy2", {
  res <- test_gs_prob_combo_2()
  prob <- res$prob
  p_efficacy_1 <- res$p_efficacy_1
  p_efficacy_2 <- res$p_efficacy_2

  expect_equal(prob$probability[2], p_efficacy_1[1] + p_efficacy_2[1], tolerance = 0.001)
})

test_that("p futility2", {
  res <- test_gs_prob_combo_2()
  prob <- res$prob
  p_futility_1 <- res$p_futility_1
  p_futility_2 <- res$p_futility_2

  expect_equal(prob$probability[4], p_futility_1[1] + p_futility_2[1], tolerance = 0.001)
})

# Test pmvnorm_combo ----
test_that("pmvnorm_combo calculate p for One test for all group or lower bound is -Inf.", {
  res <- test_pmvnorm_combo()
  p <- res$p
  p_test <- res$p_test

  expect_equal(p[1], p_test[1], tolerance = 0.001)
})

# Log-rank multiple analysis ----
## Test gs_utility_combo ----
test_that("gs_utility_combo output correct info as gs_info_combo", {
  res <- test_gs_utility_combo()
  utility_combo <- res$utility_combo
  info_combo_test <- res$info_combo_test

  expect_equal(utility_combo$info[1:11], info_combo_test[1:11])
})

test_that("gs_utility_combo output correct theta effect as gs_info_combo", {
  res <- test_gs_utility_combo()
  utility_combo <- res$utility_combo
  info_combo_test <- res$info_combo_test
  theta_test <- (-info_combo_test$delta) / sqrt(info_combo_test$sigma2)

  expect_equal(utility_combo$theta, theta_test)
})

test_that("gs_utility_combo output correct correlation matrix as gs_info_combo", {
  res <- test_gs_utility_combo()
  n_analysis <- res$n_analysis
  utility_combo <- res$utility_combo
  info_combo_test <- res$info_combo_test

  info <- info_combo_test[[10]]
  cov <- matrix(0, n_analysis, n_analysis)
  for (i in 1:n_analysis) {
    for (j in 1:n_analysis) {
      k <- min(i, j)
      cov[i, j] <- info[k] / (info[i] * info[j])
    }
  }
  corr_test <- cov2cor(cov)

  expect_equal(utility_combo$corr, corr_test)
})

## Multiple test analysis ----
test_that("gs_utility_combo output correct info as gs_info_combo", {
  res <- test_gs_utility_combo_multiple()
  utility_combo <- res$utility_combo
  info_combo_test <- res$info_combo_test

  expect_equal(utility_combo$info[1:11], info_combo_test[1:11])
})

test_that("gs_utility_combo output correct theta effect as gs_info_combo", {
  res <- test_gs_utility_combo_multiple()
  utility_combo <- res$utility_combo
  info_combo_test <- res$info_combo_test
  theta_test <- (-info_combo_test$delta) / sqrt(info_combo_test$sigma2)

  expect_equal(utility_combo$theta, theta_test)
})

test_that("gs_utility_combo output correct correlation matrix as gs_info_combo", {
  res <- test_gs_utility_combo_multiple()
  rho <- res$rho
  gamma <- res$gamma
  tau <- res$tau
  analysis_time <- res$analysis_time
  n_analysis <- res$n_analysis
  gs_arm <- res$gs_arm
  utility_combo <- res$utility_combo

  sigma2 <- gsDesign2:::gs_sigma2_combo(
    arm0 = gs_arm$arm0,
    arm1 = gs_arm$arm1,
    tmax = analysis_time,
    rho = rho,
    gamma = gamma,
    tau = tau
  )
  corr_test <- cov2cor(sigma2)

  expect_equal(utility_combo$corr, corr_test)
})
