# Helper functions used by test-independent-utility_combo.R

test_get_combo_weight <- function() {
  rho <- c(1, 1, 0, 0)
  gamma <- c(0, 1, 0, 1)
  tau <- c(-1, -1, -1, -1)

  weight <- gsDesign2:::get_combo_weight(rho, gamma, tau)
  weight1_rho <- substring(weight[[1]], 125, 130)
  weight2_rho <- substring(weight[[2]], 125, 130)
  weight1_gamma <- substring(weight[[1]], 133, 140)
  weight2_gamma <- substring(weight[[2]], 133, 140)
  weight1_tau <- substring(weight[[1]], 143, 148)
  weight2_tau <- substring(weight[[2]], 143, 148)

  list(
    "weight1_rho" = weight1_rho,
    "weight2_rho" = weight2_rho,
    "weight1_gamma" = weight1_gamma,
    "weight2_gamma" = weight2_gamma
  )
}

test_get_combo_weight_tau <- function() {
  rho <- c(1, 1, 0, 0)
  gamma <- c(0, 1, 0, 1)
  tau <- c(1, 1, 0, 0)
  weight <- gsDesign2:::get_combo_weight(rho, gamma, tau)
  weight1_tau <- substring(weight[[1]], 143, 148)
  weight3_tau <- substring(weight[[3]], 143, 148)

  list(
    "weight1_tau" = weight1_tau,
    "weight3_tau" = weight3_tau
  )
}

test_gs_delta_combo <- function() {
  rho <- c(1, 1, 0, 0)
  gamma <- c(0, 1, 0, 1)
  tau <- c(-1, -1, -1, -1)

  enroll_rate <- define_enroll_rate(
    duration = c(2, 2, 30),
    rate = c(3, 6, 9)
  )
  fail_rate <- define_fail_rate(
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    dropout_rate = rep(.001, 2),
    hr = c(.9, .6)
  )
  arm <- gs_create_arm(enroll_rate, fail_rate, ratio = 1, total_time = 1e6)
  delta <- gsDesign2:::gs_delta_combo(
    arm0 = arm$arm0, arm1 = arm$arm1,
    tmax = 30, rho = rho, gamma = gamma, tau = rep(-1, length(rho)),
    approx = "asymptotic", normalization = FALSE
  )

  list(
    "rho" = rho,
    "gamma" = gamma,
    "tau" = tau,
    "arm" = arm,
    "delta" = delta
  )
}

test_gs_sigma2_combo <- function() {
  rho <- c(1, 1, 0, 0)
  gamma <- c(0, 1, 0, 1)
  tau <- c(-1, -1, -1, -1)

  enroll_rate <- define_enroll_rate(
    duration = c(2, 2, 30),
    rate = c(3, 6, 9)
  )
  fail_rate <- define_fail_rate(
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    dropout_rate = rep(.001, 2),
    hr = c(.9, .6)
  )
  arm <- gs_create_arm(enroll_rate, fail_rate, ratio = 1, total_time = 1e6)

  sigma2 <- gsDesign2:::gs_sigma2_combo(
    arm0 = arm$arm0, arm1 = arm$arm1, tmax = 30,
    rho = rho, gamma = gamma, tau = rep(-1, length(rho)),
    approx = "asymptotic"
  )
  rho1 <- outer(rho, rho, function(x, y) (x + y) / 2)
  gamma1 <- outer(gamma, gamma, function(x, y) (x + y) / 2)

  list(
    "rho1" = rho1,
    "gamma1" = gamma1,
    "tau" = tau,
    "arm" = arm,
    "sigma2" = sigma2
  )
}

test_gs_info_combo <- function() {
  rho <- c(1, 1, 0, 0)
  gamma <- c(0, 1, 0, 1)
  tau <- c(-1, -1, -1, -1)

  enroll_rate <- define_enroll_rate(
    duration = c(2, 2, 30),
    rate = c(3, 6, 9)
  )
  fail_rate <- define_fail_rate(
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    dropout_rate = rep(.001, 2),
    hr = c(.9, .6)
  )

  info_combo <- gsDesign2:::gs_info_combo(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = 1, # Experimental:Control randomization ratio
    event = NULL, # Events at analyses
    analysis_time = 30, # Times of analyses
    rho = rho,
    gamma = gamma,
    tau = rep(-1, length(rho)),
    approx = "asymptotic"
  )

  list(
    "rho" = rho,
    "gamma" = gamma,
    "tau" = tau,
    "enroll_rate" = enroll_rate,
    "fail_rate" = fail_rate,
    "info_combo" = info_combo
  )
}

test_gs_prob_combo_1 <- function() {
  lower <- -0.6
  upper <- 0.4
  rho <- c(1, 1, 0, 0)
  gamma <- c(0, 1, 0, 1)
  tau <- c(-1, -1, -1, -1)
  enroll_rate <- define_enroll_rate(
    duration = c(2, 2, 30),
    rate = c(3, 6, 9)
  )
  fail_rate <- define_fail_rate(
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    dropout_rate = rep(.001, 2),
    hr = c(.9, .6)
  )
  arm <- gs_create_arm(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = 1,
    total_time = 1e6
  )
  sigma <- gsDesign2:::gs_sigma2_combo(
    arm0 = arm$arm0,
    arm1 = arm$arm1,
    tmax = 30,
    rho = rho,
    gamma = gamma,
    tau = rep(-1, length(rho)),
    approx = "asymptotic"
  )
  corr <- cov2cor(sigma)
  n_test <- length(rho)
  theta <- rep(0, n_test)
  analysis <- 1

  prob <- gsDesign2:::gs_prob_combo(
    lower_bound = rep(lower, n_test),
    upper_bound = rep(upper, n_test),
    analysis = analysis,
    theta = theta,
    corr = corr,
    algorithm = GenzBretz(maxpts = 1e5, abseps = 1e-5)
  )
  p_efficacy <- gsDesign2:::pmvnorm_combo(
    lower = rep(upper, n_test),
    upper = rep(Inf, n_test),
    group = analysis,
    mean = theta,
    corr = corr
  )
  p_futility <- gsDesign2:::pmvnorm_combo(
    lower = rep(-Inf, n_test),
    upper = rep(lower, n_test),
    group = analysis,
    mean = theta,
    corr = corr
  )

  list("prob" = prob, "p_efficacy" = p_efficacy, "p_futility" = p_futility)
}

test_gs_prob_combo_2 <- function() {
  lower <- c(-0.2, -0.3)
  upper <- c(0.3, 0.4)
  rho <- c(1, 1, 0, 0)
  gamma <- c(0, 1, 0, 1)
  tau <- c(-1, -1, -1, -1)
  enroll_rate <- define_enroll_rate(
    duration = c(2, 2, 30),
    rate = c(3, 6, 9)
  )
  fail_rate <- define_fail_rate(
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    dropout_rate = rep(.001, 2),
    hr = c(.9, .6)
  )
  arm <- gs_create_arm(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = 1,
    total_time = 1e6
  )
  sigma <- gsDesign2:::gs_sigma2_combo(
    arm0 = arm$arm0,
    arm1 = arm$arm1,
    tmax = 30,
    rho = rho,
    gamma = gamma,
    tau = rep(-1, length(rho)),
    approx = "asymptotic"
  )
  corr <- cov2cor(sigma)
  n_test <- length(rho)
  theta <- rep(0, n_test)
  analysis <- c(1, 2)
  prob <- gsDesign2:::gs_prob_combo(
    lower_bound = rep(lower, n_test),
    upper_bound = rep(upper, n_test),
    analysis = analysis,
    theta = theta,
    corr = corr,
    algorithm = GenzBretz(maxpts = 1e5, abseps = 1e-5)
  )
  c <- c(1, 3)
  corr1 <- corr[c, c]
  p_efficacy_1 <- gsDesign2:::pmvnorm_combo(
    lower = rep(upper[1], 2),
    upper = rep(Inf, 2),
    group = 1,
    mean = theta[c],
    corr = corr1
  )
  p_futility_1 <- gsDesign2:::pmvnorm_combo(
    lower = rep(-Inf, 2),
    upper = rep(lower[1], 2),
    group = 1,
    mean = theta[c],
    corr = corr1
  )
  p_efficacy_2 <- gsDesign2:::pmvnorm_combo(
    lower = c(lower[1], upper[2]),
    upper = c(upper[1], Inf),
    group = analysis,
    mean = theta,
    corr = corr
  )
  p_futility_2 <- gsDesign2:::pmvnorm_combo(
    lower = c(lower[1], -Inf),
    upper = c(upper[1], lower[2]),
    group = analysis,
    mean = theta,
    corr = corr
  )

  list(
    "prob" = prob,
    "p_efficacy_1" = p_efficacy_1,
    "p_efficacy_2" = p_efficacy_2,
    "p_futility_1" = p_futility_1,
    "p_futility_2" = p_futility_2
  )
}

test_pmvnorm_combo <- function() {
  lower <- -Inf
  upper <- 0
  mean <- 0.3
  n_test <- 4
  rho <- c(1, 1, 0, 0)
  gamma <- c(0, 1, 0, 1)
  tau <- c(-1, -1, -1, -1)

  enroll_rate <- define_enroll_rate(
    stratum = "All",
    duration = c(2, 2, 10),
    rate = c(3, 6, 9)
  )
  fail_rate <- define_fail_rate(
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    dropout_rate = rep(.001, 2),
    hr = c(.9, .6)
  )
  arm <- gs_create_arm(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = 1,
    total_time = 1e6
  )
  sigma <- gsDesign2:::gs_sigma2_combo(
    arm0 = arm$arm0,
    arm1 = arm$arm1,
    tmax = 30,
    rho = rho,
    gamma = gamma,
    tau = rep(-1, length(rho)),
    approx = "asymptotic"
  )
  corr <- cov2cor(sigma)

  p <- gsDesign2:::pmvnorm_combo(
    lower = rep(lower, n_test),
    upper = rep(upper, n_test),
    group = 2,
    mean = rep(mean, n_test),
    corr = corr,
    algorithm = mvtnorm::GenzBretz(maxpts = 1e5, abseps = 1e-5)
  )

  p_test <- mvtnorm::pmvnorm(
    lower = rep(lower, n_test),
    upper = rep(upper, n_test),
    mean = rep(mean, n_test),
    corr = corr,
    sigma = NULL,
    algorithm = mvtnorm::GenzBretz(maxpts = 1e5, abseps = 1e-5)
  )

  list("p" = p, "p_test" = p_test)
}

test_gs_utility_combo <- function() {
  enroll_rate <- define_enroll_rate(
    duration = c(2, 2, 30),
    rate = c(3, 6, 9)
  )
  fail_rate <- define_fail_rate(
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    dropout_rate = rep(.001, 2),
    hr = c(.9, .6)
  )
  analysis_time <- c(12, 24, 36)
  n_analysis <- length(analysis_time)
  fh_test <- rbind(data.frame(
    rho = 0,
    gamma = 0,
    tau = -1,
    test = 1,
    analysis = 1:3,
    analysis_time = analysis_time
  ))
  gs_arm <- gs_create_arm(
    enroll_rate,
    fail_rate,
    ratio = 1, # Randomization ratio
    total_time = max(analysis_time) # Total study duration
  )

  utility_combo <- gsDesign2:::gs_utility_combo(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    fh_test = fh_test,
    ratio = 1,
    algorithm = GenzBretz(maxpts = 1e5, abseps = 1e-5)
  )

  info_combo_test <- gsDesign2:::gs_info_combo(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = 1,
    analysis_time = analysis_time,
    rho = 0,
    gamma = 0
  )

  list(
    "n_analysis" = n_analysis,
    "utility_combo" = utility_combo,
    "info_combo_test" = info_combo_test
  )
}

test_gs_utility_combo_multiple <- function() {
  enroll_rate <- define_enroll_rate(
    duration = c(2, 2, 30),
    rate = c(3, 6, 9)
  )
  fail_rate <- define_fail_rate(
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    dropout_rate = rep(.001, 2),
    hr = c(.9, .6)
  )
  analysis_time <- 36
  n_analysis <- length(analysis_time)
  rho <- c(0, 0.5, 1)
  gamma <- c(0.5, 0.5, 0.5)
  tau <- c(-1, -1, -1)
  fh_test <- rbind(data.frame(
    rho = rho,
    gamma = gamma,
    tau = tau,
    test = 1:3,
    analysis = 1,
    analysis_time = analysis_time
  ))
  gs_arm <- gs_create_arm(
    enroll_rate,
    fail_rate,
    ratio = 1, # Randomization ratio
    total_time = max(analysis_time) # Total study duration
  )

  utility_combo <- gsDesign2:::gs_utility_combo(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    fh_test = fh_test,
    ratio = 1,
    algorithm = GenzBretz(maxpts = 1e5, abseps = 1e-5)
  )

  info_combo_test <- gsDesign2:::gs_info_combo(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = 1,
    analysis_time = analysis_time,
    rho = rho,
    gamma = gamma
  )

  list(
    "rho" = rho,
    "gamma" = gamma,
    "tau" = tau,
    "analysis_time" = analysis_time,
    "n_analysis" = n_analysis,
    "gs_arm" = gs_arm,
    "utility_combo" = utility_combo,
    "info_combo_test" = info_combo_test
  )
}
