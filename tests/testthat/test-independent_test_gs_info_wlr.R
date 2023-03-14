# weighted log rank test with 3 options of weights
test_that("Validate the function based on examples with individual functions", {
  # Enrollments
  enroll_rate <- tibble::tibble(stratum = "All", duration = 12, rate = 500 / 12)
  # fail_rate
  fail_rate <- tibble::tibble(
    stratum = "All",
    duration = c(4, 100),
    fail_rate = log(2) / 15, # Median survival 15 months
    hr = c(1, .6), # Delay effect after 4 months
    dropout_rate = 0.001
  )
  ## Randomization Ratio is 1:1
  ratio <- 1

  ## Type I error (one-sided)
  alpha <- 0.025

  ## Power (1 - beta)
  beta <- 0.2
  power <- 1 - beta

  # Interim Analysis Time
  analysis_time <- c(12, 24, 36)

  # create arms
  # Define study design object in each arm
  gs_arm <- gs_create_arm(
    enroll_rate,
    fail_rate,
    ratio = 2, # Randomization ratio
    total_time = 36 # Total study duration
  )
  arm0 <- gs_arm[["arm0"]]
  arm1 <- gs_arm[["arm1"]]

  # FH(0,1) example
  weight <- function(x, arm0, arm1) {
    gsDesign2::wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 1)
  }
  gs_info <- gsDesign2::gs_info_wlr(
    enroll_rate, fail_rate, ratio,
    analysis_time = analysis_time,
    weight = weight
  )
  fh01 <- gs_info %>% dplyr::mutate_if(is.numeric, round, digits = 5)

  N01 <- sum(enroll_rate$rate * enroll_rate$duration)
  n0 <- N01 / 2
  n1 <- N01 / 2

  delta01 <- abs(sapply(analysis_time, function(x) {
    gsDesign2:::gs_delta_wlr(arm0, arm1, tmax = x, weight = weight)
  }))
  sigma201 <- abs(sapply(analysis_time, function(x) {
    gsDesign2:::gs_sigma2_wlr(arm0, arm1, tmax = x, weight = weight)
  }))

  info01 <- N01 * sigma201
  theta01 <- delta01 / sigma201

  evt01 <- gsDesign2:::prob_event.arm(arm0, tmax = analysis_time) * n0 +
    gsDesign2:::prob_event.arm(arm1, tmax = analysis_time) * n1
  # log_ahr <- sapply(analysis_time, function(t_k) {
  #   gsDesign2:::gs_delta_wlr(arm0, arm1, tmax = t_k, weight = weight) /
  #     gsDesign2:::gs_delta_wlr(
  #       arm0, arm1,
  #       tmax = t_k,
  #       weight = weight,
  #       approx = "generalized schoenfeld",
  #       normalization = TRUE
  #     )
  # })

  avehr <- gsDesign2::ahr(
    enroll_rate = enroll_rate, fail_rate = fail_rate, ratio = ratio,
    total_duration = analysis_time
  )

  # FH(0,1)
  expect_equal(object = as.numeric(fh01$n), expected = rep(N01, 3), tolerance = 1)
  expect_equal(object = as.numeric(fh01$event), expected = evt01, tolerance = 1)
  expect_equal(object = as.numeric(fh01$delta), expected = -delta01, tolerance = .01)
  expect_equal(object = as.numeric(fh01$sigma2), expected = sigma201, tolerance = .01)
  expect_equal(object = as.numeric(fh01$theta), expected = theta01, tolerance = .2)
  expect_equal(object = as.numeric(fh01$info), expected = info01, tolerance = 0.3)
})
