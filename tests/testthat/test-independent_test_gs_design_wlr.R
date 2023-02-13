#### NOTE: all reference numbers come from simulation results in
### https://keaven.github.io/gsd-deming/wlr.html#wlr
### please look for sections titled "Simulation results based on 10,000 replications."

test_that("Validate the function based on examples with simulation results", {
  x <- gsDesign::gsSurv(
    k = 3, test.type = 4, alpha = 0.025,
    beta = 0.1, astar = 0, timing = c(1),
    sfu = gsDesign::sfLDOF, sfupar = c(0),
    sfl = gsDesign::sfLDOF, sflpar = c(0),
    lambdaC = c(0.1),
    hr = 0.6, hr0 = 1, eta = 0.01,
    gamma = c(10),
    R = c(12), S = NULL,
    T = 36, minfup = 24, ratio = 1
  )
  enroll_rate <- tibble::tibble(
    stratum = "All",
    duration = 12,
    rate = 500 / 12
  )
  fail_rate <- tibble::tibble(
    stratum = "All",
    duration = c(4, 100),
    fail_rate = log(2) / 15, # Median survival 15 month
    hr = c(1, 0.6),
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
  # logrank test
  lrk <- gsDesign2::gs_design_wlr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    weight = function(x, arm0, arm1) {
      gsDesign2::wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 0)
    },
    ratio = ratio, alpha = alpha, beta = beta,
    upar = x$upper$bound,
    lpar = x$lower$bound,
    analysis_time = c(12, 24, 36)
  )
  lrk_bnd <-
    lrk$bounds %>%
    dplyr::mutate_if(is.numeric, round, digits = 2) %>%
    dplyr::select(analysis, bound, probability) %>%
    tidyr::pivot_wider(names_from = bound, values_from = probability)

  # FH(0,1)
  fh01 <- gsDesign2::gs_design_wlr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    weight = function(x, arm0, arm1) {
      gsDesign2::wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 1)
    },
    ratio = ratio, alpha = alpha, beta = beta,
    upar = x$upper$bound,
    lpar = x$lower$bound,
    analysis_time = analysis_time
  )
  fh01_bnd <-
    fh01$bounds %>%
    dplyr::mutate_if(is.numeric, round, digits = 2) %>%
    dplyr::select(analysis, bound, probability) %>%
    tidyr::pivot_wider(names_from = bound, values_from = probability)

  # FH(0,0.5)
  fh0d5 <- gsDesign2::gs_design_wlr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    weight = function(x, arm0, arm1) {
      gsDesign2::wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 0.5)
    },
    ratio = ratio, alpha = alpha, beta = beta,
    upar = x$upper$bound,
    lpar = x$lower$bound,
    analysis_time = analysis_time
  )
  fh0d5_bnd <-
    fh0d5$bounds %>%
    dplyr::mutate_if(is.numeric, round, digits = 2) %>%
    dplyr::select(analysis, bound, probability) %>%
    tidyr::pivot_wider(names_from = bound, values_from = probability)

  # FH(0.5,0.5)
  fh5d5 <- gsDesign2::gs_design_wlr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    weight = function(x, arm0, arm1) {
      gsDesign2::wlr_weight_fh(x, arm0, arm1, rho = 0.5, gamma = 0.5)
    },
    ratio = ratio, alpha = alpha, beta = beta,
    upar = x$upper$bound,
    lpar = x$lower$bound,
    analysis_time = analysis_time
  )
  fh5d5_bnd <-
    fh5d5$bounds %>%
    dplyr::mutate_if(is.numeric, round, digits = 2) %>%
    dplyr::select(analysis, bound, probability) %>%
    tidyr::pivot_wider(names_from = bound, values_from = probability)

  # logrank part
  expect_equal(object = as.numeric(lrk$analysis$n), expected = rep(386, 3), tolerance = 3)
  expect_equal(object = as.numeric(lrk$analysis$event), expected = c(82.77, 190.05, 255.61), tolerance = 3)
  expect_equal(object = as.numeric(lrk$analysis$ahr), expected = c(0.87, 0.72, 0.69), tolerance = 0.3)
  expect_equal(object = as.numeric(lrk_bnd$upper), expected = c(0.00, 0.41, 0.80), tolerance = 0.3)
  expect_equal(object = as.numeric(lrk_bnd$lower), expected = c(0.07, 0.14, 0.20), tolerance = 0.3)
  # fh(0,1)
  expect_equal(object = as.numeric(fh01$analysis$n), expected = rep(317, 3), tolerance = 3)
  expect_equal(object = as.numeric(fh01$analysis$event), expected = c(68.01, 156.13, 210.06), tolerance = 3)
  expect_equal(object = as.numeric(fh01$analysis$ahr), expected = c(0.76, 0.65, 0.63), tolerance = 0.3)
  expect_equal(object = as.numeric(fh01_bnd$upper), expected = c(0.00, 0.45, 0.79), tolerance = 0.3)
  expect_equal(object = as.numeric(fh01_bnd$lower), expected = c(0.04, 0.12, 0.21), tolerance = 0.3)
  # fh(0,0.5)
  expect_equal(object = as.numeric(fh0d5$analysis$n), expected = rep(314, 3), tolerance = 3)
  expect_equal(object = as.numeric(fh0d5$analysis$event), expected = c(67.21, 154.43, 207.92), tolerance = 3)
  expect_equal(object = as.numeric(fh0d5$analysis$ahr), expected = c(0.81, 0.67, 0.65), tolerance = 0.3)
  expect_equal(object = as.numeric(fh0d5_bnd$upper), expected = c(0.00, 0.44, 0.79), tolerance = 0.3)
  expect_equal(object = as.numeric(fh0d5_bnd$lower), expected = c(0.05, 0.12, 0.21), tolerance = 0.3)
  # fh(0.5,0.5)
  expect_equal(object = as.numeric(fh5d5$analysis$n), expected = rep(317, 3), tolerance = 3)
  expect_equal(object = as.numeric(fh5d5$analysis$event), expected = c(67.87, 155.86, 209.82), tolerance = 3)
  expect_equal(object = as.numeric(fh5d5$analysis$ahr), expected = c(0.81, 0.68, 0.66), tolerance = 0.3)
  expect_equal(object = as.numeric(fh5d5_bnd$upper), expected = c(0.00, 0.43, 0.80), tolerance = 0.3)
  expect_equal(object = as.numeric(fh5d5_bnd$lower), expected = c(0.06, 0.12, 0.20), tolerance = 0.3)
})
