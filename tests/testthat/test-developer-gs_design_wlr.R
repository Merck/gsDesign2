enroll_rate <- define_enroll_rate(duration = 12, rate = 1)

fail_rate <- define_fail_rate(duration = c(4, 100),
                              fail_rate = log(2) / 15,
                              hr = c(1, .6),
                              dropout_rate = 0.001)
ratio <-  1
weight <- function(x, arm0, arm1) {wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 0.5)}
upper <- gs_spending_bound
upar <- list(sf = gsDesign::sfLDOF, total_spend = 0.025)
lower <- gs_b
lpar <- rep(-Inf, 3)

test_that("Validate info-frac driven design with a known study duration",{
  # output from gsDesign2
  x <- gs_design_wlr(enroll_rate = enroll_rate, fail_rate = fail_rate,
                     ratio = ratio, alpha = 0.025, beta = 0.2, weight = weight,
                     upper = upper, upar = upar,
                     lower = lower, lpar = lpar,
                     analysis_time = 36, info_frac = c(0.3, 0.7, 1))


  # validate the info frac
  expect_equal(x$analysis$info_frac, c(0.3, 0.7, 1), tolerance = 1e-6)
  # validate the final analysis time
  expect_equal(max(x$analysis$time), 36)
})

test_that("Validate calendar-time driven design",{
  # output from gsDesign2
  x <- gs_design_wlr(enroll_rate = enroll_rate, fail_rate = fail_rate,
                     ratio = ratio, alpha = 0.025, beta = 0.2, weight = weight,
                     upper = upper, upar = upar,
                     lower = lower, lpar = lpar,
                     analysis_time = c(12, 24, 36), info_frac = NULL)

  # validate the analysis time
  expect_equal(x$analysis$time, c(12, 24, 36))
})

test_that("Validate calendar-time and info-frac driven design -- A",{
  analysis_time <- c(12, 24, 36)
  info_frac <- c(0.3, 0.7, 1)
  # output from gsDesign2
  x <- gs_design_wlr(enroll_rate = enroll_rate, fail_rate = fail_rate,
                     ratio = ratio, alpha = 0.025, beta = 0.2, weight = weight,
                     upper = upper, upar = upar,
                     lower = lower, lpar = lpar,
                     analysis_time = analysis_time, info_frac = info_frac)

  # validate the analysis time and info_frac
  diff_time <- x$analysis$time - analysis_time
  diff_info_frac <- x$analysis$info_frac - info_frac

  diff_time <- ifelse(abs(diff_time) >= 0.5, diff_time, 0)
  diff_info_frac <- ifelse(abs(diff_info_frac) >= 1e-5, diff_info_frac, 0)

  idx_driven_by_time <- which(diff_time == 0)
  idx_driven_by_info_frac <- which(diff_info_frac == 0)

  expect_equal(x$analysis$time[idx_driven_by_time], analysis_time[idx_driven_by_time])
  expect_equal(x$analysis$info_frac[idx_driven_by_info_frac], info_frac[idx_driven_by_info_frac], tolerance = 1e-5)
})

test_that("Validate calendar-time and info-frac driven design -- B",{
  analysis_time <- c(18, 29, 36)
  info_frac <- c(0.3, 0.7, 1)
  # output from gsDesign2
  x <- gs_design_wlr(enroll_rate = enroll_rate, fail_rate = fail_rate,
                     ratio = ratio, alpha = 0.025, beta = 0.2, weight = weight,
                     upper = upper, upar = upar,
                     lower = lower, lpar = lpar,
                     analysis_time = analysis_time, info_frac = info_frac)

  # validate the analysis time and info_frac
  diff_time <- x$analysis$time - analysis_time
  diff_info_frac <- x$analysis$info_frac - info_frac

  diff_time <- ifelse(abs(diff_time) >= 0.5, diff_time, 0)
  diff_info_frac <- ifelse(abs(diff_info_frac) >= 1e-5, diff_info_frac, 0)

  idx_driven_by_time <- which(diff_time == 0)
  idx_driven_by_info_frac <- which(diff_info_frac == 0)

  expect_equal(x$analysis$time[idx_driven_by_time], analysis_time[idx_driven_by_time])
  expect_equal(x$analysis$info_frac[idx_driven_by_info_frac], info_frac[idx_driven_by_info_frac], tolerance = 1e-5)
})

test_that("Validate calendar-time and info-frac driven design -- C",{
  analysis_time <- c(18, 24, 36)
  info_frac <- c(0.3, 0.7, 1)
  # output from gsDesign2
  x <- gs_design_wlr(enroll_rate = enroll_rate, fail_rate = fail_rate,
                     ratio = ratio, alpha = 0.025, beta = 0.2, weight = weight,
                     upper = upper, upar = upar,
                     lower = lower, lpar = lpar,
                     analysis_time = analysis_time, info_frac = info_frac)

  # validate the analysis time and info_frac
  diff_time <- x$analysis$time - analysis_time
  diff_info_frac <- x$analysis$info_frac - info_frac

  diff_time <- ifelse(abs(diff_time) >= 0.5, diff_time, 0)
  diff_info_frac <- ifelse(abs(diff_info_frac) >= 1e-5, diff_info_frac, 0)

  idx_driven_by_time <- which(diff_time == 0)
  idx_driven_by_info_frac <- which(diff_info_frac == 0)

  expect_equal(x$analysis$time[idx_driven_by_time], analysis_time[idx_driven_by_time])
  expect_equal(x$analysis$info_frac[idx_driven_by_info_frac], info_frac[idx_driven_by_info_frac], tolerance = 1e-5)
})

test_that("Validate if the output info-frac match the planned info-frac, when the design is only driven by info frac", {
  x <- gs_design_wlr(
    alpha = 0.025,
    beta = 0.9,
    enroll_rate = define_enroll_rate(duration = 12, rate = 1),
    fail_rate = define_fail_rate(duration = c(4, Inf), fail_rate = log(2) / 10,
                                 hr = c(1, 0.6), dropout_rate = 0.001),
    ratio = 1,
    info_frac = c(0.75, 1),
    analysis_time = 36,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_b,
    lpar = rep(-Inf, 2),
    info_scale = "h0_info",
    weight = function(x, arm0, arm1) {wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 0)}
  )

  expect_equal(x$analysis$info_frac[1], 0.75, tolerance = 1e-6)
})


test_that("Validate if the output info-frac match the planned info-frac, when the design is driven by both info frac and analysis time", {
  x <- gs_design_wlr(
    alpha = 0.025,
    beta = 0.9,
    enroll_rate = define_enroll_rate(duration = 12, rate = 1),
    fail_rate = define_fail_rate(duration = c(4, Inf), fail_rate = log(2) / 10,
                                 hr = c(1, 0.6), dropout_rate = 0.001),
    ratio = 1,
    info_frac = c(0.75, 1),
    analysis_time = c(10, 36),
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_b,
    lpar = rep(-Inf, 2),
    info_scale = "h0_info",
    weight = function(x, arm0, arm1) {wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 0)}
  )

  expect_equal(x$analysis$info_frac[1], 0.75, tolerance = 1e-6)
})
