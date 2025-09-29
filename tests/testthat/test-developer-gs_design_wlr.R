enroll_rate <- define_enroll_rate(duration = 12, rate = 1)

fail_rate <- define_fail_rate(duration = c(4, 100),
                              fail_rate = log(2) / 15,
                              hr = c(1, .6),
                              dropout_rate = 0.001)
ratio <-  1
weight <- list(method = "fh", param = list(rho = 0, gamma = 0.5))
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
  expect_equal(x$analysis$info_frac0, c(0.3, 0.7, 1), tolerance = 5e-5)
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
  x1 <- gs_design_wlr(
    alpha = 0.025,
    beta = 0.9,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = ratio,
    info_frac = c(0.75, 1),
    analysis_time = 36,
    upper = upper,
    upar = upar,
    lower = lower,
    lpar = lpar,
    info_scale = "h0_info",
    weight = "logrank"
  )

  x2 <- gs_design_wlr(
    alpha = 0.025,
    beta = 0.9,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = ratio,
    info_frac = c(0.75, 1),
    analysis_time = 36,
    upper = upper,
    upar = upar,
    lower = lower,
    lpar = lpar,
    info_scale = "h0_h1_info",
    weight = "logrank"
  )

  x3 <- gs_design_wlr(
    alpha = 0.025,
    beta = 0.9,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = ratio,
    info_frac = c(0.75, 1),
    analysis_time = 36,
    upper = upper,
    upar = upar,
    lower = lower,
    lpar = lpar,
    info_scale = "h1_info",
    weight = "logrank"
  )

  expect_equal(x1$analysis$info_frac[1], 0.75, tolerance = 1e-6)
  expect_equal(x2$analysis$info_frac0[1], 0.75, tolerance = 1e-6)
  expect_equal(x3$analysis$info_frac[1], 0.75, tolerance = 1e-6)
})

test_that("Validate if the output info-frac match the planned info-frac, when the design is driven by both info frac and analysis time", {
  x1 <- gs_design_wlr(
    alpha = 0.025,
    beta = 0.9,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = ratio,
    info_frac = c(0.75, 1),
    analysis_time = c(1, 36),
    upper = upper,
    upar = upar,
    lower = lower,
    lpar = lpar,
    info_scale = "h0_info",
    weight = "logrank"
  )

  x2 <- gs_design_wlr(
    alpha = 0.025,
    beta = 0.9,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = ratio,
    info_frac = c(0.75, 1),
    analysis_time = c(1, 36),
    upper = upper,
    upar = upar,
    lower = lower,
    lpar = lpar,
    info_scale = "h0_h1_info",
    weight = "logrank"
  )

  x3 <- gs_design_wlr(
    alpha = 0.025,
    beta = 0.9,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = ratio,
    info_frac = c(0.75, 1),
    analysis_time = c(1, 36),
    upper = upper,
    upar = upar,
    lower = lower,
    lpar = lpar,
    info_scale = "h1_info",
    weight = "logrank"
  )

  expect_equal(x1$analysis$info_frac[1], 0.75, tolerance = 5e-6)
  expect_equal(x2$analysis$info_frac0[1], 0.75, tolerance = 5e-6)
  expect_equal(x3$analysis$info_frac[1], 0.75, tolerance = 5e-6)
})

test_that("Validate if WLR design under logrank test generates similar design as in AHR", {
  x1 <- gs_design_ahr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    alpha = 0.025,
    beta = 0.1,
    analysis_time = c(9, 27, 36),
    info_frac = NULL,
    ratio = ratio,
    binding = FALSE,
    upper = "gs_spending_bound",
    upar = list(sf = "sfLDOF", total_spend = 0.025, param = NULL),
    lower = "gs_spending_bound",
    lpar = list(sf = "sfLDOF", total_spend = 0.1, param = NULL),
    h1_spending = TRUE,
    info_scale = "h0_h1_info")

  x2 <- gs_design_wlr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    alpha = 0.025,
    beta = 0.1,
    analysis_time = c(9, 27, 36),
    info_frac = NULL,
    ratio = ratio,
    binding = FALSE,
    weight = "logrank",
    upper = "gs_spending_bound",
    upar = list(sf = "sfLDOF", total_spend = 0.025, param = NULL),
    lower = "gs_spending_bound",
    lpar = list(sf = "sfLDOF", total_spend = 0.1, param = NULL),
    h1_spending = TRUE,
    info_scale = "h0_h1_info")

  # sample size is approximately close
  expect_equal(x1$analysis$n, x2$analysis$n, tolerance = 1e-2)

  # events is approximately close
  expect_equal(x1$analysis$event, x2$analysis$event, tolerance = 1e-2)

  # ahr is approximately close
  expect_equal(x1$analysis$ahr, x2$analysis$ahr, tolerance = 1e-2)

  # theta is approximately close
  expect_equal(x1$analysis$theta, x2$analysis$theta, tolerance = 5e-2)

  # info is approximately close
  expect_equal(x1$analysis$info, x2$analysis$info, tolerance = 1e-2)

  # info0 is approximately close
  expect_equal(x1$analysis$info0, x2$analysis$info0, tolerance = 1e-2)

  # info_frac is approximately close
  expect_equal(x1$analysis$info_frac, x2$analysis$info_frac, tolerance = 1e-2)

  # info_frac0 is approximately close
  expect_equal(x1$analysis$info_frac0, x2$analysis$info_frac0, tolerance = 1e-2)

  # upper bound is approximately close
  expect_equal(x1$bound$z[x1$bound$bound == "upper"], x2$bound$z[x2$bound$bound == "upper"], tolerance = 1e-2)

  # lower bound is approximately close
  expect_equal(x1$bound$z[x1$bound$bound == "lower"], x2$bound$z[x2$bound$bound == "lower"], tolerance = 5e-2)

  # probability of crossing upper bound under H0 is approximately close
  expect_equal(x1$bound$probability0[x1$bound$bound == "upper"], x2$bound$probability0[x2$bound$bound == "upper"], tolerance = 1e-2)

  # probability of crossing lower bound under H0 is approximately close
  expect_equal(x1$bound$probability0[x1$bound$bound == "lower"], x2$bound$probability0[x2$bound$bound == "lower"], tolerance = 1e-2)

  # probability of crossing upper bound under H1 is approximately close
  expect_equal(x1$bound$probability[x1$bound$bound == "upper"], x2$bound$probability[x2$bound$bound == "upper"], tolerance = 1e-2)

  # probability of crossing lower bound under H1 is approximately close
  expect_equal(x1$bound$probability[x1$bound$bound == "lower"], x2$bound$probability[x2$bound$bound == "lower"], tolerance = 1e-2)

  # hr at upper bound is approximately close
  expect_equal(x1$bound$`~hr at bound`[x1$bound$bound == "upper"], x2$bound$`~hr at bound`[x2$bound$bound == "upper"], tolerance = 1e-2)

  # hr at lower bound is approximately close
  expect_equal(x1$bound$`~hr at bound`[x1$bound$bound == "lower"], x2$bound$`~hr at bound`[x2$bound$bound == "lower"], tolerance = 5e-2)

  # nominal p at upper bound is approximately close
  expect_equal(x1$bound$`nominal p`[x1$bound$bound == "upper"], x2$bound$`nominal p`[x2$bound$bound == "upper"], tolerance = 1e-2)

  # nominal p at lower bound is approximately close
  expect_equal(x1$bound$`nominal p`[x1$bound$bound == "upper"], x2$bound$`nominal p`[x2$bound$bound == "upper"], tolerance = 1e-2)

})
