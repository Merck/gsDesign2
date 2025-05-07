test_that("Check using gs_info_wlr and gs_power_npe", {
  enroll_rate <- define_enroll_rate(
    duration = 12,
    rate = 500 / 12
  )
  fail_rate <- define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 15, # Median survival 15 months
    dropout_rate = 0.001,
    hr = c(1, .6) # Delay effect after 4 months
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
  # calculate all pieces of information
  weight <- list(method = "fh", param = list(rho = 0, gamma = 1))
  gs_info <- gsDesign2::gs_info_wlr(
    enroll_rate,
    fail_rate,
    ratio,
    analysis_time = analysis_time,
    weight = weight
  )
  fh01 <- gs_info %>% dplyr::mutate_if(is.numeric, round, digits = 5)

  up <- gsDesign::gsDesign(
    k = length(fh01$event),
    test.type = 1,
    n.I = fh01$event,
    maxn.IPlan = max(fh01$event),
    sfu = gsDesign::sfLDOF,
    sfupar = NULL
  )$upper$bound

  npe <- gsDesign2::gs_power_npe(
    theta = fh01$theta,
    info = fh01$info,
    info0 = fh01$info0,
    binding = FALSE,
    upper = gsDesign2::gs_b,
    lower = gsDesign2::gs_b,
    upar = up,
    lpar = c(qnorm(.1), rep(-Inf, length(fh01$event) - 1)),
    test_upper = TRUE,
    test_lower = TRUE,
    r = 18,
    tol = 1e-6
  ) %>% dplyr::arrange(analysis, bound)

  # output
  gspow <- gsDesign2::gs_power_wlr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = ratio, # Experimental:Control randomization ratio
    weight = weight,
    approx = "asymptotic",
    event = fh01$event, # Targeted events of analysis
    analysis_time = NULL, # Targeted times of analysis
    binding = FALSE,
    upper = gsDesign2::gs_b, # Default is Lan-DeMets approximation of
    upar = up,
    lower = gsDesign2::gs_b,
    lpar = c(qnorm(.1), rep(-Inf, length(fh01$event) - 1)), # Futility only at IA1
    test_upper = TRUE,
    test_lower = TRUE,
    r = 18,
    tol = 1e-6
  )

  # tests
  expect_equal(object = as.numeric(gspow$analysis$time), expected = fh01$time, tolerance = 0.0001)
  expect_equal(object = as.numeric(gspow$analysis$event), expected = fh01$event, tolerance = 1)

  tt <- gspow$bounds %>% dplyr::arrange(analysis, bound)
  tt1 <- npe %>%
    dplyr::arrange(analysis, bound) %>%
    dplyr::filter(z > -999999)
  expect_equal(object = as.numeric(tt$z), expected = as.numeric(tt1$z), tolerance = 0.1)
  expect_equal(object = as.numeric(tt$probability), expected = tt1$probability, tolerance = 0.001)
  expect_equal(object = as.numeric(gspow$analysis$ahr), expected = fh01$ahr, tolerance = 0.01)
  expect_equal(object = as.numeric(gspow$analysis$theta), expected = fh01$theta, tolerance = 0.001)
  expect_equal(object = as.numeric(gspow$analysis$info), expected = fh01$info, tolerance = 0.001)
  expect_equal(object = as.numeric(gspow$analysis$info0), expected = fh01$info0, tolerance = 0.001)
})
