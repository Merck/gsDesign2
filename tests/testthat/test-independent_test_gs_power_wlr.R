test_that("Check using gs_info_wlr and gs_power_npe", {
  enroll_rate <- tibble::tibble(stratum = "All",
                                duration = 12,
                                rate = 500 / 12)
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

  #create arms
  # Define study design object in each arm
  gs_arm <- gsDesign2:::gs_create_arm(
    enroll_rate,
    fail_rate,
    ratio = 2, # Randomization ratio
    total_time = 36 # Total study duration
  )
  arm0 <- gs_arm[["arm0"]]
  arm1 <- gs_arm[["arm1"]]
  #calculate all pieces of information
  weight <- function(x, arm0, arm1) {
    gsDesign2::wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 1)
  }
  gs_info <- gsDesign2::gs_info_wlr(
    enroll_rate,
    fail_rate,
    ratio,
    analysis_time = analysis_time,
    weight = weight
  )
  fh01 <-gs_info %>% dplyr::mutate_if(is.numeric, round, digits = 5)

  up <-gsDesign::gsDesign(k = length(fh01$Events),
                test.type = 1,
                n.I = fh01$Events,
                maxn.IPlan = max(fh01$Events),
                sfu = gsDesign::sfLDOF,
                sfupar = NULL)$upper$bound

  npe <- gsDesign2::gs_power_npe(theta = fh01$theta,
                      info = fh01$info,
                      info0 = fh01$info0,
                      binding = F,
                      upper = gsDesign2::gs_b,
                      lower = gsDesign2::gs_b,
                      upar = up,
                      lpar = c(qnorm(.1), rep(-Inf, length(fh01$Events) - 1)),
                      test_upper = T,
                      test_lower = T,
                      r = 18,
                      tol = 1e-6) %>% dplyr::arrange(Analysis, Bound)

 #output
  gspow <- gsDesign2::gs_power_wlr(enroll_rate = enroll_rate,
                        fail_rate = fail_rate,
                        ratio = ratio,               # Experimental:Control randomization ratio
                        weight = weight,
                        approx = "asymptotic",
                        events = fh01$Events, # Targeted events of analysis
                        analysis_time = NULL,   # Targeted times of analysis
                        binding = FALSE,
                        upper = gsDesign2::gs_b, # Default is Lan-DeMets approximation of
                        upar = up,
                        lower = gsDesign2::gs_b,
                        lpar = c(qnorm(.1), rep(-Inf, length(fh01$Events) - 1)), # Futility only at IA1
                        test_upper = TRUE,
                        test_lower = TRUE,
                        r = 18,
                        tol = 1e-6)

  #tests
  expect_equal(object = as.numeric(gspow$analysis$Time), expected = fh01$Time, tolerance = 0.0001)
  expect_equal(object = as.numeric(gspow$analysis$Events), expected = fh01$Events, tolerance = 1)

  tt <- gspow$bounds %>% dplyr::arrange(Analysis, Bound)
  tt1 <- npe %>% dplyr::arrange(Analysis, Bound) %>% dplyr::filter(Z > -999999)
  expect_equal(object = as.numeric(tt$Z), expected = as.numeric(tt1$Z), tolerance = 0.1)
  expect_equal(object = as.numeric(tt$Probability), expected = tt1$Probability, tolerance = 0.001)
  expect_equal(object = as.numeric(gspow$analysis$AHR), expected = fh01$AHR, tolerance = 0.01)
  expect_equal(object = as.numeric(gspow$analysis$theta), expected = fh01$theta, tolerance = 0.001)
  expect_equal(object = as.numeric(gspow$analysis$info), expected = fh01$info, tolerance = 0.001)
  expect_equal(object = as.numeric(gspow$analysis$info0), expected = fh01$info0, tolerance = 0.001)
})
