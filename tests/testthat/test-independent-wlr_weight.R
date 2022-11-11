test_that("test wlr_weight_1", {
  expect_equal(gsDesign2::wlr_weight_1(), 1)
})

test_that("test wlr_weight_n", {
  enrollRates <- tibble::tibble(Stratum = "All",
                                duration = c(2, 2, 30),
                                rate = c(3, 6, 9))
  failRates <- tibble::tibble(Stratum = "All",
                              duration = c(3, 100),
                              failRate = log(2)/c(9, 18),
                              hr = c(.9,.6),
                              dropoutRate = rep(.001, 2))
  total_time <- 36
  analysis_time <- 12
  
  arm <- gsDesign2:::gs_create_arm(enrollRates, failRates, ratio = 1, total_time = total_time)
  arm0 <- arm$arm0
  arm1 <- arm$arm1
  
  expect_equal(gsDesign2::wlr_weight_n(x = analysis_time, arm0 = arm0, arm1 = arm1, power = 2), 
               (2 * (0.5 * gsDesign2:::prob_risk(arm0, analysis_time, total_time) + 0.5 * gsDesign2:::prob_risk(arm1, analysis_time, total_time)))^2)
})