# Test 1: independent test using AHR to check outputs of gs_info_ahr ####

testthat::test_that("results match if only put in targeted analysis times",{
  enrollRates <- tibble::tibble(Stratum = "All",
                                duration = c(2, 2, 10),
                                rate = c(3, 6, 9))
  failRates <- tibble::tibble(Stratum = "All",
                              duration = c(3, 100),
                              failRate = log(2)/c(9, 18),
                              hr = c(0.9, 0.6),
                              dropoutRate = rep(0.001, 2))
  totalDuration <- c(18, 27, 36)
  
  testthat::expect_equal(gs_info_ahr(enrollRates = enrollRates,
                                     failRates = failRates,
                                     analysisTimes = totalDuration) %>% select(Time, AHR, Events, info, info0),
                         AHR(enrollRates = enrollRates,
                             failRates = failRates,
                             totalDuration = totalDuration))
})


testthat::test_that("results match if only put in targeted events",{
  enrollRates <- tibble::tibble(Stratum = "All",
                                duration = c(2, 2, 10),
                                rate = c(3, 6, 9))
  failRates <- tibble::tibble(Stratum = "All",
                              duration = c(3, 100),
                              failRate = log(2)/c(9, 18),
                              hr = c(0.9, 0.6),
                              dropoutRate = rep(0.001, 2))
  events <- c(30, 40, 50)
  
  out1 <- gs_info_ahr(enrollRates = enrollRates, failRates = failRates, events = events)
  
  totalDuration <- out1$Time
  
  testthat::expect_equal(out1 %>% select(Time, AHR, Events, info, info0),
                         AHR(enrollRates = enrollRates,
                         failRates=failRates,
                         totalDuration = totalDuration))
  
  # since above test is based on the output "Time", here is to check whether the output "Time" is reasonable
  
  # "Time" should be at the time points when targeted event numbers are achieved
  testthat::expect_equal(round(out1$Events), round(events))
  
})


testthat::test_that("results match if put in both analysis time and targeted events",{
  enrollRates <- tibble::tibble(Stratum = "All",
                                duration = c(2, 2, 10),
                                rate = c(3, 6, 9))
  failRates <- tibble::tibble(Stratum = "All",
                              duration = c(3, 100),
                              failRate = log(2)/c(9, 18),
                              hr = c(0.9, 0.6),
                              dropoutRate = rep(0.001, 2))
  events <- c(30, 40, 50)
  analysisTime <- c(16, 19, 26)
  
  out1 <- gs_info_ahr(enrollRates = enrollRates,
                      failRates = failRates,
                      events = events,
                      analysisTimes = analysisTime)
  
  totalDuration <- out1$Time
  
  testthat::expect_equal(out1 %>% select(Time, AHR, Events, info, info0),
                         AHR(enrollRates = enrollRates,
                             failRates = failRates,
                             totalDuration = totalDuration))
  
  # since above test is based on the output "Time", here is to check whether the output "Time" is reasonable
  
  # either being equal to the corresponding element in the input analysisTime or at the time point when targeted event number achieved
  testthat::expect_equal(max((1 - (out1$Time == analysisTime))*(1 - (round(out1$Events) == round(events)))),
                         0)
  
  # "Time" >= input analysisTime
  testthat::expect_gte(max(out1$Time - analysisTime), 0)
  
  # "Events" >= input events
  testthat::expect_gte(max(round(out1$Events) - round(events)), 0)
  
})
