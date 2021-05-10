load("./fixtures/simulation_test_data.Rdata")

testthat::test_that("AHR results are consistent with simulation results for single stratum and multiple cutoff", {
  enrollRates=tibble::tibble(Stratum="All",
                             duration=c(2,2,10),
                             rate=c(3,6,9))
  failRates=tibble::tibble(Stratum="All",
                           duration=c(3,100),
                           failRate=log(2)/c(9,18),
                           hr=c(.9,.6),
                           dropoutRate=rep(.001,2))
  actual <- AHR(enrollRates = enrollRates,
                failRates = failRates,
                totalDuration=c(12, 24, 36))

  testthat::expect_true(all.equal(simulation_AHR1$AHR, actual$AHR, tolerance = 0.005))
  testthat::expect_true(all.equal(simulation_AHR1$Events, actual$Events, tolerance = 0.005))
})

testthat::test_that("AHR results are consistent with simulation results for single stratum and single cutoff", {
  enrollRates=tibble::tibble(Stratum="All",
                             duration=c(2,2,10),
                             rate=c(3,6,9))
  failRates=tibble::tibble(Stratum="All",
                           duration=c(3,100),
                           failRate=log(2)/c(9,18),
                           hr=c(.9,.6),
                           dropoutRate=rep(.001,2))
  totalDuration = 30
  actual <- AHR(enrollRates = enrollRates,
                failRates = failRates,
                totalDuration=totalDuration)
  testthat::expect_true(all.equal(simulation_AHR2$AHR, actual$AHR, tolerance = 1e-3))
  testthat::expect_true(all.equal(simulation_AHR2$Events, actual$Events, tolerance = 2e-3))

})

testthat::test_that("AHR results are consistent with simulation results for single stratum and multiple cutoff", {
  enrollRates <- tibble::tibble(Stratum = "All",
                                duration = c(2, 2, 10),
                                rate = c(3, 6, 9))
  failRates <- tibble::tibble(Stratum = "All",
                              duration = c(3, Inf),
                              failRate = log(2)/c(9, 18),
                              hr = c(0.9, 0.6),
                              dropoutRate = rep(0.001, 2))
  totalDuration <- c(15, 30)

  actual <- AHR(enrollRates = enrollRates,
                failRates = failRates,
                totalDuration=totalDuration)
  testthat::expect_true(all.equal(simulation_AHR3$AHR, actual$AHR, tolerance = 5e-3))
  testthat::expect_true(all.equal(simulation_AHR3$Events, actual$Events, tolerance = 7e-3))

})
