test_that("check enrollments",{
  expect_error(gsDesign2:::check_enrollRates(tibble::tibble(rate = c(2, 4))))
  expect_error(gsDesign2:::check_enrollRates(tibble::tibble(duration = c(10, 20), rate = c("a", "b"))))
  expect_error(gsDesign2:::check_enrollRates(tibble::tibble(duration = c(10, 20), rate = c(2, -4))))
  
  expect_error(gsDesign2:::check_enrollRates(tibble::tibble(duration = c(10, 20))))
  expect_error(gsDesign2:::check_enrollRates(tibble::tibble(rate = c(2, 4), duration = c("a", "b"))))
  expect_error(gsDesign2:::check_enrollRates(tibble::tibble(rate = c(2, 4), duration = c(10, -20))))
})

test_that("check failRates",{
  # lack duration
  expect_error(gsDesign2:::check_failRates(tibble::tibble(failRates = c(0.2, 0.4), dropoutRates = 0.01)))
  # lack failRates
  expect_error(gsDesign2:::check_failRates(tibble::tibble(duration = c(2, 4), dropoutRates = 0.01)))
  # lack dropoutRates
  expect_error(gsDesign2:::check_failRates(tibble::tibble(failRates = c(0.2, 0.4), duration = c(10, 20))))
  
  # check of column `duration`
  expect_error(gsDesign2:::check_failRates(tibble::tibble(failRates = c(2, 4), duration = c("a", "b"), dropoutRates = 0.01)))
  expect_error(gsDesign2:::check_failRates(tibble::tibble(failRates = c(2, 4), duration = c(10, -20), dropoutRates = 0.01)))
  
  #check of column `failRates`
  expect_error(gsDesign2:::check_failRates(tibble::tibble(duration = c(10, 20), failRates = c("a", "b"), dropoutRates = 0.01)))
  expect_error(gsDesign2:::check_failRates(tibble::tibble(duration = c(10, 20), failRates = c(2, -4), dropoutRates = 0.01)))
  
  #check of column `hr`
  expect_error(gsDesign2:::check_failRates(tibble::tibble(duration = c(10, 20), failRates = c(0.02, 0.04), dropoutRates = 0.01, hr = "a")))
  expect_error(gsDesign2:::check_failRates(tibble::tibble(duration = c(10, 20), failRates = c(2, -4), dropoutRates = 0.01, hr = -1)))
  
  #check of column `dropoutRate`
  expect_error(gsDesign2:::check_failRates(tibble::tibble(duration = c(10, 20), failRates = c(0.02, 0.04), dropoutRates = "a", hr = 0.6)))
  expect_error(gsDesign2:::check_failRates(tibble::tibble(duration = c(10, 20), failRates = c(2, -4), dropoutRates = -1, hr = 0.6)))
})

test_that("check enrollments and failRates together",{
  expect_error(gsDesign2:::check_enrollRates_failRates(enrollRates = tibble::tibble(duration = c(10, 20),
                                                                                    rate = c(2, 4),
                                                                                    Stratum = "All"),
                                                       failRates = tibble::tibble(duration = c(10, 20), 
                                                                                  failRates = c(0.02, 0.04), 
                                                                                  dropoutRates = 0.001, 
                                                                                  hr = 0.6,
                                                                                  Stratum = c("S1", "S2"))))
 
})

test_that("check analysisTimes",{
  expect_error(gsDesign2:::check_analysisTimes("a"))
  expect_error(gsDesign2:::check_analysisTimes(c(20, 10)))
})

test_that("check events",{
  expect_error(gsDesign2:::check_events("a"))
  expect_error(gsDesign2:::check_events(c(20, 10)))
})

testthat::test_that("check totalDuration",{
  expect_error(gsDesign2:::check_totalDuration("a"))
  expect_error(gsDesign2:::check_totalDuration(c(-10, 10)))
})

test_that("check ratio",{
  expect_error(gsDesign2:::check_ratio("a"))
  expect_error(gsDesign2:::check_ratio(-2))
})

test_that("check info",{
  expect_error(gsDesign2:::check_info(c("a", "b")))
  expect_error(gsDesign2:::check_info(c(20, 10)))
})

test_that("check theta",{
  expect_error(gsDesign2:::check_theta(c("a", "b"), K = 2))
  expect_error(gsDesign2:::check_theta(c(20, 10), K = 1))
  expect_error(gsDesign2:::check_theta(c(20, -10), K = 2))
})

test_that("check test_upper",{
  expect_error(gsDesign2:::check_test_upper(c("a", "b"), K = 2))
  expect_error(gsDesign2:::check_test_upper(c(TRUE, FALSE, FALSE), K = 1))
  expect_error(gsDesign2:::check_test_upper(c(TRUE, FALSE), K = 2))
})

test_that("check test_lower",{
  expect_error(gsDesign2:::check_test_lower(c("a", "b"), K = 2))
  expect_error(gsDesign2:::check_test_lower(c(TRUE, FALSE, FALSE), K = 1))
})

test_that("check check_alpha_beta",{
  expect_error(gsDesign2:::check_alpha_beta(alpha = "a", beta = 0.2))
  expect_error(gsDesign2:::check_alpha_beta(alpha = 0.025, beta = "b"))
  expect_error(gsDesign2:::check_alpha_beta(alpha = c(0.025, 0.05), beta = 0.2))
  expect_error(gsDesign2:::check_alpha_beta(alpha = 0.025, beta = c(0.2, 0.3)))
  expect_error(gsDesign2:::check_alpha_beta(alpha = -1, beta = 0.1))
  expect_error(gsDesign2:::check_alpha_beta(alpha = 0.025, beta = -0.1))
  expect_error(gsDesign2:::check_alpha_beta(alpha = 0.5, beta = 0.6))
})

test_that("check check_IF",{
  expect_error(gsDesign2:::check_IF(c("a", "b")))
  expect_error(gsDesign2:::check_IF(c(2/3, 1/3, 1)))
  expect_error(gsDesign2:::check_IF(c(2/3, 3/4)))
})
