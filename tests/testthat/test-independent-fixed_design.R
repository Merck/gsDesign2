# Enrollment rate
enrollRates <- tibble::tibble(
  Stratum = "All", 
  duration = 18, 
  rate = 20)

# Failure rates
failRates <- tibble::tibble(
  Stratum = "All", 
  duration = c(4, 100), 
  failRate = log(2) / 12,
  hr = c(1, .6), 
  dropoutRate = .001)

# Study duration in months
studyDuration <- 36

# Experimental / Control randomization ratio
ratio <- 1 

test_that("input checking", {
  # miss enrollRates
  expect_error(fixed_design("AHR", alpha = 0.025, power = 0.9, failRates = failRates, studyDuration = studyDuration, ratio = ratio))
  
  # miss failRates
  expect_error(fixed_design("AHR", alpha = 0.025, power = 0.9, enrollRates = enrollRates, studyDuration = studyDuration, ratio = ratio))
  
  # multiple rho for FH/MB
  expect_error(fixed_design("FH", alpha = 0.025, power = 0.9, enrollRates = enrollRates, failRates = failRates, 
                            studyDuration = studyDuration, ratio = ratio, rho = c(0.5, 0)))
  expect_error(fixed_design("MB", alpha = 0.025, power = 0.9, enrollRates = enrollRates, failRates = failRates, 
                            studyDuration = studyDuration, ratio = ratio, rho = c(0.5, 0)))
  
  # multiple tau for FH/MB
  expect_error(fixed_design("FH", alpha = 0.025, power = 0.9, enrollRates = enrollRates, failRates = failRates, 
                            studyDuration = studyDuration, ratio = ratio, tau = c(0.5, 0)))
  expect_error(fixed_design("MB", alpha = 0.025, power = 0.9, enrollRates = enrollRates, failRates = failRates, 
                            studyDuration = studyDuration, ratio = ratio, tau = c(0.5, 0)))
  
  # redundant tau in FH
  expect_error(fixed_design("FH", alpha = 0.025, power = 0.9, enrollRates = enrollRates, failRates = failRates, 
                            studyDuration = studyDuration, ratio = ratio, tau = 0.5))
  
  # redundant rho/gamma in MB
  expect_error(fixed_design("MB", alpha = 0.025, power = 0.9, enrollRates = enrollRates, failRates = failRates, 
                            studyDuration = studyDuration, ratio = ratio, rho = 0.5, gamma = 0.5))
  
  # p_c/p_e/rd0 not input in RD
  expect_error(fixed_design("RD", alpha = 0.025, power = 0.9, p_e = 0.1, rd0 = 0, ratio = ratio))
  expect_error(fixed_design("RD", alpha = 0.025, power = 0.9, p_c = 0.1, rd0 = 0, ratio = ratio))
  expect_error(fixed_design("RD", alpha = 0.025, power = 0.9, p_c= 0.2, p_e = 0.1, ratio = ratio))
  expect_error(fixed_design("RD", alpha = 0.025, p_c= 0.2, p_e = 0.1, rd0 = 0, ratio = ratio))
})

test_that("AHR", {
  x <- fixed_design(x = "AHR", 
                    alpha = 0.025, power = 0.9, 
                    enrollRates = enrollRates, failRates = failRates, 
                    studyDuration = studyDuration, ratio = ratio) 
  
  y <- fixed_design(x = "AHR", 
                    alpha = 0.025,  
                    enrollRates = enrollRates %>% mutate(rate = x$analysis$N/duration), failRates = failRates, 
                    studyDuration = studyDuration, ratio = ratio) 
  
  expect(y$analysis$Power, 0.9)
})

test_that("FH", {
  x <- fixed_design(x = "FH", 
                    alpha = 0.025, power = 0.9, 
                    enrollRates = enrollRates, failRates = failRates, 
                    studyDuration = studyDuration, ratio = ratio,
                    rho = 0.5, gamma = 0.5) 
  
  y <- fixed_design(x = "FH", 
                    alpha = 0.025,  
                    enrollRates = enrollRates %>% mutate(rate = x$analysis$N/duration), failRates = failRates, 
                    studyDuration = studyDuration, ratio = ratio,
                    rho = 0.5, gamma = 0.5) 
  
  expect(y$analysis$Power, 0.9)
})

test_that("MB", {
  x <- fixed_design(x = "MB", 
                    alpha = 0.025, power = 0.9, 
                    enrollRates = enrollRates, failRates = failRates, 
                    studyDuration = studyDuration, ratio = ratio,
                    tau = 8) 
  
  y <- fixed_design(x = "MB", 
                    alpha = 0.025,  
                    enrollRates = enrollRates %>% mutate(rate = x$analysis$N/duration), failRates = failRates, 
                    studyDuration = studyDuration, ratio = ratio,
                    tau = 8) 
  
  expect(y$analysis$Power, 0.9)
})

test_that("LF", {
  x <- fixed_design(x = "LF", 
                    alpha = 0.025, power = 0.9, 
                    enrollRates = enrollRates, failRates = failRates, 
                    studyDuration = studyDuration, ratio = ratio) 
  
  y <- fixed_design(x = "LF", 
                    alpha = 0.025,  
                    enrollRates = enrollRates %>% mutate(rate = x$analysis$N/duration), failRates = failRates, 
                    studyDuration = studyDuration, ratio = ratio) 
  
  expect(y$analysis$Power, 0.9)
})

test_that("MaxCombo", {
  x <- fixed_design(x = "MaxCombo", 
                    alpha = 0.025, power = 0.9, 
                    enrollRates = enrollRates, failRates = failRates, 
                    studyDuration = studyDuration, ratio = ratio,
                    rho = c(0, 0.5, 0.5),
                    gamma = c(0, 0, 0.5),
                    tau = c(-1, 4, 6)) 
  
  y <- fixed_design(x = "MaxCombo", 
                    alpha = 0.025,  
                    enrollRates = enrollRates %>% mutate(rate = x$analysis$N/duration), failRates = failRates, 
                    studyDuration = studyDuration, ratio = ratio,
                    rho = c(0, 0.5, 0.5),
                    gamma = c(0, 0, 0.5),
                    tau = c(-1, 4, 6)) 
  
  expect(y$analysis$Power, 0.9)
})

test_that("RMST", {
  x <- fixed_design(x = "RMST", 
                    alpha = 0.025, power = 0.9, 
                    enrollRates = enrollRates, failRates = failRates, 
                    studyDuration = studyDuration, ratio = ratio,
                    tau = 18) 
  
  y <- fixed_design(x = "RMST", 
                    alpha = 0.025,  
                    enrollRates = enrollRates %>% mutate(rate = x$analysis$N/duration), failRates = failRates, 
                    studyDuration = studyDuration, ratio = ratio,
                    tau = 18) 
  
  expect(y$analysis$Power, 0.9)
})

test_that("RD", {
  x <- fixed_design(x = "RD", 
                    alpha = 0.025, power = 0.9, 
                    p_c = .15, p_e = .1, rd0 = 0, ratio = ratio,
                    tau = 18) 
  
  y <- fixed_design(x = "RD", 
                    alpha = 0.025, N = x$analysis$N,
                    p_c = .15, p_e = .1, rd0 = 0, ratio = ratio,
                    tau = 18) 
  
  expect(y$analysis$Power, 0.9)
})

