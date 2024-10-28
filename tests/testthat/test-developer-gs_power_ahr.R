test_that("default parameter", {
  x1 <- gs_power_ahr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1))
  x2 <- gs_power_ahr_(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1))
  expect_equal(x1$analysis$time, x2$Time[x2$Bound == "Upper"], tolerance = 1e-5)
  expect_equal(x1$analysis$event, x2$Events[x2$Bound == "Upper"], tolerance = 1e-5)
  expect_equal(x1$bound$z[x1$bound$bound == "upper"], x2$Z[x2$Bound == "Upper"], tolerance = 5e-6)
  expect_equal(x1$bound$probability[x1$bound$bound == "upper"], x2$Probability[x2$Bound == "Upper"], tolerance = 1e-5)
  expect_equal(x1$analysis$ahr, x2$AHR[x2$Bound == "Upper"], tolerance = 1e-6)
  expect_equal(x1$analysis$theta, x2$theta[x2$Bound == "Upper"], tolerance = 1e-6)
  expect_equal(x1$analysis$info, x2$info[x2$Bound == "Upper"], tolerance = 1e-6)
  expect_equal(x1$analysis$info0, x2$info0[x2$Bound == "Upper"], tolerance = 1e-6)
})

test_that("calendar based cut", {
  x1 <- gs_power_ahr(
    analysis_time = c(12, 24, 36),
    event = NULL,
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  )
  x2 <- gs_power_ahr_(
    analysisTimes = c(12, 24, 36),
    events = NULL,
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  )
  expect_equal(x1$analysis$time, x2$Time[x2$Bound == "Upper"])
  expect_equal(x1$analysis$event, x2$Events[x2$Bound == "Upper"])
  expect_equal(x1$bound$z[x1$bound$bound == "upper"], x2$Z[x2$Bound == "Upper"])
  expect_equal(x1$bound$probability[x1$bound$bound == "upper"], x2$Probability[x2$Bound == "Upper"])
  expect_equal(x1$analysis$ahr, x2$AHR[x2$Bound == "Upper"])
  expect_equal(x1$analysis$theta, x2$theta[x2$Bound == "Upper"])
  expect_equal(x1$analysis$info, x2$info[x2$Bound == "Upper"])
  expect_equal(x1$analysis$info0, x2$info0[x2$Bound == "Upper"])
  expect_equal(x1$analysis$time, x2$Time[x2$Bound == "Lower"])
  expect_equal(x1$analysis$event, x2$Events[x2$Bound == "Lower"])
  expect_equal(x1$bound$z[x1$bound$bound == "lower"], x2$Z[x2$Bound == "Lower"])
  expect_equal(x1$bound$probability[x1$bound$bound == "lower"], x2$Probability[x2$Bound == "Lower"])
  expect_equal(x1$analysis$ahr, x2$AHR[x2$Bound == "Lower"])
  expect_equal(x1$analysis$theta, x2$theta[x2$Bound == "Lower"])
  expect_equal(x1$analysis$info, x2$info[x2$Bound == "Lower"])
  expect_equal(x1$analysis$info0, x2$info0[x2$Bound == "Lower"])
})

test_that("event based cut", {
  x1 <- gs_power_ahr(
    analysis_time = NULL,
    event = c(20, 50, 70),
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  )
  x2 <- gs_power_ahr_(
    analysisTimes = NULL,
    events = c(20, 50, 70),
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  )
  expect_equal(x1$analysis$time, x2$Time[x2$Bound == "Upper"], tolerance = 1e-6)
  expect_equal(x1$analysis$event, x2$Events[x2$Bound == "Upper"], tolerance = 1e-6)
  expect_equal(x1$bound$z[x1$bound$bound == "upper"], x2$Z[x2$Bound == "Upper"], tolerance = 1e-6)
  expect_equal(x1$bound$probability[x1$bound$bound == "upper"], x2$Probability[x2$Bound == "Upper"], tolerance = 1e-6)
  expect_equal(x1$analysis$ahr, x2$AHR[x2$Bound == "Upper"], tolerance = 1e-6)
  expect_equal(x1$analysis$theta, x2$theta[x2$Bound == "Upper"], tolerance = 1e-6)
  expect_equal(x1$analysis$info, x2$info[x2$Bound == "Upper"], tolerance = 1e-6)
  expect_equal(x1$analysis$info0, x2$info0[x2$Bound == "Upper"], tolerance = 1e-6)
  expect_equal(x1$analysis$time, x2$Time[x2$Bound == "Lower"], tolerance = 1e-6)
  expect_equal(x1$analysis$event, x2$Events[x2$Bound == "Lower"], tolerance = 1e-6)
  expect_equal(x1$bound$z[x1$bound$bound == "lower"], x2$Z[x2$Bound == "Lower"], tolerance = 1e-6)
  expect_equal(x1$bound$probability[x1$bound$bound == "lower"], x2$Probability[x2$Bound == "Lower"])
  expect_equal(x1$analysis$ahr, x2$AHR[x2$Bound == "Lower"], tolerance = 1e-6)
  expect_equal(x1$analysis$theta, x2$theta[x2$Bound == "Lower"], tolerance = 1e-6)
  expect_equal(x1$analysis$info, x2$info[x2$Bound == "Lower"], tolerance = 1e-6)
  expect_equal(x1$analysis$info0, x2$info0[x2$Bound == "Lower"], tolerance = 1e-6)
})

test_that("calendar + event based cut", {
  x1 <- gs_power_ahr(
    analysis_time = c(12, 24, 36),
    event = c(30, 40, 50),
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  )
  x2 <- gs_power_ahr_(
    analysisTimes = c(12, 24, 36),
    events = c(30, 40, 50),
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  )
  expect_equal(x1$analysis$time, x2$Time[x2$Bound == "Upper"], tolerance = 1e-5)
  expect_equal(x1$analysis$event, x2$Events[x2$Bound == "Upper"], tolerance = 1e-5)
  expect_equal(x1$bound$z[x1$bound$bound == "upper"], x2$Z[x2$Bound == "Upper"], tolerance = 1e-6)
  expect_equal(x1$bound$probability[x1$bound$bound == "upper"], x2$Probability[x2$Bound == "Upper"], tolerance = 1e-6)
  expect_equal(x1$analysis$ahr, x2$AHR[x2$Bound == "Upper"], tolerance = 1e-6)
  expect_equal(x1$analysis$theta, x2$theta[x2$Bound == "Upper"], tolerance = 1e-6)
  expect_equal(x1$analysis$info, x2$info[x2$Bound == "Upper"], tolerance = 1e-5)
  expect_equal(x1$analysis$info0, x2$info0[x2$Bound == "Upper"], tolerance = 1e-5)
  expect_equal(x1$analysis$time, x2$Time[x2$Bound == "Lower"], tolerance = 1e-5)
  expect_equal(x1$analysis$event, x2$Events[x2$Bound == "Lower"], tolerance = 1e-5)
  expect_equal(x1$bound$z[x1$bound$bound == "lower"], x2$Z[x2$Bound == "Lower"], tolerance = 1e-5)
  expect_equal(x1$bound$probability[x1$bound$bound == "lower"], x2$Probability[x2$Bound == "Lower"], tolerance = 1e-6)
  expect_equal(x1$analysis$ahr, x2$AHR[x2$Bound == "Lower"], tolerance = 1e-6)
  expect_equal(x1$analysis$theta, x2$theta[x2$Bound == "Lower"], tolerance = 1e-6)
  expect_equal(x1$analysis$info, x2$info[x2$Bound == "Lower"], tolerance = 1e-5)
  expect_equal(x1$analysis$info0, x2$info0[x2$Bound == "Lower"], tolerance = 1e-5)
})

test_that("Use default lower and lpar but set test_lower is FALSE", {
  # using the default lower and lpar but set test_lower = FALSE,
  # which means there is no futility test
  x <- gs_power_ahr(analysis_time = c(24, 36),
                    event = c(50, 100),
                    test_lower = FALSE)

  expect_equal(unique(x$bound$bound), "upper")
})

test_that("Use default lower, lpar and test_lower", {
  # using the default lower, lpar and test_lower,
  # return an error message asking for total spend for the futility test
  expect_error(gs_power_ahr(analysis_time = c(24, 36),
                            event = c(50, 100)))
})
