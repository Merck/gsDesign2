source_files <- list.files("./old_function/", "*.R$")
sapply(paste0("./old_function/", source_files), source)

library(dplyr)

test_that("Call with defaults", {
  x1 <- gs_design_ahr()
  x2 <- gs_design_ahr_()
  expect_equal(x1$analysis$time, x2$bounds$Time[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$event, x2$bounds$Events[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$z[x1$bound$bound == "upper"], x2$bounds$Z[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$probability[x1$bound$bound == "upper"], x2$bounds$Probability[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$ahr, x2$bounds$AHR[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$theta, x2$bounds$theta[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$info, x2$bounds$info[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$info0, x2$bounds$info0[x2$bounds$Bound == "Upper"])
})

test_that("Single analysis", {
  x1 <- gs_design_ahr(analysis_time = 40)
  x2 <- gs_design_ahr_(analysisTimes = 40)
  expect_equal(x1$analysis$time, x2$bounds$Time[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$event, x2$bounds$Events[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$z[x1$bound$bound == "upper"], x2$bounds$Z[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$probability[x1$bound$bound == "upper"], x2$bounds$Probability[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$ahr, x2$bounds$AHR[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$theta, x2$bounds$theta[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$info, x2$bounds$info[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$info0, x2$bounds$info0[x2$bounds$Bound == "Upper"])
})

test_that("Multiple analysisTimes", {
  x1 <- gs_design_ahr(analysis_time = c(12, 24, 36))
  x2 <- gs_design_ahr_(analysisTimes = c(12, 24, 36))
  expect_equal(x1$analysis$time, x2$bounds$Time[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$event, x2$bounds$Events[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$z[x1$bound$bound == "upper"], x2$bounds$Z[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$probability[x1$bound$bound == "upper"], x2$bounds$Probability[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$ahr, x2$bounds$AHR[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$theta, x2$bounds$theta[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$info, x2$bounds$info[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$info0, x2$bounds$info0[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$z[x1$bound$bound == "lower"], x2$bounds$Z[x2$bounds$Bound == "Lower"][1])
  expect_equal(x1$bound$probability[x1$bound$bound == "lower"], x2$bounds$Probability[x2$bounds$Bound == "Lower"][1])
})

test_that("Specified information fraction", {
  x1 <- gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = 36)
  x2 <- gs_design_ahr_(IF = c(.25, .75, 1), analysisTimes = 36)
  expect_equal(x1$analysis$time, x2$bounds$Time[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$event, x2$bounds$Events[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$z[x1$bound$bound == "upper"], x2$bounds$Z[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$probability[x1$bound$bound == "upper"], x2$bounds$Probability[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$ahr, x2$bounds$AHR[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$theta, x2$bounds$theta[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$info, x2$bounds$info[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$info0, x2$bounds$info0[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$z[x1$bound$bound == "lower"], x2$bounds$Z[x2$bounds$Bound == "Lower"][1])
  expect_equal(x1$bound$probability[x1$bound$bound == "lower"], x2$bounds$Probability[x2$bounds$Bound == "Lower"][1])
})

test_that("Multiple analysis times & IF and driven by times", {
  x1 <- gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
  x2 <- gs_design_ahr_(IF = c(.25, .75, 1), analysisTimes = c(12, 25, 36))
  expect_equal(x1$analysis$time, x2$bounds$Time[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$event, x2$bounds$Events[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$z[x1$bound$bound == "upper"], x2$bounds$Z[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$probability[x1$bound$bound == "upper"], x2$bounds$Probability[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$ahr, x2$bounds$AHR[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$theta, x2$bounds$theta[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$info, x2$bounds$info[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$info0, x2$bounds$info0[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$z[x1$bound$bound == "lower"], x2$bounds$Z[x2$bounds$Bound == "Lower"][1])
  expect_equal(x1$bound$probability[x1$bound$bound == "lower"], x2$bounds$Probability[x2$bounds$Bound == "Lower"][1])
})

test_that("Multiple analysis times & IF and driven by IF", {
  x1 <- gs_design_ahr(info_frac = c(1 / 3, .8, 1), analysis_time = c(12, 25, 36))
  x2 <- gs_design_ahr_(IF = c(1 / 3, .8, 1), analysisTimes = c(12, 25, 36))
  expect_equal(x1$analysis$time, x2$bounds$Time[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$event, x2$bounds$Events[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$z[x1$bound$bound == "upper"], x2$bounds$Z[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$probability[x1$bound$bound == "upper"], x2$bounds$Probability[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$ahr, x2$bounds$AHR[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$theta, x2$bounds$theta[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$info, x2$bounds$info[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$info0, x2$bounds$info0[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$z[x1$bound$bound == "lower"], x2$bounds$Z[x2$bounds$Bound == "Lower"][1])
  expect_equal(x1$bound$probability[x1$bound$bound == "lower"], x2$bounds$Probability[x2$bounds$Bound == "Lower"][1])
})

test_that("2-sided symmetric design with O'Brien-Fleming spending", {
  x1 <- gs_design_ahr(
    analysis_time = c(12, 24, 36), binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    h1_spending = FALSE
  )
  x2 <- gs_design_ahr_(
    analysisTimes = c(12, 24, 36), binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    h1_spending = FALSE
  )
  expect_equal(x1$analysis$time, x2$bounds$Time[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$event, x2$bounds$Events[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$z[x1$bound$bound == "upper"], x2$bounds$Z[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$probability[x1$bound$bound == "upper"], x2$bounds$Probability[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$ahr, x2$bounds$AHR[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$theta, x2$bounds$theta[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$info, x2$bounds$info[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$info0, x2$bounds$info0[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$z[x1$bound$bound == "lower"], x2$bounds$Z[x2$bounds$Bound == "Lower"])
  expect_equal(x1$bound$probability[x1$bound$bound == "lower"], x2$bounds$Probability[x2$bounds$Bound == "Lower"])
})

test_that("Pocock lower spending under H1 (NPH)", {
  x1 <- gs_design_ahr(
    analysis_time = c(12, 24, 36), binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDPocock, total_spend = 0.1, param = NULL, timing = NULL),
    h1_spending = TRUE
  )
  x2 <- gs_design_ahr_(
    analysisTimes = c(12, 24, 36), binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDPocock, total_spend = 0.1, param = NULL, timing = NULL),
    h1_spending = TRUE
  )
  expect_equal(x1$analysis$time, x2$bounds$Time[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$event, x2$bounds$Events[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$z[x1$bound$bound == "upper"], x2$bounds$Z[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$probability[x1$bound$bound == "upper"], x2$bounds$Probability[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$ahr, x2$bounds$AHR[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$theta, x2$bounds$theta[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$info, x2$bounds$info[x2$bounds$Bound == "Upper"])
  expect_equal(x1$analysis$info0, x2$bounds$info0[x2$bounds$Bound == "Upper"])
  expect_equal(x1$bound$z[x1$bound$bound == "lower"], x2$bounds$Z[x2$bounds$Bound == "Lower"])
  expect_equal(x1$bound$probability[x1$bound$bound == "lower"], x2$bounds$Probability[x2$bounds$Bound == "Lower"])
})
