assert("default parameter", {
  x1 <- gs_power_ahr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1))
  x2 <- gs_power_ahr_(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1))
  (all.equal(x1$analysis$time, x2$Time[x2$Bound == "Upper"], tolerance = 1e-5))
  (all.equal(x1$analysis$event, x2$Events[x2$Bound == "Upper"], tolerance = 1e-5))
  (all.equal(x1$bound$z[x1$bound$bound == "upper"], x2$Z[x2$Bound == "Upper"], tolerance = 5e-6))
  (all.equal(x1$bound$probability[x1$bound$bound == "upper"], x2$Probability[x2$Bound == "Upper"], tolerance = 1e-5))
  (all.equal(x1$analysis$ahr, x2$AHR[x2$Bound == "Upper"], tolerance = 1e-6))
  (all.equal(x1$analysis$theta, x2$theta[x2$Bound == "Upper"], tolerance = 1e-6))
  (all.equal(x1$analysis$info, x2$info[x2$Bound == "Upper"], tolerance = 1e-6))
  (all.equal(x1$analysis$info0, x2$info0[x2$Bound == "Upper"], tolerance = 1e-6))
})

assert("calendar based cut", {
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
  u1 <- x1$bound[x1$bound$bound == "upper", ]
  u2 <- x2[x2$Bound == "Upper", ]
  l1 <- x1$bound[x1$bound$bound == "lower", ]
  l2 <- x2[x2$Bound == "Lower", ]
  (x1$analysis$time %==% u2$Time)
  (x1$analysis$event %==% u2$Events)
  (u1$z %==% u2$Z)
  (u1$probability %==% u2$Probability)
  (x1$analysis$ahr %==% u2$AHR)
  (x1$analysis$theta %==% u2$theta)
  (x1$analysis$info %==% u2$info)
  (x1$analysis$info0 %==% u2$info0)
  (x1$analysis$time %==% l2$Time)
  (x1$analysis$event %==% l2$Events)
  (l1$z %==% l2$Z)
  (l1$probability %==% l2$Probability)
  (x1$analysis$ahr %==% l2$AHR)
  (x1$analysis$theta %==% l2$theta)
  (x1$analysis$info %==% l2$info)
  (x1$analysis$info0 %==% l2$info0)
})

assert("event based cut", {
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
  (all.equal(x1$analysis$time, x2$Time[x2$Bound == "Upper"], tolerance = 1e-6))
  (all.equal(x1$analysis$event, x2$Events[x2$Bound == "Upper"], tolerance = 1e-6))
  (all.equal(x1$bound$z[x1$bound$bound == "upper"], x2$Z[x2$Bound == "Upper"], tolerance = 1e-6))
  (all.equal(x1$bound$probability[x1$bound$bound == "upper"], x2$Probability[x2$Bound == "Upper"], tolerance = 1e-6))
  (all.equal(x1$analysis$ahr, x2$AHR[x2$Bound == "Upper"], tolerance = 1e-6))
  (all.equal(x1$analysis$theta, x2$theta[x2$Bound == "Upper"], tolerance = 1e-6))
  (all.equal(x1$analysis$info, x2$info[x2$Bound == "Upper"], tolerance = 1e-6))
  (all.equal(x1$analysis$info0, x2$info0[x2$Bound == "Upper"], tolerance = 1e-6))
  (all.equal(x1$analysis$time, x2$Time[x2$Bound == "Lower"], tolerance = 1e-6))
  (all.equal(x1$analysis$event, x2$Events[x2$Bound == "Lower"], tolerance = 1e-6))
  (all.equal(x1$bound$z[x1$bound$bound == "lower"], x2$Z[x2$Bound == "Lower"], tolerance = 1e-6))
  (all.equal(x1$bound$probability[x1$bound$bound == "lower"], x2$Probability[x2$Bound == "Lower"], tolerance = 1e-5))
  (all.equal(x1$analysis$ahr, x2$AHR[x2$Bound == "Lower"], tolerance = 1e-6))
  (all.equal(x1$analysis$theta, x2$theta[x2$Bound == "Lower"], tolerance = 1e-6))
  (all.equal(x1$analysis$info, x2$info[x2$Bound == "Lower"], tolerance = 1e-6))
  (all.equal(x1$analysis$info0, x2$info0[x2$Bound == "Lower"], tolerance = 1e-6))
})

assert("calendar + event based cut", {
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
  (all.equal(x1$analysis$time, x2$Time[x2$Bound == "Upper"], tolerance = 1e-5))
  (all.equal(x1$analysis$event, x2$Events[x2$Bound == "Upper"], tolerance = 1e-5))
  (all.equal(x1$bound$z[x1$bound$bound == "upper"], x2$Z[x2$Bound == "Upper"], tolerance = 1e-6))
  (all.equal(x1$bound$probability[x1$bound$bound == "upper"], x2$Probability[x2$Bound == "Upper"], tolerance = 1e-6))
  (all.equal(x1$analysis$ahr, x2$AHR[x2$Bound == "Upper"], tolerance = 1e-6))
  (all.equal(x1$analysis$theta, x2$theta[x2$Bound == "Upper"], tolerance = 1e-6, scale = 1))
  (all.equal(x1$analysis$info, x2$info[x2$Bound == "Upper"], tolerance = 1e-5))
  (all.equal(x1$analysis$info0, x2$info0[x2$Bound == "Upper"], tolerance = 1e-5))
  (all.equal(x1$analysis$time, x2$Time[x2$Bound == "Lower"], tolerance = 1e-5))
  (all.equal(x1$analysis$event, x2$Events[x2$Bound == "Lower"], tolerance = 1e-5))
  (all.equal(x1$bound$z[x1$bound$bound == "lower"], x2$Z[x2$Bound == "Lower"], tolerance = 1e-5))
  (all.equal(x1$bound$probability[x1$bound$bound == "lower"], x2$Probability[x2$Bound == "Lower"], tolerance = 1e-6))
  (all.equal(x1$analysis$ahr, x2$AHR[x2$Bound == "Lower"], tolerance = 1e-6))
  (all.equal(x1$analysis$theta, x2$theta[x2$Bound == "Lower"], tolerance = 1e-6, scale = 1))
  (all.equal(x1$analysis$info, x2$info[x2$Bound == "Lower"], tolerance = 1e-5))
  (all.equal(x1$analysis$info0, x2$info0[x2$Bound == "Lower"], tolerance = 1e-5))
})

assert("Use default lower and lpar but set test_lower is FALSE", {
  # using the default lower and lpar but set test_lower = FALSE,
  # which means there is no futility test
  x <- gs_power_ahr(analysis_time = c(24, 36),
                    event = c(50, 100),
                    test_lower = FALSE)

  (unique(x$bound$bound) %==% "upper")
})

assert("Use default lower, lpar and test_lower", {
  # using the default lower, lpar and test_lower,
  # return an error message asking for total spend for the futility test
  (has_error(gs_power_ahr(analysis_time = c(24, 36),
                            event = c(50, 100))))
})

assert("Validate the boundary is symmetric in symmetric designs.", {
  x <- gs_design_ahr(analysis_time = 36, info_frac = 1:3/3,
                     upper = gs_spending_bound,
                     upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
                     lower = gs_spending_bound,
                     lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
                     binding = TRUE, h1_spending = FALSE) |>
  to_integer()

  upper_z <- x$bound$z[x$bound$bound == "upper"]
  lower_z <- x$bound$z[x$bound$bound == "lower"]
  (all.equal(upper_z, -lower_z))
})
