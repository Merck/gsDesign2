assert("Call with defaults", {
  x1 <- gs_design_ahr()
  x2 <- gs_design_ahr_()
  u1 <- x1$bound[x1$bound$bound == "upper", ]
  u2 <- x2$bounds[x2$bounds$Bound == "Upper", ]
  (x1$analysis$time %==% u2$Time)
  (x1$analysis$event %==% u2$Events)
  (u1$z %==% u2$Z)
  (u1$probability %==% u2$Probability)
  (x1$analysis$ahr %==% u2$AHR)
  (x1$analysis$theta %==% u2$theta)
  (x1$analysis$info %==% u2$info)
  (x1$analysis$info0 %==% u2$info0)
})

assert("Single analysis", {
  x1 <- gs_design_ahr(analysis_time = 40)
  x2 <- gs_design_ahr_(analysisTimes = 40)
  u1 <- x1$bound[x1$bound$bound == "upper", ]
  u2 <- x2$bounds[x2$bounds$Bound == "Upper", ]
  (x1$analysis$time %==% u2$Time)
  (x1$analysis$event %==% u2$Events)
  (u1$z %==% u2$Z)
  (u1$probability %==% u2$Probability)
  (x1$analysis$ahr %==% u2$AHR)
  (x1$analysis$theta %==% u2$theta)
  (x1$analysis$info %==% u2$info)
  (x1$analysis$info0 %==% u2$info0)
})

assert("Multiple analysisTimes", {
  x1 <- gs_design_ahr(analysis_time = c(12, 24, 36))
  x2 <- gs_design_ahr_(analysisTimes = c(12, 24, 36))
  u1 <- x1$bound[x1$bound$bound == "upper", ]
  u2 <- x2$bounds[x2$bounds$Bound == "Upper", ]
  l1 <- x1$bound[x1$bound$bound == "lower", ]
  l2 <- x2$bounds[x2$bounds$Bound == "Lower", ]
  (x1$analysis$time %==% u2$Time)
  (all.equal(x1$analysis$event, u2$Events))
  (u1$z %==% u2$Z)
  (u1$probability %==% u2$Probability)
  (x1$analysis$ahr %==% u2$AHR)
  (x1$analysis$theta %==% u2$theta)
  (x1$analysis$info %==% u2$info)
  (x1$analysis$info0 %==% u2$info0)
  (l1$z %==% l2$Z)
  (l1$probability %==% l2$Probability)
})

assert("Specified information fraction", {
  x1 <- gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = 36)
  x2 <- gs_design_ahr_(IF = c(.25, .75, 1), analysisTimes = 36)
  u1 <- x1$bound[x1$bound$bound == "upper", ]
  u2 <- x2$bounds[x2$bounds$Bound == "Upper", ]
  l1 <- x1$bound[x1$bound$bound == "lower", ]
  l2 <- x2$bounds[x2$bounds$Bound == "Lower", ]
  (all.equal(x1$analysis$time, u2$Time, tolerance = 1e-5))
  (all.equal(x1$analysis$event, u2$Events, tolerance = 1e-5))
  (all.equal(u1$z, u2$Z, tolerance = 1e-5))
  (all.equal(u1$probability, u2$Probability))
  (all.equal(x1$analysis$ahr, u2$AHR, tolerance = 1e-5))
  (all.equal(x1$analysis$theta, u2$theta, tolerance = 1e-5))
  (all.equal(x1$analysis$info, u2$info, tolerance = 1e-5))
  (all.equal(x1$analysis$info0, u2$info0, tolerance = 1e-5))
  (all.equal(l1$z, l2$Z, tolerance = 1e-5))
  (all.equal(l1$probability, l2$Probability))
})

assert("Multiple analysis times & IF and driven by times", {
  x1 <- gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
  x2 <- gs_design_ahr_(IF = c(.25, .75, 1), analysisTimes = c(12, 25, 36))
  u1 <- x1$bound[x1$bound$bound == "upper", ]
  u2 <- x2$bounds[x2$bounds$Bound == "Upper", ]
  l1 <- x1$bound[x1$bound$bound == "lower", ]
  l2 <- x2$bounds[x2$bounds$Bound == "Lower", ]
  (x1$analysis$time %==% u2$Time)
  (all.equal(x1$analysis$event, u2$Events))
  (u1$z %==% u2$Z)
  (u1$probability %==% u2$Probability)
  (x1$analysis$ahr %==% u2$AHR)
  (x1$analysis$theta %==% u2$theta)
  (x1$analysis$info %==% u2$info)
  (x1$analysis$info0 %==% u2$info0)
  (l1$z %==% l2$Z)
  (l1$probability %==% l2$Probability)
})

assert("Multiple analysis times & IF and driven by IF", {
  x1 <- gs_design_ahr(info_frac = c(1 / 3, .8, 1), analysis_time = c(12, 25, 36))
  x2 <- gs_design_ahr_(IF = c(1 / 3, .8, 1), analysisTimes = c(12, 25, 36))
  u1 <- x1$bound[x1$bound$bound == "upper", ]
  u2 <- x2$bounds[x2$bounds$Bound == "Upper", ]
  l1 <- x1$bound[x1$bound$bound == "lower", ]
  l2 <- x2$bounds[x2$bounds$Bound == "Lower", ]
  (x1$analysis$time %==% u2$Time)
  (all.equal(x1$analysis$event, u2$Events))
  (u1$z %==% u2$Z)
  (u1$probability %==% u2$Probability)
  (x1$analysis$ahr %==% u2$AHR)
  (x1$analysis$theta %==% u2$theta)
  (x1$analysis$info %==% u2$info)
  (x1$analysis$info0 %==% u2$info0)
  (l1$z %==% l2$Z)
  (l1$probability %==% l2$Probability)
})

assert("2-sided symmetric design with O'Brien-Fleming spending", {
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
  u1 <- x1$bound[x1$bound$bound == "upper", ]
  u2 <- x2$bounds[x2$bounds$Bound == "Upper", ]
  l1 <- x1$bound[x1$bound$bound == "lower", ]
  l2 <- x2$bounds[x2$bounds$Bound == "Lower", ]
  (x1$analysis$time %==% u2$Time)
  (x1$analysis$event %==% u2$Events)
  (u1$z %==% u2$Z)
  (u1$probability %==% u2$Probability)
  (x1$analysis$ahr %==% u2$AHR)
  (x1$analysis$theta %==% u2$theta)
  (x1$analysis$info %==% u2$info)
  (x1$analysis$info0 %==% u2$info0)
  (l1$z %==% l2$Z)
  (l1$probability %==% l2$Probability)
})

assert("Pocock lower spending under H1 (NPH)", {
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
  u1 <- x1$bound[x1$bound$bound == "upper", ]
  u2 <- x2$bounds[x2$bounds$Bound == "Upper", ]
  l1 <- x1$bound[x1$bound$bound == "lower", ]
  l2 <- x2$bounds[x2$bounds$Bound == "Lower", ]
  (x1$analysis$time %==% u2$Time)
  (all.equal(x1$analysis$event, u2$Events))
  (u1$z %==% u2$Z)
  (u1$probability %==% u2$Probability)
  (x1$analysis$ahr %==% u2$AHR)
  (x1$analysis$theta %==% u2$theta)
  (x1$analysis$info %==% u2$info)
  (x1$analysis$info0 %==% u2$info0)
  (l1$z %==% l2$Z)
  (l1$probability %==% l2$Probability)
})

assert("Spending time when both efficacy and futility bound are fixed", {

  x <- gs_design_ahr(alpha = 0.025,
                     beta = 0.1,
                     info_frac = 1:3/3, analysis_time = 36,
                     upper = gs_b,
                     upar = gsDesign::gsDesign(k = 3, test.type = 1, n.I = 1:3/3,
                                               sfu = gsDesign::sfLDOF, sfupar = NULL, alpha = 0.025)$upper$bound,
                     lower = gs_b,
                     lpar = rep(-Inf, 3))

  (!("spending_time" %in% names(x$bound)))
})

assert("Pre-specificed spending time", {

  # one-sided design
  x <- gs_design_ahr(alpha = 0.025,
                     beta = 0.1,
                     info_frac = NULL, analysis_time = c(12, 24, 36),
                     upper = gs_spending_bound,
                     upar = list(sf = gsDesign::sfLDOF, timing = c(12, 24, 36) / 36, total_spend = 0.025),
                     lower = gs_b,
                     lpar = rep(-Inf, 3))

  expected <- c(12, 24, 36) / 36
  (x$bound$spending_time %==% expected)

  # two-sided design
  x <- gs_design_ahr(alpha = 0.025,
                     beta = 0.1,
                     info_frac = NULL, analysis_time = c(12, 24, 36),
                     upper = gs_spending_bound,
                     upar = list(sf = gsDesign::sfLDOF, timing = c(12, 24, 36) / 36, total_spend = 0.025),
                     lower = gs_spending_bound,
                     lpar = list(sf = gsDesign::sfLDOF, timing = c(15, 24, 36) / 36, total_spend = 0.1))

  expected <- c(12, 24, 36) / 36
  (filter(x$bound, bound == "upper")$spending_time %==% expected)
  expected <- c(15, 24, 36) / 36
  (filter(x$bound, bound == "lower")$spending_time %==% expected)
})

assert("Spending time when the analyses are driven by information fraction", {
  # one-sided design
  x <- gs_design_ahr(alpha = 0.025,
                     beta = 0.1,
                     info_frac = 1:3/3, analysis_time = 36,
                     upper = gs_spending_bound,
                     upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
                     lower = gs_b,
                     lpar = rep(-Inf, 3))

  (all.equal(x$bound$spending_time, 1:3/3))

  # two-sided design with futility bound spending under H1
  x <- gs_design_ahr(alpha = 0.025,
                     beta = 0.1,
                     info_frac = 1:3/3, analysis_time = 36,
                     upper = gs_spending_bound,
                     upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
                     lower = gs_spending_bound,
                     lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = 3),
                     h1_spending = TRUE)

  (all.equal(filter(x$bound, bound == "upper")$spending_time, 1:3/3))
  expected <- x$analysis$info / max(x$analysis$info)
  (filter(x$bound, bound == "lower")$spending_time %==% expected)

  # two-sided design with futility bound spending under H0
  x <- gs_design_ahr(alpha = 0.025,
                     beta = 0.1,
                     info_frac = 1:3/3, analysis_time = 36,
                     upper = gs_spending_bound,
                     upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
                     lower = gs_spending_bound,
                     lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = 3),
                     h1_spending = FALSE)

  (all.equal(filter(x$bound, bound == "upper")$spending_time, 1:3/3))
  expected <- x$analysis$info0 / max(x$analysis$info0)
  (filter(x$bound, bound == "lower")$spending_time %==% expected)
})

assert("Spending time when some analyses are skipped", {

  # two-sided design with futility bound spending under H1
  x <- gs_design_ahr(alpha = 0.025,
                     beta = 0.1,
                     info_frac = 1:3/3, analysis_time = 36,
                     upper = gs_spending_bound,
                     upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
                     lower = gs_spending_bound,
                     lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = 3),
                     h1_spending = TRUE,
                     test_lower = c(FALSE, TRUE, TRUE))

  (all.equal(filter(x$bound, bound == "upper")$spending_time, 1:3/3))
  expected <- x$analysis$info[2:3] / max(x$analysis$info)
  (filter(x$bound, bound == "lower")$spending_time %==% expected)
})
