# Test 1: compare with gsDesign under proportional hazard ####

x <- gsSurv(
  k = 2,
  test.type = 1,
  alpha = 0.025,
  beta = 0.2,
  astar = 0,
  timing = 0.7,
  sfu = sfLDOF,
  sfupar = c(0),
  sfl = sfLDOF,
  sflpar = c(0),
  lambdaC = log(2) / 9,
  hr = 0.65,
  hr0 = 1,
  eta = 0.001,
  gamma = c(6, 12, 18, 24),
  R = c(2, 2, 2, 6),
  S = NULL,
  T = NULL,
  minfup = NULL,
  ratio = 1
)

# update x with gsDesign() to get integer event counts
x <- gsDesign(
  k = x$k,
  test.type = 1,
  alpha = x$alpha,
  beta = x$beta,
  sfu = x$upper$sf,
  sfupar = x$upper$param,
  n.I = ceiling(x$n.I),
  maxn.IPlan = ceiling(x$n.I[x$k]),
  delta = x$delta,
  delta1 = x$delta1,
  delta0 = x$delta0
)
y <- gsBoundSummary(x,
  ratio = 1,
  digits = 4,
  ddigits = 2,
  tdigits = 1,
  timename = "Month",
  logdelta = TRUE
)

testthat::test_that("under same number of events, compare the power", {
  out <- gs_power_ahr(
    enroll_rate = define_enroll_rate(
      duration = c(2, 2, 2, 6),
      rate = c(6, 12, 18, 24)
    ),
    fail_rate = define_fail_rate(
      duration = 1,
      fail_rate = log(2) / 9,
      hr = 0.65,
      dropout_rate = 0.001
    ),
    ratio = 1,
    # set number of events the same as the design x above from gsDesign()
    event = x$n.I,
    analysis_time = NULL,
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL, theta = 0),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2, param = NULL, timing = NULL, theta = 0),
    test_upper = TRUE,
    test_lower = FALSE
  )

  testthat::expect_equal(out$bound$probability[1:2], y$Efficacy[c(5, 10)], tolerance = 0.02)
})

testthat::test_that("under same power setting, compare the number of events", {
  out <- gs_power_ahr(
    enroll_rate = define_enroll_rate(
      duration = c(2, 2, 2, 6),
      rate = c(6, 12, 18, 24)
    ),
    fail_rate = define_fail_rate(
      duration = 1,
      fail_rate = log(2) / 9,
      dropout_rate = 0.001,
      hr = 0.65
    ),
    ratio = 1,
    event = NULL,
    # adjust the times s.t. power ~= 0.801 and information fraction ~= 0.7 (same as the design x above from gsDesign())
    analysis_time = c(21, 34.9),
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL, theta = 0),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2, param = NULL, timing = NULL, theta = 0),
    test_upper = TRUE,
    test_lower = FALSE
  )
  # in case test fails, check whether caused by small tolerance
  testthat::expect_equal(out$analysis$event[1:2], x$n.I, tolerance = 0.02)
})
