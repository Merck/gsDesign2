x <- gsDesign::gsSurv(
  k = 2,
  test.type = 1,
  alpha = 0.025,
  beta = 0.2,
  astar = 0,
  timing = 0.7,
  sfu = gsDesign::sfLDOF,
  sfupar = c(0),
  sfl = gsDesign::sfLDOF,
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
) |> gsDesign::toInteger()

y <- gsDesign::gsBoundSummary(
  x,
  ratio = 1,
  digits = 4,
  ddigits = 2,
  tdigits = 4,
  timename = "Month",
  logdelta = TRUE
)

res <- list("x" = x, "y" = y)

# Test 1: compare with gsDesign under proportional hazard
test_that("under same number of events, compare the power", {
  x <- res$x
  y <- res$y

  out <- gs_power_ahr(
    enroll_rate = define_enroll_rate(
      duration = x$R,
      rate = x$gamma |> as.vector()
    ),
    fail_rate = define_fail_rate(
      duration = Inf,
      fail_rate = x$lambdaC |> as.vector(),
      dropout_rate = x$etaE |> as.vector(),
      hr = x$hr
    ),
    ratio = x$ratio,
    # Set number of events the same as the design x above from gsDesign()
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

  expect_equal(out$bound$probability[1:2], y$Efficacy[c(5, 10)], tolerance = 0.02)
})

test_that("under same power setting, compare the number of events", {
  x <- res$x

  out <- gs_power_ahr(
    enroll_rate = define_enroll_rate(
      duration = x$R,
      rate = x$gamma |> as.vector()
    ),
    fail_rate = define_fail_rate(
      duration = Inf,
      fail_rate = x$lambdaC |> as.vector(),
      dropout_rate = x$etaE |> as.vector(),
      hr = x$hr
    ),
    ratio = x$ratio,
    event = NULL,
    # Adjust the times s.t. power ~= 0.801 and information fraction ~= 0.7
    # (same as the design x above from gsDesign())
    analysis_time = sub("^Month: ", "", y$Analysis[startsWith(y$Analysis, "Month: ")]) |> as.numeric(),
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL, theta = 0),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2, param = NULL, timing = NULL, theta = 0),
    test_upper = TRUE,
    test_lower = FALSE
  )

  # In case test fails, check whether caused by small tolerance
  expect_equal(out$analysis$event[1:2], x$n.I, tolerance = 0.002)
})
