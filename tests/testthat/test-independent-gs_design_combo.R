load("fixtures/sim_gsd_pMaxCombo_exp1_H0_test.Rdata")
load("fixtures/sim_gsd_pMaxCombo_exp1_H1_test.Rdata")

ratio <- 1
algorithm <- GenzBretz(
  maxpts = 1e5,
  abseps = 1e-5
)
alpha <- 0.025
beta <- 0.2
enroll_rate <- define_enroll_rate(
  duration = 12,
  rate = 500 / 12
)

fail_rate <- define_fail_rate(
  duration = c(4, 100),
  fail_rate = log(2) / 15, # median survival 15 month
  dropout_rate = 0.001,
  hr = c(1, .6)
)

fh_test <- rbind(
  data.frame(
    rho = 0,
    gamma = 0,
    tau = -1,
    test = 1,
    analysis = 1:3,
    analysis_time = c(12, 24, 36)
  ),
  data.frame(
    rho = c(0, 0.5),
    gamma = 0.5,
    tau = -1,
    test = 2:3,
    analysis = 3,
    analysis_time = 36
  )
)

x <- gsDesign::gsSurv(
  k = 3,
  test.type = 4,
  alpha = 0.025,
  beta = 0.2,
  astar = 0,
  timing = c(1),
  sfu = sfLDOF,
  sfupar = c(0),
  sfl = sfLDOF,
  sflpar = c(0),
  lambdaC = c(0.1),
  hr = 0.6,
  hr0 = 1,
  eta = 0.01,
  gamma = c(10),
  R = c(12),
  S = NULL,
  T = 36,
  minfup = 24,
  ratio = 1
)

# User-defined boundary
gs_design_combo_test1 <- gs_design_combo(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  fh_test = fh_test,
  alpha = alpha,
  beta = beta,
  ratio = 1,
  binding = FALSE, # test.type = 4 non-binding futility bound
  upar = x$upper$bound,
  lpar = x$lower$bound
)

#### Boundary derived by spending function testing
gs_design_combo_test2 <- gs_design_combo(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  fh_test = fh_test,
  alpha = 0.025,
  beta = 0.2,
  ratio = 1,
  binding = FALSE, # test.type = 4 non-binding futility bound
  upper = gs_spending_combo,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025), # alpha spending
  lower = gs_spending_combo,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2), # beta spending
)

testthat::test_that("calculate analysis number as planned", {
  expect_equal(max(fh_test$analysis), max(gs_design_combo_test2$analysis$analysis))
})
testthat::test_that("calculate analysisTimes as planned", {
  expect_equal(unique(fh_test$analysis_time), unique(gs_design_combo_test2$analysis$time))
})

for (i in 1:max(fh_test$analysis)) {
  testthat::test_that("calculate N and each analysis Events N as planned", {
    event <- test_event(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      td = unique(fh_test$analysis_time)[i]
    )
    enrollsum <- enroll_rate$duration * enroll_rate$rate
    N <- max(gs_design_combo_test2$analysis$n)
    expect_equal(event * N / enrollsum, unique(gs_design_combo_test2$analysis$event)[i], tolerance = 0.01)
  })
}

testthat::test_that("calculate probability under alternative", {
  expect_equal(
    1 - beta,
    max((gs_design_combo_test2$bounds %>% filter(bound == "upper"))$probability),
    tolerance = 0.0001
  )
})

testthat::test_that("calculate probability under null", {
  expect_equal(
    alpha,
    max((gs_design_combo_test2$bounds %>% filter(bound == "upper"))$probability0),
    tolerance = 0.005
  )
})
