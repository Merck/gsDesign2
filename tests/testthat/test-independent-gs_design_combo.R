load("fixtures/sim_gsd_pMaxCombo_exp1_H0_test.Rdata")
load("fixtures/sim_gsd_pMaxCombo_exp1_H1_test.Rdata")

ratio <- 1
algorithm <- GenzBretz(
  maxpts = 1e5,
  abseps = 1e-5
)
alpha <- 0.025
beta <- 0.2
enrollRates <- tibble::tibble(
  stratum = "All",
  duration = 12,
  rate = 500 / 12
)

failRates <- tibble::tibble(
  stratum = "All",
  duration = c(4, 100),
  failRate = log(2) / 15, # median survival 15 month
  hr = c(1, .6),
  dropoutRate = 0.001
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


# User defined boundary
gs_design_combo_test1 <- gs_design_combo(enrollRates,
  failRates %>% dplyr::rename(fail_rate = failRate, dropout_rate = dropoutRate),
  fh_test,
  alpha = alpha,
  beta = beta,
  ratio = 1,
  binding = FALSE, # test.type = 4 non-binding futility bound
  upar = x$upper$bound,
  lpar = x$lower$bound
)

#### Boundary derived by spending function testing
gs_design_combo_test2 <- gs_design_combo(enrollRates,
  failRates %>% dplyr::rename(fail_rate = failRate, dropout_rate = dropoutRate),
  fh_test,
  alpha = 0.025,
  beta = 0.2,
  ratio = 1,
  binding = FALSE, # test.type = 4 non-binding futility bound
  upper = gs_spending_combo,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025), # alpha spending
  lower = gs_spending_combo,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2), # beta spending
)

test_tEvents <- function(enrollRates = tibble::tibble(
                           stratum = "All",
                           duration = c(2, 2, 10),
                           rate = c(3, 6, 9) * 5
                         ),
                         failRates = tibble::tibble(
                           stratum = "All",
                           duration = c(3, 100),
                           failRate = log(2) / c(9, 18),
                           hr = c(.9, .6),
                           dropoutRate = rep(.001, 2)
                         ),
                         td = 15) {
  enrollRates_1 <- enrollRates
  enrollRates_1$rate <- enrollRates$rate / 2
  failRatesc <- failRates[, c("duration", "failRate", "dropoutRate")]
  failRatest <- failRatesc
  failRatest$failRate <- failRates$failRate * failRates$hr
  eventc <- gsDesign2::expected_event(
    enroll_rate = enrollRates_1,
    fail_rate = failRatesc %>% dplyr::rename(fail_rate = failRate, dropout_rate = dropoutRate),
    total_duration = td,
    simple = FALSE
  )
  eventt <- gsDesign2::expected_event(
    enroll_rate = enrollRates_1,
    fail_rate = failRatest %>% dplyr::rename(fail_rate = failRate, dropout_rate = dropoutRate),
    total_duration = td,
    simple = FALSE
  )
  totale <- sum(eventc$event + eventt$event)
  return(totale)
}

testthat::test_that("calculate analysis number as planed", {
  expect_equal(max(fh_test$analysis), max(gs_design_combo_test2$analysis$analysis))
})
testthat::test_that("calculate analysisTimes as planed", {
  expect_equal(unique(fh_test$analysis_time), unique(gs_design_combo_test2$analysis$time))
})

for (i in 1:max(fh_test$analysis)) {
  testthat::test_that("calculate N and each analysis Events N as planed", {
    event <- test_tEvents(
      enrollRates = enrollRates,
      failRates = failRates,
      td = unique(fh_test$analysis_time)[i]
    )
    enrollsum <- enrollRates$duration * enrollRates$rate
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
