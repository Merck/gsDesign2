load("./fixtures/sim_gsd_pMaxCombo_exp1_H0_test.Rdata")
load("./fixtures/sim_gsd_pMaxCombo_exp1_H1_test.Rdata")

ratio <- 1
algorithm <- GenzBretz(maxpts = 1e5, 
                       abseps = 1e-5)
alpha <- 0.025
beta <- 0.2
enrollRates <- tibble::tibble(Stratum = "All", 
                              duration = 12, 
                              rate = 500/12)

failRates <- tibble::tibble(Stratum = "All",
                            duration = c(4, 100),
                            failRate = log(2) / 15,  # median survival 15 month
                            hr = c(1, .6),
                            dropoutRate = 0.001)

fh_test <- rbind(data.frame(rho = 0, 
                            gamma = 0, 
                            tau = -1,
                            test = 1,
                            Analysis = 1:3,
                            analysisTimes = c(12, 24, 36)),
                 data.frame(rho = c(0, 0.5),
                            gamma = 0.5, 
                            tau = -1,
                            test = 2:3,
                            Analysis = 3, 
                            analysisTimes = 36))

x <- gsDesign::gsSurv(k = 3, 
                      test.type = 4, 
                      alpha = 0.025,
                      beta = 0.2, 
                      astar = 0, 
                      timing = c( 1 ),
                      sfu = sfLDOF , 
                      sfupar = c( 0 ), 
                      sfl = sfLDOF ,
                      sflpar = c( 0 ), 
                      lambdaC = c( 0.1 ) ,
                      hr = 0.6 , 
                      hr0 = 1 , 
                      eta = 0.01 ,
                      gamma = c( 10 ) ,
                      R = c( 12 ) , 
                      S = NULL ,
                      T = 36 , 
                      minfup = 24 , 
                      ratio = 1 )


# User defined boundary
gs_design_combo_test1 <- gs_design_combo(enrollRates,
                                         failRates,
                                         fh_test,
                                         alpha = alpha,
                                         beta = beta,
                                         ratio = 1,
                                         binding = FALSE,        # test.type = 4 non-binding futility bound
                                         upar = x$upper$bound,
                                         lpar = x$lower$bound)
#### Boundary derived by spending function testing
gs_design_combo_test2 <- gs_design_combo(enrollRates,
                                         failRates,
                                         fh_test,
                                         alpha = 0.025,
                                         beta = 0.2,
                                         ratio = 1,
                                         binding = FALSE,                 # test.type = 4 non-binding futility bound
                                         upper = gs_spending_combo,
                                         upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),   # alpha spending
                                         lower = gs_spending_combo,
                                         lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2),     # beta spending
)

test_tEvents <- function(enrollRates = tibble::tibble(Stratum = "All",
                                                      duration = c(2, 2, 10),
                                                      rate = c(3, 6, 9) * 5),
                         failRates = tibble::tibble(Stratum = "All",
                                                    duration = c(3, 100),
                                                    failRate = log(2) / c(9, 18),
                                                    hr = c(.9, .6),
                                                    dropoutRate = rep(.001, 2)),
                         td = 15
){
  enrollRates_1 <- enrollRates
  enrollRates_1$rate <- enrollRates$rate/2
  failRatesc <- failRates[,c("duration","failRate","dropoutRate")]
  failRatest <- failRatesc
  failRatest$failRate <- failRates$failRate*failRates$hr
  eventc <- gsDesign2::eEvents_df(enrollRates = enrollRates_1,
                                 failRates = failRatesc,
                                 totalDuration = td,
                                 simple = FALSE)
  eventt <- gsDesign2::eEvents_df(enrollRates = enrollRates_1,
                                 failRates = failRatest,
                                 totalDuration = td,
                                 simple = FALSE)
  totale <- sum(eventc$Events+eventt$Events)
  return(totale)
}



test_that("calculate analysis number as planed",{
  expect_equal(max(fh_test$Analysis), max(gs_design_combo_test1$Analysis ))
})
test_that("calculate analysisTimes as planed",{
  expect_equal(unique(fh_test$analysisTimes), unique(gs_design_combo_test1$Time))
})

for (i in 1:max(fh_test$Analysis)) {
  test_that ("calculate N and each analysis Events N as planed",{
    event <- test_tEvents(enrollRates = enrollRates,
                          failRates = failRates,
                          td = unique(fh_test$analysisTimes)[i])
    enrollsum = enrollRates$duration * enrollRates$rate
    N = max(gs_design_combo_test1$N)
    expect_equal(event*N/enrollsum, unique(gs_design_combo_test1$Events)[i], tolerance = 0.01)
  })
}

testthat::test_that("calculate uppder bound as planed",{
  expect_equal(x$upper$bound, (gs_design_combo_test1 %>% filter(Bound == "Upper"))$Z)
})
testthat::test_that("calculate lower bound as planed",{
  expect_equal(x$lower$bound, (gs_design_combo_test1 %>% filter(Bound == "Lower"))$Z)
})
testthat::test_that("calculate probability under alternative",{
  expect_equal(1 - beta, max(gs_design_combo_test1$Probability), tolerance = 0.0001)
})


###### test IA by simulation under H0 ########
for (i in 1:max(fh_test$Analysis)) {
  testthat::test_that("compare probability under H0 for efficacy",{
    expect_equal(sim_gsd_pMaxCombo_exp1_H0_test$upper[i], gs_design_combo_test1$Probability_Null[i], tolerance = 0.01)
  })
}

####### test IA by simulation under H1 ########
for (i in 1:max(fh_test$Analysis)) {
  testthat::test_that("compare probability under H1 for efficacy",{
    expect_equal(sim_gsd_pMaxCombo_exp1_H1_test$upper[i], gs_design_combo_test1$Probability[i], tolerance = 0.01)
  })
}

for (i in 1:max(fh_test$Analysis)) {
  testthat::test_that("compare probability under H1 for futility",{
    expect_equal(sim_gsd_pMaxCombo_exp1_H1_test$lower[i], gs_design_combo_test1$Probability[i+max(fh_test$Analysis)], tolerance=0.01)
  })
}

testthat::test_that("calculate analysis number as planed",{
  expect_equal(max(fh_test$Analysis), max(gs_design_combo_test2$Analysis ))
})
testthat::test_that("calculate analysisTimes as planed",{
  expect_equal(unique(fh_test$analysisTimes), unique(gs_design_combo_test2$Time))
})

for (i in 1:max(fh_test$Analysis)) {
  testthat::test_that("calculate N and each analysis Events N as planed",{
    event <- test_tEvents(enrollRates = enrollRates,
                          failRates = failRates,
                          td = unique(fh_test$analysisTimes)[i])
    enrollsum = enrollRates$duration*enrollRates$rate
    N = max(gs_design_combo_test2$N)
    expect_equal(event*N/enrollsum, unique(gs_design_combo_test2$Events)[i], tolerance=0.01)
  })
}
testthat::test_that("calculate probability under alternative",{
  expect_equal(1 - beta, max(gs_design_combo_test2$Probability), tolerance = 0.0001)
})
testthat::test_that("calculate probability under null",{
  expect_equal(alpha, max((gs_design_combo_test2 %>% filter(Bound == "Upper"))$Probability_Null), tolerance = 0.0001)
})