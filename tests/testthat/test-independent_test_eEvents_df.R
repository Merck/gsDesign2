
testthat::test_that("expected events is different from gsDesign::eEvents and eEvents_df",{
  enrollRates <- tibble::tibble(duration=c(2,1,2),rate=c(5,10,20))
  failRates <- tibble::tibble(duration=c(1,1,1),failRate=c(.05,.02,.01),dropoutRate=.01)
  totalDuration <- 20
  testthat::expect_equal(gsDesign2::eEvents_df(enrollRates,failRates,totalDuration,simple=TRUE),
                         gsDesign::eEvents(lambda=failRates$failRate,S=failRates$duration[1:(nrow(failRates)-1)],
                                           eta=failRates$dropoutRate,gamma=enrollRates$rate,
                                           R=enrollRates$duration,T=totalDuration)$d,
                         ignore_attr = TRUE)
})
testthat::test_that("data frame returned from eEvents_df not as expected",{
  # test case from gsSurvNPH
  enrollRates <-tibble::tibble(duration=c(1,1,8),rate=c(3,2,0))
  failRates <- tibble::tibble(duration=c(4,Inf),failRate=c(.03,.06),dropoutRate=c(.001,.002))
  totalDuration <- 7
  xx <- gsDesign2::eEvents_df(enrollRates,failRates,totalDuration,simple=FALSE) %>% data.frame()
  # expected checked with alternate calculations in gsSurvNPH vignette
  expected <- data.frame(t=c(0,4),
                         failRate=c(0.03,0.06),
                         Events=c(0.5642911, 0.5194821))
  testthat::expect_equal(xx,expected)
})
