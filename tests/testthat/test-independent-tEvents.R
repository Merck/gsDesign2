# directly compare the results of tEvents with eEvents_df.
test_tEvents<-function(enrollRates = tibble::tibble(Stratum = "All",
                                                    duration = c(2, 2, 10),
                                                    rate = c(3, 6, 9) * 5),
                       failRates = tibble::tibble(Stratum = "All",
                                                  duration = c(3, 100),
                                                  failRate = log(2) / c(9, 18),
                                                  hr = c(.9, .6),
                                                  dropoutRate = rep(.001, 2)),
                       td=14.9
){
  enrollRates_1=enrollRates
  enrollRates_1$rate=enrollRates$rate/2
  failRatesc=failRates[,c("duration","failRate","dropoutRate")]
  failRatest=failRatesc
  failRatest$failRate=failRates$failRate*failRates$hr
  eventc=eEvents_df(enrollRates=enrollRates_1,
                    failRates=failRatesc,
                    totalDuration=td,
                    simple=FALSE)
  eventt=eEvents_df(enrollRates=enrollRates_1,
                    failRates=failRatest,
                    totalDuration=td,
                    simple=FALSE)
  totale=sum(eventc$Events+eventt$Events)
  return(totale)
}

testthat::test_that("tEvents does not equal to eEvent_df's result", {
  enrollRates = tibble::tibble(Stratum = "All",
                               duration = c(2, 2, 10),
                               rate = c(3, 6, 9) * 5)
  
  failRates = tibble::tibble(Stratum = "All",
                             duration = c(3, 100),
                             failRate = log(2) / c(9, 18),
                             hr = c(.9, .6),
                             dropoutRate = rep(.001, 2))
  targetEvents = 150
  interval = c(.01, 100)
  t1=tEvents(enrollRates = enrollRates,
             failRates = failRates,
             targetEvents = targetEvents,
             interval = interval)
  testthat::expect_equal(
    t1$Events,
    test_tEvents(enrollRates=enrollRates,
                 failRates=failRates,
                 td=t1$Time)
  )
})

testthat::test_that("tEvents does not euqal to AHR's result",{
  enrollRates = tibble::tibble(Stratum = "All",
                               duration = c(2, 2, 10),
                               rate = c(3, 6, 9) * 5)
  failRates = tibble::tibble(Stratum = "All",
                             duration = c(3, 100),
                             failRate = log(2) / c(9, 18),
                             hr = c(.9, .6),
                             dropoutRate = rep(.001, 2))
  targetEvents = 150
  interval = c(.01, 100)
  t1=tEvents(enrollRates = enrollRates,
             failRates = failRates,
             targetEvents = targetEvents,
             interval = interval)
  
  testthat::expect_equal(
    t1$Events,
    AHR(enrollRates=enrollRates,
        failRates=failRates,
        totalDuration=t1$Time,
        ratio=1,
        simple = TRUE)$Events
  )
})