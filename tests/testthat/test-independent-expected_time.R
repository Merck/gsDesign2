# directly compare the results of expected_time with expected_event.
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
  
  eventc <- expected_event(enroll_rate=enrollRates_1 %>% dplyr::rename(stratum = Stratum),
                           fail_rate=failRatesc %>% dplyr::rename(fail_rate = failRate, dropout_rate = dropoutRate),
                           total_duration=td,
                           simple=FALSE)
  eventt <- expected_event(enroll_rate=enrollRates_1 %>% dplyr::rename(stratum = Stratum),
                           fail_rate=failRatest %>% dplyr::rename(fail_rate = failRate, dropout_rate = dropoutRate),
                           total_duration=td,
                           simple=FALSE)
  totale=sum(eventc$Events+eventt$Events)
  return(totale)
}

testthat::test_that("expected_time does not equal to eEvent_df's result", {
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
  
  t1 = expected_time(enroll_rate = enrollRates %>% dplyr::rename(stratum = Stratum),
             fail_rate = failRates %>% dplyr::rename(stratum = Stratum, fail_rate = failRate, dropout_rate = dropoutRate),
             target_event = targetEvents,
             interval = interval)
  
  testthat::expect_equal(
    t1$Events,
    test_tEvents(enrollRates=enrollRates,
                 failRates=failRates,
                 td=t1$Time)
  )
})

testthat::test_that("expected_time does not euqal to AHR's result",{
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
  t1=expected_time(enroll_rate = enrollRates %>% dplyr::rename(stratum = Stratum),
             fail_rate = failRates %>% dplyr::rename(stratum = Stratum, fail_rate = failRate, dropout_rate = dropoutRate),
             target_event = targetEvents,
             interval = interval)
  
  testthat::expect_equal(
    t1$Events,
    AHR(enroll_rate=enrollRates %>% dplyr::rename(stratum = Stratum),
        fail_rate=failRates %>% dplyr::rename(stratum = Stratum, fail_rate = failRate, dropout_rate = dropoutRate),
        total_duration=t1$Time,
        ratio=1,
        simple = TRUE)$Events
  )
})
