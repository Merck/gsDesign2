nEvent = function(followup) {
  failduration = failRates$duration
  failtime = cumsum(failduration)
  failRate = failRates$failRate
  dropoutRate = failRates$dropoutRate
  lamda = failRate + dropoutRate
  lamda1 = c(lamda, last(lamda))
  failRate1 = c(failRate, last(failRate))

  failtimeend = c(0, failtime[failtime < followup], followup)
  failtimeend1 = c(failtime[failtime < followup], followup)
  lamda2 = lamda1[c(1:(length(failtimeend) - 1))]
  failRate2 = failRate1[c(1:(length(failtimeend) - 1))]

  failduration = diff(failtimeend)
  failduration2 = followup - failtimeend1

  fail = lamda2 * failduration
  sumfail = cumsum(fail)
  Bi1 = c(1, exp(-sumfail))
  diffbi = diff(Bi1)
  Bi = Bi1[c(1:(length(Bi1) - 1))]

  totalevent = diffbi * (1 / lamda2 - failduration2) + Bi * failduration

  failevent = totalevent * (failRate2 / lamda2)
  return(sum(failevent))
}

test_Event = function(enrollRates, failRates, totalDuration) {
  enrolltime = c(0, cumsum(enrollRates$duration))
  Event = 0
  for (i in c(1:length(enrollRates$duration))) {
    enrollmentstart = 0
    enrollmentend = enrollRates$duration[i]
    enrollrate = enrollRates$rate[i]
    followup = totalDuration - enrolltime[i]
    nEventnum = 0

    if (followup > 0 && followup <= enrollmentend) {
      nEventnum = nEvent(followup) * enrollrate
    } else if (followup > 0 && followup > enrollmentend) {
      nEventnum = (nEvent(followup) - nEvent(followup - enrollmentend)) * enrollrate
    } else {
      nEventnum = 0
    }
    Event = Event + nEventnum
  }
  return(Event)
}

enrollRates = tibble::tibble(duration = c(50),
                             rate = c(10))
failRates = tibble::tibble(duration = c(10, 20, 10),
                           failRate = log(2) / c(5, 10, 5),
                           dropoutRate = c(0.1, 0.2, 0))
totalDuration = 5
simple = TRUE

###test1:with mutiple failrates, short FU
testthat::test_that(
  "expected events is different from double-programmed vs eEvents_df, with mutiple failrates, short FU",{
    testthat::expect_equal(
      test_Event(enrollRates, failRates, totalDuration),
      eEvents_df(enrollRates, failRates, totalDuration, simple)
    )
  })

###test2:with mutiple failrates, long FU
testthat::test_that("expected events is different from double-programmed vs eEvents_df,
                    with mutiple failrates, long FU",{
                      totalDuration=80
                      testthat::expect_equal(test_Event(enrollRates, failRates, totalDuration),
                                             eEvents_df(enrollRates, failRates, totalDuration, simple))
                      })

###test3:with mutiple failrates and with mutiple enrollment duration
testthat::test_that("expected events is different from double-programmed vs eEvents_df,
                    with mutiple enrollment duration",{
                      enrollRates=tibble::tibble(duration=c(50,10),
                                                 rate=c(10,5))
                      failRates=tibble::tibble(duration=c(10,20,10),
                                               failRate=log(2)/c(5,10,5),
                                               dropoutRate=c(0.1,0.2,0))
                      totalDuration=80
                      testthat::expect_equal(test_Event(enrollRates, failRates, totalDuration),
                                             eEvents_df(enrollRates, failRates, totalDuration, simple))
                      })

