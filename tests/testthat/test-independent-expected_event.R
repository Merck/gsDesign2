library(dplyr)

test_that("expected events is different from gsDesign::eEvents and expected_event", {
  enroll_rate <- define_enroll_rate(duration = c(2, 1, 2), rate = c(5, 10, 20))
  fail_rate <- define_fail_rate(duration = c(1, 1, 1), fail_rate = c(.05, .02, .01), hr = 1, dropout_rate = .01)
  total_duration <- 20
  testthat::expect_equal(
    expected_event(
      enroll_rate,
      fail_rate,
      total_duration,
      simple = TRUE
    ),
    gsDesign::eEvents(
      lambda = fail_rate$fail_rate, S = fail_rate$duration[1:(nrow(fail_rate) - 1)],
      eta = fail_rate$dropout_rate, gamma = enroll_rate$rate,
      R = enroll_rate$duration, T = total_duration
    )$d,
    ignore_attr = TRUE
  )
})

test_that("data frame returned from expected_event not as expected", {
  # test case from gsSurvNPH
  enroll_rate <- define_enroll_rate(duration = c(1, 1, 8), rate = c(3, 2, 0))
  fail_rate <- define_fail_rate(duration = c(4, Inf), fail_rate = c(.03, .06), dropout_rate = c(.001, .002), hr = 1)
  total_duration <- 7

  xx <- expected_event(
    enroll_rate,
    fail_rate,
    total_duration,
    simple = FALSE
  ) %>%
    data.frame()
  # expected checked with alternate calculations in gsSurvNPH vignette
  expected <- data.frame(
    t = c(0, 4),
    fail_rate = c(0.03, 0.06),
    event = c(0.5642911, 0.5194821)
  )
  testthat::expect_equal(xx, expected)
})

# Double programming tests
nEvent <- function(followup) {
  failduration <- failRates$duration
  failtime <- cumsum(failduration)
  failRate <- failRates$failRate
  dropoutRate <- failRates$dropoutRate
  lamda <- failRate + dropoutRate
  lamda1 <- c(lamda, last(lamda))
  failRate1 <- c(failRate, last(failRate))

  failtimeend <- c(0, failtime[failtime < followup], followup)
  failtimeend1 <- c(failtime[failtime < followup], followup)
  lamda2 <- lamda1[c(1:(length(failtimeend) - 1))]
  failRate2 <- failRate1[c(1:(length(failtimeend) - 1))]

  failduration <- diff(failtimeend)
  failduration2 <- followup - failtimeend1

  fail <- lamda2 * failduration
  sumfail <- cumsum(fail)
  Bi1 <- c(1, exp(-sumfail))
  diffbi <- diff(Bi1)
  Bi <- Bi1[c(1:(length(Bi1) - 1))]

  totalevent <- diffbi * (1 / lamda2 - failduration2) + Bi * failduration

  failevent <- totalevent * (failRate2 / lamda2)
  return(sum(failevent))
}

test_Event <- function(enrollRates, failRates, totalDuration) {
  enrolltime <- c(0, cumsum(enrollRates$duration))
  Event <- 0
  for (i in seq_along(enrollRates$duration)) {
    enrollmentstart <- 0
    enrollmentend <- enrollRates$duration[i]
    enrollrate <- enrollRates$rate[i]
    followup <- totalDuration - enrolltime[i]
    nEventnum <- 0

    if (followup > 0 && followup <= enrollmentend) {
      nEventnum <- nEvent(followup) * enrollrate
    } else if (followup > 0 && followup > enrollmentend) {
      nEventnum <- (nEvent(followup) - nEvent(followup - enrollmentend)) * enrollrate
    } else {
      nEventnum <- 0
    }
    Event <- Event + nEventnum
  }
  return(Event)
}

enroll_rate <- define_enroll_rate(
  duration = c(50),
  rate = c(10)
)

fail_rate <- define_fail_rate(
  duration = c(10, 20, 10),
  fail_rate = log(2) / c(5, 10, 5),
  dropout_rate = c(0.1, 0.2, 0),
  hr = 1
)
fail_rate$failRate <- fail_rate$fail_rate
fail_rate$dropoutRate <- fail_rate$dropout_rate
failRates <- fail_rate

total_duration <- 5
simple <- TRUE

### test1:with mutiple failrates, short FU
testthat::test_that(
  "expected events is different from double-programmed vs expected_event, with mutiple failrates, short FU",
  {
    testthat::expect_equal(
      test_Event(enroll_rate, fail_rate, total_duration),
      expected_event(
        enroll_rate,
        fail_rate,
        total_duration,
        simple
      )
    )
  }
)

### test2:with mutiple failrates, long FU
testthat::test_that(
  "expected events is different from double-programmed vs expected_event, with mutiple failrates, long FU",
  {
    total_duration <- 80
    testthat::expect_equal(
      test_Event(enroll_rate, fail_rate, total_duration),
      expected_event(
        enroll_rate,
        fail_rate,
        total_duration,
        simple
      )
    )
  }
)

### test3:with mutiple failrates and with multiple enrollment duration
testthat::test_that(
  "expected events is different from double-programmed vs expected_event, with mutiple enrollment duration",
  {
    enrollRates <- define_enroll_rate(
      duration = c(50, 10),
      rate = c(10, 5)
    )
    failRates <- define_fail_rate(
      duration = c(10, 20, 10),
      fail_rate = log(2) / c(5, 10, 5),
      dropout_rate = c(0.1, 0.2, 0),
      hr = 1
    )

    fail_rate$failRate <- fail_rate$fail_rate
    fail_rate$dropoutRate <- fail_rate$dropout_rate
    failRates <- fail_rate

    total_duration <- 80
    testthat::expect_equal(
      test_Event(enroll_rate, fail_rate, total_duration),
      expected_event(
        enroll_rate,
        fail_rate,
        total_duration,
        simple
      )
    )
  }
)
