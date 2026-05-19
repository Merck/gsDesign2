# Helper functions used by test-independent-expected_event.R

n_event <- function(failRates, followup) {
  failduration <- failRates$duration
  failtime <- cumsum(failduration)
  failRate <- failRates$failRate
  dropoutRate <- failRates$dropoutRate
  lamda <- failRate + dropoutRate
  lamda1 <- c(lamda, dplyr::last(lamda))
  failRate1 <- c(failRate, dplyr::last(failRate))

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

test_expected_event <- function(enrollRates, failRates, totalDuration) {
  enrolltime <- c(0, cumsum(enrollRates$duration))
  Event <- 0
  for (i in seq_along(enrollRates$duration)) {
    enrollmentstart <- 0
    enrollmentend <- enrollRates$duration[i]
    enrollrate <- enrollRates$rate[i]
    followup <- totalDuration - enrolltime[i]
    nEventnum <- 0

    if (followup > 0 && followup <= enrollmentend) {
      nEventnum <- n_event(failRates, followup) * enrollrate
    } else if (followup > 0 && followup > enrollmentend) {
      nEventnum <- (n_event(failRates, followup) - n_event(failRates, followup - enrollmentend)) * enrollrate
    } else {
      nEventnum <- 0
    }
    Event <- Event + nEventnum
  }
  return(Event)
}

params_expected_event <- function() {
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

  list(
    "enroll_rate" = enroll_rate,
    "fail_rate" = fail_rate,
    "failRates" = failRates,
    "total_duration" = total_duration,
    "simple" = simple
  )
}
