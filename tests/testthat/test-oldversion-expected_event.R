library(dplyr)

my_path <- paste0(system.file(package = "gsDesign2"), "/old_function/")
source_files <- list.files(my_path, "*.R$")
sapply(paste0(my_path, source_files), source)

test_that("expected event vs gsDesign", {
  enroll_rate <- define_enroll_rate(duration = c(2, 1, 2), rate = c(5, 10, 20))
  fail_rate <- define_fail_rate(duration = c(1, 1, 1), fail_rate = c(.05, .02, .01), dropout_rate = .01)
  total_duration <- 20
  x1 <- gsDesign::eEvents(  # gsDesign
    lambda = fail_rate$fail_rate,
    S = fail_rate$duration[1:(nrow(fail_rate) - 1)],
    eta = fail_rate$dropout_rate,
    gamma = enroll_rate$rate,
    R = enroll_rate$duration,
    T = total_duration
  )$d
  x2 <- eEvents_df_(      # gsDesign2 old version
    enrollRates = enroll_rate %>% rename(Stratum = stratum),
    failRates = fail_rate %>% rename(Stratum = stratum, failRate = fail_rate, dropoutRate = dropout_rate),
    total_duration,
    simple = TRUE)
  x3 <- expected_event(   # gsDesign2 latest version
    enroll_rate, fail_rate, total_duration, simple = TRUE)
  expect(x1, x2)
  expect(x2, x3)
})