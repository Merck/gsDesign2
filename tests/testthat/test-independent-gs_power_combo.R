test_tEvents <- function(enrollRates = tibble::tibble(
                           Stratum = "All",
                           duration = c(2, 2, 10),
                           rate = c(3, 6, 9) * 5
                         ),
                         failRates = tibble::tibble(
                           Stratum = "All",
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


enrollRates <- tibble::tibble(Stratum = "All", duration = 12, rate = 500 / 12)

failRates <- tibble::tibble(
  Stratum = "All",
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

# User defined bound
gs_power_combo_test1 <- gsDesign2::gs_power_combo(
  enrollRates = enrollRates %>% dplyr::rename(stratum = Stratum),
  failRates = failRates %>% dplyr::rename(stratum = Stratum, fail_rate = failRate, dropout_rate = dropoutRate),
  fh_test = fh_test,
  upper = gs_b, upar = c(3, 2, 1),
  lower = gs_b, lpar = c(-1, 0, 1)
)

test_that("calculate analysis number as planed", {
  expect_equal(max(fh_test$analysis), max(gs_power_combo_test1$analysis$analysis))
})

test_that("calculate analysisTimes as planed", {
  expect_equal(unique(fh_test$analysis_time), unique(gs_power_combo_test1$analysis$time))
})


for (i in 1:max(fh_test$analysis)) {
  test_that("calculate N and each analysis Events N as planed", {
    event <- test_tEvents(
      enrollRates = enrollRates,
      failRates = failRates,
      td = unique(fh_test$analysis_time)[i]
    )
    expect_equal(event, unique(gs_power_combo_test1$analysis$event)[i], tolerance = 0.01)
  })
}


# Minimal Information Fraction derived bound
gs_power_combo_test2 <- gsDesign2::gs_power_combo(enrollRates %>% dplyr::rename(stratum = Stratum),
  failRates %>% dplyr::rename(stratum = Stratum, fail_rate = failRate, dropout_rate = dropoutRate),
  fh_test,
  upper = gs_spending_combo,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lower = gs_spending_combo,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2)
)

test_that("calculate analysis number as planed", {
  expect_equal(max(fh_test$analysis), max(gs_power_combo_test2$analysis$analysis))
})

test_that("calculate analysisTimes as planed", {
  expect_equal(unique(fh_test$analysis_time), unique(gs_power_combo_test2$analysis$time))
})

for (i in 1:max(fh_test$analysis)) {
  test_that("calculate N and each analysis Events N as planed", {
    event <- test_tEvents(
      enrollRates = enrollRates,
      failRates = failRates,
      td = unique(fh_test$analysis_time)[i]
    )
    expect_equal(event, unique(gs_power_combo_test2$analysis$event)[i], tolerance = 0.01)
  })
}
