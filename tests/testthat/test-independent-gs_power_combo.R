test_tEvents <- function(enroll_rate,
                         fail_rate,
                         td = 15) {
  
  enroll_rate_1 <- enroll_rate
  enroll_rate_1$rate <- enroll_rate$rate / 2
  
  fail_rate_c <- fail_rate 
  fail_rate_t <- fail_rate  
  fail_rate_t$fail_rate <- fail_rate_t$fail_rate * fail_rate_t$hr
  
  event_c <- gsDesign2::expected_event(
    enroll_rate = enroll_rate_1,
    fail_rate = fail_rate_c,
    total_duration = td,
    simple = FALSE
  )

  event_t <- gsDesign2::expected_event(
    enroll_rate = enroll_rate_1,
    fail_rate = fail_rate_t,
    total_duration = td,
    simple = FALSE
  )

  total_e <- sum(event_c$event + event_t$event)
  
  total_e
}


enroll_rate <- define_enroll_rate(duration = 12, rate = 500 / 12)

fail_rate <- define_fail_rate(
  duration = c(4, 100),
  fail_rate = log(2) / 15, # median survival 15 month
  dropout_rate = 0.001,
  hr = c(1, .6)
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
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
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
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      td = unique(fh_test$analysis_time)[i]
    )
    expect_equal(event, unique(gs_power_combo_test1$analysis$event)[i], tolerance = 0.01)
  })
}


# Minimal Information Fraction derived bound
gs_power_combo_test2 <- gsDesign2::gs_power_combo(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
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
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      td = unique(fh_test$analysis_time)[i]
    )
    expect_equal(event, unique(gs_power_combo_test2$analysis$event)[i], tolerance = 0.01)
  })
}
