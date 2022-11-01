test_tEvents<-function(enrollRates = tibble::tibble(Stratum = "All",
                                                    duration = c(2, 2, 10),
                                                    rate = c(3, 6, 9) * 5),
                       failRates = tibble::tibble(Stratum = "All",
                                                  duration = c(3, 100),
                                                  failRate = log(2) / c(9, 18),
                                                  hr = c(.9, .6),
                                                  dropoutRate = rep(.001, 2)),
                       td = 15
){
  enrollRates_1 = enrollRates
  enrollRates_1$rate = enrollRates$rate/2
  failRatesc = failRates[,c("duration", "failRate", "dropoutRate")]
  failRatest= failRatesc
  failRatest$failRate = failRates$failRate*failRates$hr
  eventc = gsDesign2::eEvents_df(enrollRates=enrollRates_1,
                                 failRates = failRatesc,
                                 totalDuration = td,
                                 simple = FALSE)
  eventt = gsDesign2::eEvents_df(enrollRates=enrollRates_1,
                                 failRates=failRatest,
                                 totalDuration=td,
                                 simple=FALSE)
  totale = sum(eventc$Events+eventt$Events)
  return(totale)
}


enrollRates <- tibble::tibble(Stratum = "All", duration = 12, rate = 500/12)

failRates <- tibble::tibble(Stratum = "All",
                            duration = c(4, 100),
                            failRate = log(2) / 15,  # median survival 15 month
                            hr = c(1, .6),
                            dropoutRate = 0.001)

fh_test <- rbind( data.frame(rho = 0,
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

# User defined bound
gs_power_combo_test1 <- gsDesign2::gs_power_combo(enrollRates = enrollRates,
                                                  failRates = failRates,
                                                  fh_test = fh_test,
                                                  upper = gs_b, upar = c(3, 2, 1),
                                                  lower = gs_b, lpar = c(-1, 0, 1))

test_that ( "calculate analysis number as planed",{
  expect_equal(max(fh_test$Analysis), max(gs_power_combo_test1$analysis$Analysis ))
})

test_that ( "calculate analysisTimes as planed",{
  expect_equal(unique(fh_test$analysisTimes), unique(gs_power_combo_test1$analysis$Time))
})


for (i in 1:max(fh_test$Analysis)) {
  test_that ( "calculate N and each analysis Events N as planed",{
    event <- test_tEvents(enrollRates = enrollRates,
                          failRates = failRates,
                          td = unique(fh_test$analysisTimes)[i])
    expect_equal(event, unique(gs_power_combo_test1$analysis$Events)[i], tolerance = 0.01)
  })
}


# Minimal Information Fraction derived bound
gs_power_combo_test2 <- gsDesign2::gs_power_combo(enrollRates, failRates, fh_test,
                                       upper = gs_spending_combo,
                                       upar  = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
                                       lower = gs_spending_combo,
                                       lpar  = list(sf = gsDesign::sfLDOF, total_spend = 0.2))

test_that ( "calculate analysis number as planed",{
  expect_equal(max(fh_test$Analysis), max(gs_power_combo_test2$analysis$Analysis))
})

test_that ( "calculate analysisTimes as planed",{
  expect_equal(unique(fh_test$analysisTimes), unique(gs_power_combo_test2$analysis$Time))
})

for (i in 1:max(fh_test$Analysis)) {
  test_that ( "calculate N and each analysis Events N as planed",{
    event <- test_tEvents(enrollRates = enrollRates,
                          failRates = failRates,
                          td = unique(fh_test$analysisTimes)[i])
    expect_equal(event, unique(gs_power_combo_test2$analysis$Events)[i], tolerance = 0.01)
  })
}