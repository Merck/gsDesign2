test_AHR <- function(enrollRates = tibble::tibble(Stratum = "All", duration = c(2, 2, 10), rate = c(3, 6, 9)),
                   failRates = tibble::tibble(Stratum = "All",duration = c(3, 100), failRate = log(2)/c(9, 18),
                                              hr = c(0.9, 0.6), dropoutRate = rep(0.001, 2)),
                   totalDuration = 30,
                   ratio = 1,
                   simple = TRUE){

  Bystratum <- NULL
  AHR_allstrata <- NULL
  for (Omega in unique(totalDuration)){
    bystratum <- NULL
    ahr_allstrata <- NULL
    for (stratum in unique(enrollRates$Stratum)){
      enrollRates.e <- enrollRates.c <- enrollRates[enrollRates$Stratum==stratum,]
      enrollRates.e$rate <- enrollRates.e$rate*ratio/(1+ratio)
      enrollRates.c$rate <- enrollRates.c$rate/(1+ratio)

      failRates.c <- failRates.e <- failRates[failRates$Stratum==stratum,]
      failRates.e$failRate <- failRates.e$failRate*failRates.e$hr

      Events.e <- eEvents_df(enrollRates = enrollRates.e, failRates = failRates.e, totalDuration = Omega, simple = FALSE)
      Events.c <- eEvents_df(enrollRates = enrollRates.c, failRates = failRates.c, totalDuration = Omega, simple = FALSE)

      names(Events.e)[2:3] <- c('failRate.e','Events.e')
      names(Events.c)[2:3] <- c('failRate.c','Events.c')
      Events <- cbind(Events.e[,1:3],Events.c[,2:3])
      Events$lnhr <- log(Events$failRate.e/Events$failRate.c)
      Events$Events <- Events$Events.e + Events$Events.c

      ahr <- tibble::tibble(Stratum=stratum,
                            Time=Omega,
                            avehr=sum(Events$lnhr*Events$Events)/sum(Events$Events),
                            info=sum(1/(1/Events$Events.e+1/Events$Events.c)),
                            info0=sum(Events$Events*ratio/(1+ratio)^2),
                            Events=sum(Events$Events))

      ahr_allstrata <- rbind(ahr_allstrata, ahr)

      bystratum <- tibble::tibble(Time=Omega,
                                  Stratum=stratum,
                                  t=Events$t,
                                  HR=failRates[failRates$Stratum==stratum,]$hr,
                                  info=1/(1/Events$Events.e+1/Events$Events.c),
                                  info0=Events$Events*ratio/(1+ratio)^2,
                                  Events=Events$Events)
      bystratum <- bystratum %>% dplyr::relocate(Events, .before = info)

      Bystratum <- rbind(Bystratum, bystratum)
    }
    AHR_allstrata <- rbind(AHR_allstrata, tibble::tibble(Time=ahr_allstrata$Time,
                                                         AHR=exp(sum(ahr_allstrata$avehr*ahr_allstrata$Events)/sum(ahr_allstrata$Events)),
                                                         Events=sum(ahr_allstrata$Events),
                                                         info=sum(ahr_allstrata$info),
                                                         info0=sum(ahr_allstrata$info0))[1,])
  }

  if (simple==FALSE){
    return(Bystratum)
  } else {
    return(AHR_allstrata)
  }
}

# Test 1: for the situation of single stratum and single cutoff ####


testthat::test_that("Validation passed for the situation of single stratum and single cutoff",{
  enrollRates <- tibble::tibble(Stratum = "All",
                                duration = c(2, 2, 10),
                                rate = c(3, 6, 9))
  failRates <- tibble::tibble(Stratum = "All",
                              duration = c(3, Inf),
                              failRate = log(2)/c(9, 18),
                              hr = c(0.9, 0.6),
                              dropoutRate = rep(0.001, 2))
  totalDuration <- 30
  ratio <- 1
  testthat::expect_equal(data.frame(test_AHR(enrollRates = enrollRates,
                                           failRates = failRates,
                                           totalDuration = totalDuration,
                                           ratio = ratio,
                                           simple = TRUE)),
                         data.frame(AHR(enrollRates = enrollRates,
                                        failRates = failRates,
                                        totalDuration = totalDuration,
                                        ratio = ratio,
                                        simple = TRUE)))

  testthat::expect_equal(data.frame(test_AHR(enrollRates = enrollRates,
                                           failRates = failRates,
                                           totalDuration = totalDuration,
                                           ratio = ratio,
                                           simple = FALSE)),
                         data.frame(AHR(enrollRates = enrollRates,
                                        failRates = failRates,
                                        totalDuration = totalDuration,
                                        ratio = ratio,
                                        simple = FALSE)))
  })

# Test 2: for the situation of single stratum and multiple cutoffs ####

testthat::test_that("Validation passed for the situation of single stratum and multiple cutoffs",{
  enrollRates <- tibble::tibble(Stratum = "All",
                                duration = c(2, 2, 10),
                                rate = c(3, 6, 9))
  failRates <- tibble::tibble(Stratum = "All",
                              duration = c(3, Inf),
                              failRate = log(2)/c(9, 18),
                              hr = c(0.9, 0.6),
                              dropoutRate = rep(0.001, 2))
  totalDuration <- c(15, 30)
  ratio <- 1

  testthat::expect_equal(data.frame(test_AHR(enrollRates = enrollRates,
                                           failRates = failRates,
                                           totalDuration = totalDuration,
                                           ratio = ratio,
                                           simple = TRUE)),
                         data.frame(AHR(enrollRates = enrollRates,
                                        failRates = failRates,
                                        totalDuration = totalDuration,
                                        ratio = ratio,
                                        simple = TRUE)))
  testthat::expect_equal(data.frame(test_AHR(enrollRates = enrollRates,
                                           failRates = failRates,
                                           totalDuration = totalDuration,
                                           ratio = ratio,
                                           simple = FALSE)),
                         data.frame(AHR(enrollRates = enrollRates,
                                        failRates = failRates,
                                        totalDuration = totalDuration,
                                        ratio = ratio,
                                        simple = FALSE)))
  })

# Test 3: for the situation of multiple strata and single cutoff ####

testthat::test_that("Validation passed for the situation of multiple strata and single cutoff",{
  enrollRates <- tibble::tibble(Stratum = c(rep("High",3), rep("Low",3)),
                                duration = c(2, 2, 10, 3, 3, 6),
                                rate = c(3, 6, 9, 2, 3, 4))
  failRates <- tibble::tibble(Stratum = c(rep("High",2), rep("Low",2)),
                              duration = c(3, Inf, 5, Inf),
                              failRate = c(log(2)/c(9, 18), log(2)/c(12, 25)),
                              hr = c(0.9, 0.6, 1, 0.8),
                              dropoutRate = rep(0.001, 4))
  totalDuration <- 30
  ratio <- 1
  testthat::expect_equal(data.frame(test_AHR(enrollRates = enrollRates,
                                           failRates = failRates,
                                           totalDuration = totalDuration,
                                           ratio = ratio,
                                           simple = TRUE)),
                         data.frame(AHR(enrollRates = enrollRates,
                                        failRates = failRates,
                                        totalDuration = totalDuration,
                                        ratio = ratio,
                                        simple = TRUE)))
  testthat::expect_equal(data.frame(test_AHR(enrollRates = enrollRates,
                                           failRates = failRates,
                                           totalDuration = totalDuration,
                                           ratio = ratio,
                                           simple = FALSE)),
                         data.frame(AHR(enrollRates = enrollRates,
                                        failRates = failRates,
                                        totalDuration = totalDuration,
                                        ratio = ratio,
                                        simple = FALSE)))
  })

# Test 4: for the situation of multiple strata and multiple cutoffs ####

testthat::test_that("Validation passed for the situation of multiple strata and multiple cutoffs",{
  enrollRates <- tibble::tibble(Stratum = c(rep("High",3), rep("Low",3)),
                                duration = c(2, 2, 10, 3, 3, 6),
                                rate = c(3, 6, 9, 2, 3, 4))
  failRates <- tibble::tibble(Stratum = c(rep("High",2), rep("Low",2)),
                              duration = c(3, Inf, 5, Inf),
                              failRate = c(log(2)/c(9, 18), log(2)/c(12, 25)),
                              hr = c(0.9, 0.6, 1, 0.8),
                              dropoutRate = rep(0.001, 4))
  totalDuration <- c(15, 30)
  ratio <- 1

  testthat::expect_equal(data.frame(test_AHR(enrollRates = enrollRates,
                                           failRates = failRates,
                                           totalDuration = totalDuration,
                                           ratio = ratio,
                                           simple = TRUE)),
                         data.frame(AHR(enrollRates = enrollRates,
                                        failRates = failRates,
                                        totalDuration = totalDuration,
                                        ratio = ratio,
                                        simple = TRUE)))
  testthat::expect_equal(data.frame(test_AHR(enrollRates = enrollRates,
                                           failRates = failRates,
                                           totalDuration = totalDuration,
                                           ratio = ratio,
                                           simple = FALSE)),
                         data.frame(AHR(enrollRates = enrollRates,
                                        failRates = failRates,
                                        totalDuration = totalDuration,
                                        ratio = ratio,
                                        simple = FALSE)))
  })



