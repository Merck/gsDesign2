test_eAccrual = function(x = 0:24,
                         enrollRates = tibble::tibble(duration = c(3, 3, 18),
                                                      rate = c(5, 10, 20))) {
  boundary = cumsum(enrollRates$duration)
  rate = enrollRates$rate
  xvals = unique(c(x, boundary))
  # maxlen=sum(enrollRates$duration)
  # xvals1=xvals[xvals<=maxlen]
  eAc2 = numeric(length(xvals))
  for (t in 1:length(xvals)) {
    val = xvals[t]
    if (val <= boundary[1]) {
      eAc2[t] = val * rate[1]
    }
    else if (val <= boundary[2]) {
      eAc2[t] = boundary[1] * rate[1] + (val - boundary[1]) * rate[2]
    }
    else if (val <= boundary[3]) {
      eAc2[t] = boundary[1] * rate[1] +
        (boundary[2] - boundary[1]) * rate[2] + (val - boundary[2]) * rate[3]
    }
    else {
      eAc2[t] = boundary[1] * rate[1] +
        (boundary[2] - boundary[1]) * rate[2] + (boundary[3] - boundary[2]) *
        rate[3]
    }
  }
  #eAc3=c(eAc2, rep(max(eAc2),length(x)-length(xvals1)))
  ind <- !is.na(match(xvals, x))
  return(eAc2[ind])
}


testthat::test_that("eAccrual doesn't match with the double programming e_Acurral function", {
  testthat::expect_equal(
    eAccrual(x = 0:30,
             enrollRates = tibble::tibble(duration = c(3, 13, 18),
                                          rate = c(5, 20, 8))),
    test_eAccrual(x = 0:30,
                  enrollRates = tibble::tibble(duration = c(3, 13, 18),
                                               rate = c(5, 20, 8)))
  )
})


## add below test case



testthat::test_that("eAccrual fail to identify a non-numerical input",{
  x=c(0:20, "NA")
  expect_error(expect_message(eAccrual(x=x), "gsDesign2: x in `eAccrual()` must be a strictly increasing non-negative numeric vector"))
  
})

testthat::test_that("eAccrual fail to identify a negative input",{
  x=-20:-1
  expect_error(expect_message(eAccrual(x=x), "gsDesign2: x in `eAccrual()` must be a strictly increasing non-negative numeric vector"))
  
})


testthat::test_that("eAccrual fail to identify a non-increasing input",{
  x=20:1
  expect_error(expect_message(eAccrual(x=x), "gsDesign2: x in `eAccrual()` must be a strictly increasing non-negative numeric vector"))
  
})



testthat::test_that("eAccrual fail to identify a non-dataframe input",{
  enrollRates=as.matrix(tibble::tibble(duration=c(3,3,18), rate=c(5,10,20)))
  expect_error(expect_message(eAccrual(enrollRates = enrollRates), "gsDesign2: enrollRates in `eAccrual()` must be a data.frame"))
})


testthat::test_that("eAccrual fail to identify a non-dataframe input",{
  enrollRates=tibble::tibble(times=c(3,3,18), rate=c(5,10,20))
  expect_error(expect_message(eAccrual(enrollRates = enrollRates), "gsDesign2: enrollRates in `eAccrual()` column names must contain duration"))
})

testthat::test_that("eAccrual fail to identify a non-dataframe input",{
  enrollRates=tibble::tibble(duration=c(3,3,18), freqs=c(5,10,20))
  expect_error(expect_message(eAccrual(enrollRates = enrollRates), "gsDesign2: enrollRates in `eAccrual()` column names must contain rate"))
})



testthat::test_that("eAccrual fail to identify a non-dataframe input",{
  enrollRates=tibble::tibble(duration=c(3,3,18), rate=c(-5,10,20))
  expect_error(expect_message(eAccrual(enrollRates = enrollRates), "gsDesign2: enrollRates in `eAccrual()` must be non-negative with at least one positive rate"))
})



testthat::test_that("eAccrual fail to identify a non-dataframe input",{
  enrollRates=tibble::tibble(duration=c(3,3,18), rate=c(-15,-10,-2))
  expect_error(expect_message(eAccrual(enrollRates = enrollRates), "gsDesign2: enrollRates in `eAccrual()` must be non-negative with at least one positive rate"))
})

## add test cases for stratified design
testthat::test_that("eAccrual fail to identify a non-dataframe input",{
  x <- eAccrual(x = 40, enrollRates = tibble(Stratum = c("S1", "S2"),
                                             duration = 33, 
                                             rate = c(30, 30)))
  expect_equal(x, 33*30*2)
})

testthat::test_that("eAccrual fail to identify a non-dataframe input",{
  x <- eAccrual(x = 33, enrollRates = tibble(Stratum = c("S1", "S2"),
                                             duration = 33, 
                                             rate = c(30, 30)))
  expect_equal(x, 33*30*2)
})

testthat::test_that("eAccrual fail to identify a non-dataframe input",{
  x <- eAccrual(x = 30, enrollRates = tibble(Stratum = c("S1", "S2"),
                                             duration = 33, 
                                             rate = c(30, 30)))
  expect_equal(x, 30*30*2)
})

testthat::test_that("eAccrual fail to identify a non-dataframe input",{
  x <- eAccrual(x = 10, enrollRates = tibble(Stratum = c("S1", "S2"),
                                             duration = 33, 
                                             rate = c(30, 30)))
  expect_equal(x, 10*30*2)
})

testthat::test_that("eAccrual fail to identify a non-dataframe input",{
  x <- eAccrual(x = c(5, 10, 20, 33, 50), enrollRates = tibble(Stratum = c("S1", "S2"),
                                             duration = 33, 
                                             rate = c(30, 30)))
  expect_equal(x, c(5, 10, 20, 33, 33) * 30 * 2)
})

