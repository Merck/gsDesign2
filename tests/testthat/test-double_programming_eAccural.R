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




