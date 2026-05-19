# Helper functions used by test-independent-expected_accrual.R

test_eAccrual <- function(x, enroll_rate) {
  boundary <- cumsum(enroll_rate$duration)
  rate <- enroll_rate$rate
  xvals <- unique(c(x, boundary))

  eAc2 <- numeric(length(xvals))
  for (t in seq_along(xvals)) {
    val <- xvals[t]
    if (val <= boundary[1]) {
      eAc2[t] <- val * rate[1]
    } else if (val <= boundary[2]) {
      eAc2[t] <- boundary[1] * rate[1] + (val - boundary[1]) * rate[2]
    } else if (val <= boundary[3]) {
      eAc2[t] <- boundary[1] * rate[1] +
        (boundary[2] - boundary[1]) * rate[2] + (val - boundary[2]) * rate[3]
    } else {
      eAc2[t] <- boundary[1] * rate[1] +
        (boundary[2] - boundary[1]) * rate[2] + (boundary[3] - boundary[2]) * rate[3]
    }
  }

  ind <- !is.na(match(xvals, x))
  return(eAc2[ind])
}
