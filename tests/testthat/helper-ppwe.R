# Helper functions used by test-double_programming_ppwe.R

test_ppwe <- function(
    x = 0:20,
    failRates = tibble::tibble(
      duration = c(3, 100),
      rate = log(2) / c(9, 18)
    ),
    lower.tail = FALSE) {
  boundary <- cumsum(failRates$duration)
  rate <- failRates$rate
  xvals <- unique(c(x, boundary))
  H <- numeric(length(xvals))
  maxlen <- sum(failRates$duration)
  max.x <- max(x)

  if (length(x) <= maxlen) {
    for (t in seq_along(xvals)) {
      val <- xvals[t]
      if (val <= boundary[1]) {
        H[t] <- val * rate[1]
      } else if (val <= boundary[2]) {
        H[t] <- boundary[1] * rate[1] + (val - boundary[1]) * rate[2]
      } else {
        H[t] <- boundary[1] * rate[1] + (boundary[2] - boundary[1]) * rate[2]
      }
    }
    surv <- exp(-H)
  } else {
    boundary1 <- boundary
    boundary1[2] <- max.x
    for (t in seq_along(xvals)) {
      val <- xvals[t]
      if (val <= boundary1[1]) {
        H[t] <- val * rate[1]
      } else if (val <= boundary1[2]) {
        H[t] <- boundary1[1] * rate[1] + (val - boundary1[1]) * rate[2]
      } else {
        H[t] <- boundary1[1] * rate[1] + (boundary1[2] - boundary1[1]) * rate[2]
      }
    }
    surv <- exp(-H)
  }

  ind <- !is.na(match(xvals, x))

  if (lower.tail) {
    return(1 - surv[ind])
  } else {
    return(surv[ind])
  }
}

# Double programming of ppwe when there are 3 steps of failure rates.
# The method is a simple extention of test_ppwe.
test_2_ppwe <- function(
    x = 0:20,
    failRates = tibble::tibble(
      duration = c(3, 20, 100),
      rate = log(2) / c(9, 12, 18)
    ),
    lower.tail = FALSE) {
  boundary <- cumsum(failRates$duration)
  rate <- failRates$rate
  xvals <- unique(c(x, boundary))
  H <- numeric(length(xvals))
  for (t in seq_along(xvals)) {
    val <- xvals[t]
    if (val <= boundary[1]) {
      H[t] <- val * rate[1]
    } else if (val <= boundary[2]) {
      H[t] <- boundary[1] * rate[1] + (val - boundary[1]) * rate[2]
    } else if (val <= boundary[3]) {
      H[t] <- boundary[1] * rate[1] + (boundary[2] - boundary[1]) * rate[2] + (val - boundary[3]) * rate[3]
    } else {
      H[t] <- boundary[1] * rate[1] + (boundary[2] - boundary[1]) * rate[2] + (boundary[3] - boundary[2]) * rate[3]
    }
  }
  surv <- exp(-H)

  ind <- !is.na(match(xvals, x))

  if (lower.tail) {
    return(1 - surv[ind])
  } else {
    return(surv[ind])
  }
}
