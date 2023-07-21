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

testthat::test_that("ppwe is incorrect when there are 2-step fail rates", {
  testthat::expect_equal(
    gsDesign2::ppwe(
      x = 0:20,
      duration = c(13, 100),
      rate = log(12) / c(9, 18),
      lower_tail = FALSE
    ),
    test_ppwe(
      x = 0:20,
      failRates = tibble::tibble(duration = c(13, 100), rate = log(12) / c(9, 18)),
      lower.tail = FALSE
    )
  )
})

testthat::test_that("ppwe is incorrect if varable x is longer than the max duration of fail rates", {
  testthat::expect_equal(
    gsDesign2::ppwe(
      x = 0:80,
      duration = c(13, 50),
      rate = log(4) / c(19, 9),
      lower_tail = FALSE
    ),
    test_ppwe(
      x = 0:80,
      failRates = tibble::tibble(duration = c(13, 50), rate = log(4) / c(19, 9)),
      lower.tail = FALSE
    )
  )
})

testthat::test_that("ppwe is incorrect when there are 3-step fail rates", {
  testthat::expect_equal(
    ppwe(
      x = 0:20,
      duration = c(3, 20, 100),
      rate = log(12) / c(9, 12, 18),
      lower_tail = FALSE
    ),
    test_2_ppwe(
      x = 0:20,
      failRates = tibble::tibble(duration = c(3, 20, 100), rate = log(12) / c(9, 12, 18)),
      lower.tail = FALSE
    )
  )
})

# Add the following test case

testthat::test_that("ppwe fail to identify a non-numerical input", {
  x <- c(0:20, "NA")
  expect_error(expect_message(
    gsDesign2::ppwe(x = x, duration = 1, rate = 1),
    "gsDesign2: x in `ppwe()` must be a strictly increasing non-negative numeric vector"
  ))
})

testthat::test_that("ppwe fail to identify a negative input", {
  x <- -20:-1
  expect_error(expect_message(
    gsDesign2::ppwe(x = x, duration = 1, rate = 1),
    "gsDesign2: x in `ppwe()` must be a strictly increasing non-negative numeric vector"
  ))
})

testthat::test_that("ppwe fail to identify a non-increasing input", {
  x <- 20:1
  expect_error(expect_message(
    ppwe(x = x, duration = 1, rate = 1),
    "gsDesign2: x in `ppwe()` must be a strictly increasing non-negative numeric vector"
  ))
})
