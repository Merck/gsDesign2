assert("ppwe is incorrect when there are 2-step fail rates", {
  res <- gsDesign2::ppwe(x = 0:20, duration = c(13, 100), rate = log(12)/c(9, 18), lower_tail = FALSE)
  expected <- test_ppwe(
    x = 0:20,
    failRates = tibble::tibble(duration = c(13, 100), rate = log(12)/c(9, 18)),
    lower.tail = FALSE
  )
  (res %==% expected)
})

assert("ppwe is incorrect if varable x is longer than the max duration of fail rates", {
  res <- gsDesign2::ppwe(
    x = 0:80, duration = c(13, 50),
    rate = log(4)/c(19, 9), lower_tail = FALSE
  )
  expected <- test_ppwe(
    x = 0:80,
    failRates = tibble::tibble(duration = c(13, 50), rate = log(4)/c(19, 9)),
    lower.tail = FALSE
  )
  (all.equal(res, expected))
})

assert("ppwe is incorrect when there are 3-step fail rates", {
  res <- ppwe(
    x = 0:20, duration = c(3, 20, 100),
    rate = log(12)/c(9, 12, 18), lower_tail = FALSE
  )
  expected <- test_2_ppwe(
    x = 0:20,
    failRates = tibble::tibble(duration = c(3, 20, 100), rate = log(12)/c(9, 12, 18)),
    lower.tail = FALSE
  )
  (all.equal(res, expected))
})

# Add the following test case

assert("ppwe fail to identify a non-numerical input", {
  x <- c(0:20, "NA")
  (has_error(gsDesign2::ppwe(x = x, duration = 1, rate = 1)))
})

assert("ppwe fail to identify a negative input", {
  x <- -20:-1
  (has_error(gsDesign2::ppwe(x = x, duration = 1, rate = 1)))
})

assert("ppwe fail to identify a non-increasing input", {
  x <- 20:1
  (has_error(ppwe(x = x, duration = 1, rate = 1)))
})
