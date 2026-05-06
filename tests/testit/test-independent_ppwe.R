assert("ppwe is incorrect when there are 2-step fail rates", {
  (isTRUE(all.equal(gsDesign2::ppwe(
      x = 0:20,
      duration = c(13, 100),
      rate = log(12) / c(9, 18),
      lower_tail = FALSE
    ), test_ppwe(
      x = 0:20,
      failRates = tibble::tibble(duration = c(13, 100), rate = log(12) / c(9, 18)),
      lower.tail = FALSE
    ))))
})

assert("ppwe is incorrect if varable x is longer than the max duration of fail rates", {
  (isTRUE(all.equal(gsDesign2::ppwe(
      x = 0:80,
      duration = c(13, 50),
      rate = log(4) / c(19, 9),
      lower_tail = FALSE
    ), test_ppwe(
      x = 0:80,
      failRates = tibble::tibble(duration = c(13, 50), rate = log(4) / c(19, 9)),
      lower.tail = FALSE
    ))))
})

assert("ppwe is incorrect when there are 3-step fail rates", {
  (isTRUE(all.equal(ppwe(
      x = 0:20,
      duration = c(3, 20, 100),
      rate = log(12) / c(9, 12, 18),
      lower_tail = FALSE
    ), test_2_ppwe(
      x = 0:20,
      failRates = tibble::tibble(duration = c(3, 20, 100), rate = log(12) / c(9, 12, 18)),
      lower.tail = FALSE
    ))))
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
