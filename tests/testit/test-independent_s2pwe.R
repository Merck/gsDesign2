assert("s2pwe fails to come up with the correct answer", {
  time <- c(1, 5, 6, 8, 10)
  survival <- c(0.5, 0.4, 0.3, 0.2, 0.1)
  (isTRUE(all.equal(round(gsDesign2::s2pwe(time = time, survival = survival), 3), round(dplyr::tibble(duration = c(1, 4, 1, 2, 2), rate = c(0.693, 0.0558, 0.288, 0.203, 0.347)), 3))))
})

assert("s2pwe fails to identify non-numeric value", {
  times <- c(1, "NA")
  survival <- c(0.5, 0.4)
  (has_error(gsDesign2::s2pwe(times = times, survival = survival)))
})

assert("s2pwe fails to identify non-positive value", {
  times2 <- c(1, NA)
  survival <- c(0.5, 0.4)
  (has_error(gsDesign2::s2pwe(times = times2, survival = survival)))
})

assert("s2pwe fails to identify infinity value", {
  times3 <- c(1, Inf)
  survival <- c(0.5, 0.4)
  (has_error(gsDesign2::s2pwe(times = times2, survival = survival)))
})

assert("s2pwe fails to identify non-increasing value", {
  times4 <- c(1, 2, 1)
  survival <- c(0.5, 0.4, 0.3)
  (has_error(gsDesign2::s2pwe(times = times4, survival = survival)))
})

assert("s2pwe fails to identify non-numerical survival", {
  times5 <- c(1, 2)
  survival <- c(0.5, "NA")
  (has_error(gsDesign2::s2pwe(times = times5, survival = survival)))
})

assert("s2pwe survival and time should have the same length", {
  times6 <- c(1, 2, 5)
  survival <- c(0.5, 0.3)
  (has_error(gsDesign2::s2pwe(times = times6, survival = survival)))
})

assert("s2pwe fails to identify non-positive survival", {
  times <- c(1, 3)
  survival2 <- c(0.5, -0.1)
  (has_error(gsDesign2::s2pwe(times = times, survival = survival2)))
})

assert("s2pwe fails to identify large than 1 survival", {
  times <- c(1, 3)
  survival3 <- c(0.5, 1.5)
  (has_error(gsDesign2::s2pwe(times = times, survival = survival3)))
})

assert("s2pwe fails to identify an increasing survival series", {
  times <- c(1, 3)
  survival4 <- c(0.5, 0.9)
  (has_error(gsDesign2::s2pwe(times = times, survival = survival4)))
})
