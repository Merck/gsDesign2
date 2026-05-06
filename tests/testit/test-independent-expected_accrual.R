assert("expected_accrual doesn't match with the double programming test_eAccrual function", {
  (isTRUE(all.equal(expected_accrual(
      time = 0:30,
      enroll_rate = define_enroll_rate(
        duration = c(3, 13, 18),
        rate = c(5, 20, 8)
      )
    ), test_eAccrual(
      x = 0:30,
      enroll_rate = define_enroll_rate(
        duration = c(3, 13, 18),
        rate = c(5, 20, 8)
      )
    ))))
})

assert("expected_accrual fail to identify a non-numerical input", {
  x <- c(0:20, "NA")
  (has_error(expected_accrual(time = x), "must not be negative"))
})

assert("expected_accrual fail to identify a negative input", {
  x <- -20:-1
  (has_error(expected_accrual(time = x), "must not be negative"))
})

assert("expected_accrual fail to identify a non-increasing input", {
  x <- 20:1
  (has_error(expected_accrual(time = x), "strictly increasing"))
})

# Add test cases for stratified design
assert("expected_accrual fail to identify a non-dataframe input", {
  x <- expected_accrual(
    time = 40,
    enroll_rate = define_enroll_rate(
      stratum = c("S1", "S2"),
      duration = 33,
      rate = c(30, 30)
    )
  )
  (isTRUE(all.equal(x, 33 * 30 * 2)))
})

assert("expected_accrual fail to identify a non-dataframe input", {
  x <- expected_accrual(
    time = 33,
    enroll_rate = define_enroll_rate(
      stratum = c("S1", "S2"),
      duration = 33,
      rate = c(30, 30)
    )
  )
  (isTRUE(all.equal(x, 33 * 30 * 2)))
})

assert("expected_accrual fail to identify a non-dataframe input", {
  x <- expected_accrual(
    time = 30,
    enroll_rate = define_enroll_rate(
      stratum = c("S1", "S2"),
      duration = 33,
      rate = c(30, 30)
    )
  )
  (isTRUE(all.equal(x, 30 * 30 * 2)))
})

assert("expected_accrual fail to identify a non-dataframe input", {
  x <- expected_accrual(
    time = 10,
    enroll_rate = define_enroll_rate(
      stratum = c("S1", "S2"),
      duration = 33,
      rate = c(30, 30)
    )
  )
  (isTRUE(all.equal(x, 10 * 30 * 2)))
})

assert("expected_accrual fail to identify a non-dataframe input", {
  x <- expected_accrual(
    time = c(5, 10, 20, 33, 50),
    enroll_rate = define_enroll_rate(
      stratum = c("S1", "S2"),
      duration = 33,
      rate = c(30, 30)
    )
  )
  (isTRUE(all.equal(x, c(5, 10, 20, 33, 33) * 30 * 2)))
})
