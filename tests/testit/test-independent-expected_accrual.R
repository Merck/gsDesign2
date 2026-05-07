assert("expected_accrual doesn't match with the double programming test_eAccrual function", {
  enroll_rate <- define_enroll_rate(duration = c(3, 13, 18), rate = c(5, 20, 8))
  res <- expected_accrual(time = 0:30, enroll_rate = enroll_rate)
  expected <- test_eAccrual(x = 0:30, enroll_rate = enroll_rate)
  (res %==% expected)
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
  (x %==% 1980)
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
  (x %==% 1980)
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
  (x %==% 1800)
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
  (x %==% 600)
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
  expected <- c(5, 10, 20, 33, 33) * 30 * 2
  (x %==% expected)
})
