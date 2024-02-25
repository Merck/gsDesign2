test_that("expected_accrual doesn't match with the double programming test_eAccrual function", {
  expect_equal(
    expected_accrual(
      time = 0:30,
      enroll_rate = define_enroll_rate(
        duration = c(3, 13, 18),
        rate = c(5, 20, 8)
      )
    ),
    test_eAccrual(
      x = 0:30,
      enroll_rate = define_enroll_rate(
        duration = c(3, 13, 18),
        rate = c(5, 20, 8)
      )
    )
  )
})

test_that("expected_accrual fail to identify a non-numerical input", {
  x <- c(0:20, "NA")
  expect_error(expect_message(
    expected_accrual(time = x),
    "gsDesign2: time in `expected_accrual()` must be a strictly increasing non-negative numeric vector"
  ))
})

test_that("expected_accrual fail to identify a negative input", {
  x <- -20:-1
  expect_error(expect_message(
    expected_accrual(time = x),
    "gsDesign2: time in `expected_accrual()` must be a strictly increasing non-negative numeric vector"
  ))
})

test_that("expected_accrual fail to identify a non-increasing input", {
  x <- 20:1
  expect_error(expect_message(
    expected_accrual(time = x),
    "gsDesign2: time in `expected_accrual()` must be a strictly increasing non-negative numeric vector"
  ))
})

# Add test cases for stratified design
test_that("expected_accrual fail to identify a non-dataframe input", {
  x <- expected_accrual(
    time = 40,
    enroll_rate = define_enroll_rate(
      stratum = c("S1", "S2"),
      duration = 33,
      rate = c(30, 30)
    )
  )
  expect_equal(x, 33 * 30 * 2)
})

test_that("expected_accrual fail to identify a non-dataframe input", {
  x <- expected_accrual(
    time = 33,
    enroll_rate = define_enroll_rate(
      stratum = c("S1", "S2"),
      duration = 33,
      rate = c(30, 30)
    )
  )
  expect_equal(x, 33 * 30 * 2)
})

test_that("expected_accrual fail to identify a non-dataframe input", {
  x <- expected_accrual(
    time = 30,
    enroll_rate = define_enroll_rate(
      stratum = c("S1", "S2"),
      duration = 33,
      rate = c(30, 30)
    )
  )
  expect_equal(x, 30 * 30 * 2)
})

test_that("expected_accrual fail to identify a non-dataframe input", {
  x <- expected_accrual(
    time = 10,
    enroll_rate = define_enroll_rate(
      stratum = c("S1", "S2"),
      duration = 33,
      rate = c(30, 30)
    )
  )
  expect_equal(x, 10 * 30 * 2)
})

test_that("expected_accrual fail to identify a non-dataframe input", {
  x <- expected_accrual(
    time = c(5, 10, 20, 33, 50),
    enroll_rate = define_enroll_rate(
      stratum = c("S1", "S2"),
      duration = 33,
      rate = c(30, 30)
    )
  )
  expect_equal(x, c(5, 10, 20, 33, 33) * 30 * 2)
})
