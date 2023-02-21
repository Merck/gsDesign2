testthat::test_that("s2pwe fails to come up with the correct answer", {
  time <- c(1, 5, 6, 8, 10)
  survival <- c(0.5, 0.4, 0.3, 0.2, 0.1)
  expect_equal(
    ignore_attr = TRUE,
    round(gsDesign2::s2pwe(time = time, survival = survival), 3),
    round(dplyr::tibble(duration = c(1, 4, 1, 2, 2), rate = c(0.693, 0.0558, 0.288, 0.203, 0.347)), 3)
  )
})

testthat::test_that("s2pwe fails to identify non-numeric value", {
  times <- c(1, "NA")
  survival <- c(0.5, 0.4)
  expect_error(
    expect_message(
      gsDesign2::s2pwe(times = times, survival = survival),
      "gsDesign2: times in `s2pwe()` must be increasing positive finite numbers"
    )
  )
})

testthat::test_that("s2pwe fails to identify non-positive value", {
  times2 <- c(1, NA)
  survival <- c(0.5, 0.4)
  expect_error(
    expect_message(
      gsDesign2::s2pwe(times = times2, survival = survival),
      "gsDesign2: times in `s2pwe()` must be increasing positive finite numbers"
    )
  )
})

testthat::test_that("s2pwe fails to identify infinity value", {
  times3 <- c(1, Inf)
  survival <- c(0.5, 0.4)
  expect_error(
    expect_message(
      gsDesign2::s2pwe(times = times2, survival = survival),
      "gsDesign2: times in `s2pwe()` must be increasing positive finite numbers"
    )
  )
})

testthat::test_that("s2pwe fails to identify non-increasing value", {
  times4 <- c(1, 2, 1)
  survival <- c(0.5, 0.4, 0.3)
  expect_error(
    expect_message(
      gsDesign2::s2pwe(times = times4, survival = survival),
      "gsDesign2: times in `s2pwe()` must be increasing positive finite numbers"
    )
  )
})

testthat::test_that("s2pwe fails to identify non-numerical survival", {
  times5 <- c(1, 2)
  survival <- c(0.5, "NA")
  expect_error(
    expect_message(
      gsDesign2::s2pwe(times = times5, survival = survival),
      "gsDesign2: survival in `s2pwe()` must be numeric and of same length as times"
    )
  )
})

testthat::test_that("s2pwe survival and time should have the same length", {
  times6 <- c(1, 2, 5)
  survival <- c(0.5, 0.3)
  expect_error(
    expect_message(
      gsDesign2::s2pwe(times = times6, survival = survival),
      "gsDesign2: survival in `s2pwe()` must be numeric and of same length as times"
    )
  )
})

testthat::test_that("s2pwe fails to identify non-positive survival", {
  times <- c(1, 3)
  survival2 <- c(0.5, -0.1)
  expect_error(
    expect_message(
      gsDesign2::s2pwe(times = times, survival = survival2),
      paste(
        "gsDesign2: survival in `s2pwe()` must be non-increasing positive",
        "finite numbers less than or equal to 1 with at least 1 value < 1"
      )
    )
  )
})

testthat::test_that("s2pwe fails to identify large than 1 survival", {
  times <- c(1, 3)
  survival3 <- c(0.5, 1.5)
  expect_error(
    expect_message(
      gsDesign2::s2pwe(times = times, survival = survival3),
      paste(
        "gsDesign2: survival in `s2pwe()` must be non-increasing positive",
        "finite numbers less than or equal to 1 with at least 1 value < 1"
      )
    )
  )
})

testthat::test_that("s2pwe fails to identify an increasing survival series", {
  times <- c(1, 3)
  survival4 <- c(0.5, 0.9)
  expect_error(
    expect_message(
      gsDesign2::s2pwe(times = times, survival = survival4),
      paste(
        "gsDesign2: survival in `s2pwe()` must be non-increasing positive",
        "finite numbers less than or equal to 1 with at least 1 value < 1"
      )
    )
  )
})
