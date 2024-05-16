test_that("internal function fastlag() produces expected results", {
  observed <- gsDesign2:::fastlag(1:5, first = 100)
  expected <- c(100, 1:4)
  expect_equal(observed, expected)

  # can also work with list()
  observed <- gsDesign2:::fastlag(list(1, 2, 3, 4, 5), first = 100)
  expected <- list(100, 1, 2, 3, 4)
  expect_equal(observed, expected)
})

test_that("internal function fastlag() throws errors for bad inputs", {
  expect_error(gsDesign2:::fastlag(1:5))
  expect_error(gsDesign2:::fastlag(1:5, default = 100))
  expect_error(gsDesign2:::fastlag(data.frame(), first = 100))
  expect_error(gsDesign2:::fastlag(1:5, first = data.frame()))
  expect_error(gsDesign2:::fastlag(c(), first = 100))
  expect_error(gsDesign2:::fastlag(1:5, first = c()))
  expect_error(gsDesign2:::fastlag(1:5, first = 1:2))
})
