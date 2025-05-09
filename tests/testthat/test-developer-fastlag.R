test_that("internal function fastlag() produces expected results", {
  observed <- fastlag(1:5, first = 100)
  expected <- c(100, 1:4)
  expect_equal(observed, expected)

  # can also work with list()
  observed <- fastlag(list(1, 2, 3, 4, 5), first = 100)
  expected <- list(100, 1, 2, 3, 4)
  expect_equal(observed, expected)
})
