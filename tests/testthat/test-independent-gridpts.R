test_that("compare gridpts results with gsDesign::normalGrid results", {
  x1 <- gridpts(r = 18, mu = 4, a = -Inf, b = Inf)
  x2 <- gsDesign::normalGrid(r = 18, bounds = c(-40, 40), mu = 4, sigma = 1)
  expect_equal(x1$w, x2$gridwgts)
  expect_equal(x1$z, x2$z)
  
  x1 <- gridpts(r = 18, mu = 2, a = -Inf, b = Inf)
  x2 <- gsDesign::normalGrid(r = 18, bounds = c(-40, 40), mu = 2, sigma = 1)
  expect_equal(x1$w, x2$gridwgts)
  expect_equal(x1$z, x2$z)
})
