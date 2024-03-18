test_that("Default (N(0,1)) - approximate variance of standard normal (i.e., 1)", {
  x1 <- gridpts_(mu = 0, a = -20, b = 20, r = 18) # gsDesign2 old version
  x2 <- gsDesign2:::gridpts(mu = 0, a = -20, b = 20, r = 18) # gsDesign2 latest version
  expect_equal(x1$z, x2$z)
  expect_equal(x1$w, x2$w)
})

test_that("Approximate probability of N(0,1) above .95 quantile (i.e., .05)", {
  x1 <- gridpts_(mu = 0, a = qnorm(0.95), b = Inf, r = 18)
  x2 <- gsDesign2:::gridpts(mu = 0, a = qnorm(0.95), b = Inf, r = 18)
  expect_equal(x1$z, x2$z)
  expect_equal(x1$w, x2$w)
})

test_that("Approximate probability of N(0.5, 1) above .95 quantile (i.e., .05)", {
  x1 <- gridpts_(mu = 0.5, a = qnorm(0.95), b = Inf, r = 18)
  x2 <- gsDesign2:::gridpts(mu = 0.5, a = qnorm(0.95), b = Inf, r = 18)
  expect_equal(x1$z, x2$z)
  expect_equal(x1$w, x2$w)
})
