test_that("compare with results from gsDesign, 3 analyses, equal IA timing", {
  x <- gsDesign::gsSurv(
    k = 3, test.type = 4, alpha = 0.025,
    beta = 0.2, timing = 1,
    sfu = gsDesign::sfLDOF, sfupar = 0, sfl = gsDesign::sfLDOF,
    sflpar = 0, lambdaC = 0.1,
    hr = 0.6, hr0 = 1, eta = 0.01,
    gamma = 10,
    R = 12, S = NULL,
    T = 36, minfup = 24, ratio = 1
  )

  xbound <- cbind(x$upper$bound, x$lower$bound)

  test1 <- gsDesign2:::gs_bound(
    alpha = gsDesign::sfLDOF(0.025, 1:3 / 3)$spend,
    beta = gsDesign::sfLDOF(0.2, 1:3 / 3)$spend,
    analysis = 1:3,
    theta = x$theta[2] * sqrt(x$n.I),
    corr = outer(1:3, 1:3, function(x, y) pmin(x, y) / pmax(x, y))
  )

  expect_equal(object = unlist(test1[1], use.names = FALSE), expected = xbound[, 1], tolerance = 0.05)
  expect_equal(object = unlist(test1[2], use.names = FALSE), expected = xbound[, 2], tolerance = 0.05)
})

test_that("compare with results from gsDesign, 3 analyses, unequal IA timing", {
  y <- gsDesign::gsSurv(
    k = 3, test.type = 4, alpha = 0.025,
    beta = 0.2, timing = c(0.6, 0.8),
    sfu = gsDesign::sfLDOF, sfupar = 0, sfl = gsDesign::sfLDOF,
    sflpar = 0, lambdaC = 0.1,
    hr = 0.6, hr0 = 1, eta = 0.01,
    gamma = 10,
    R = 12, S = NULL,
    T = 36, minfup = 24, ratio = 1
  )

  ybound <- cbind(y$upper$bound, y$lower$bound)

  test2 <- gsDesign2:::gs_bound(
    alpha = gsDesign::sfLDOF(0.025, 3:5 / 5)$spend,
    beta = gsDesign::sfLDOF(0.2, 3:5 / 5)$spend,
    analysis = 1:3,
    theta = y$theta[2] * sqrt(y$n.I),
    corr = outer(3:5, 3:5, function(x, y) pmin(x, y) / pmax(x, y))
  )

  expect_equal(object = unlist(test2[1], use.names = FALSE), expected = ybound[, 1], tolerance = 0.05)
  expect_equal(object = unlist(test2[2], use.names = FALSE), expected = ybound[, 2], tolerance = 0.05)
})
