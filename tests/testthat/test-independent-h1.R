test_that("h1() returns results as expected ",{
  #the design
  gstry <- gsDesign::gsDesign(k = 3,
                              sfl = gsDesign::sfLDOF,
                              delta = 0)
  #probabilities calculated based on function h1()
  upper.null <- sum(h1(theta = gstry$theta[1],
                       I = gstry$n.I[1],
                       a = gstry$upper$bound[1],
                       b = Inf)$h)
  upper.alt <- sum(h1(theta = gstry$theta[2],
                      I = gstry$n.I[1],
                      a = gstry$upper$bound[1],
                      b = Inf)$h)
  lower.null <- sum(h1(theta = gstry$theta[1],
                       I = gstry$n.I[1],
                       a = -Inf,
                       b = gstry$lower$bound[1])$h)
  lower.alt <- sum(h1(theta = gstry$theta[2],
                      I = gstry$n.I[1],
                      a = -Inf,
                      b = gstry$lower$bound[1])$h)
  #probabilities calculated based on function gsProbability
  x <- gsDesign::gsProbability(
    k = 3,
    a = gstry$lower$bound,
    b = gstry$upper$bound,
    n.I = gstry$n.I , theta = gstry$theta
  )
  expect_equal(object = as.numeric(c(upper.null, upper.alt)), expected = x$upper$prob[1,], tolerance = 0.0001)
  expect_equal(object = as.numeric(c(lower.null, lower.alt)), expected = x$lower$prob[1,], tolerance = 0.0001)
})

test_that("h1() returns probability almost zero for extreme case",{
  exmtest1 <- sum(h1(theta = 9, I = 0.5, a = -Inf, b=0)$h)
  exmtest2 <- sum(h1(theta = 1, I = 0.5, a = 9, b = Inf)$h)
  expect_equal(object = as.numeric(c(exmtest1, exmtest2)), expected = c(0,0), tolerance = 0.0001)
})