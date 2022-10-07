test_that("hupdate() returns results as expected ",{
  #the design
  gstry <- gsDesign::gsDesign(k = 3,
                              sfl = gsDesign::sfLDOF,
                              delta = 0)
  #probabilities calculated based on function h1(), IA1 needs to full between low and upper bound
  #in order to continue to IA2
  null.01 <- h1(theta = gstry$theta[1],
                I = gstry$n.I[1],
                a = gstry$lower$bound[1],
                b = gstry$upper$bound[1])
  #IA2 to reject H0, we integrate from upper bound to Inf
  upper.null.02 <- sum(hupdate(theta = gstry$theta[1],
                               thetam1 = gstry$theta[1],
                               I = gstry$n.I[2],
                               Im1 = gstry$n.I[1],
                               gm1 = null.01,
                               a = gstry$upper$bound[2],
                               b = Inf)$h)
  #IA2 to accept H0, we integrate from -Inf to lower bound
  lower.null.02 <- sum(hupdate(theta = gstry$theta[1],
                               thetam1 = gstry$theta[1],
                               I = gstry$n.I[2],
                               Im1 = gstry$n.I[1],
                               gm1 = null.01,
                               a = -Inf,
                               b = gstry$lower$bound[2])$h)
  
  alt.01 <- h1(theta = gstry$theta[2],
               I = gstry$n.I[1],
               a = gstry$lower$bound[1],
               b = gstry$upper$bound[1])
  #IA2 to reject H0, we integrate from upper bound to Inf
  upper.alt.02 <- sum(hupdate(theta = gstry$theta[2],
                              thetam1 = gstry$theta[2],
                              I = gstry$n.I[2],
                              Im1 = gstry$n.I[1],
                              gm1 = alt.01,
                              a = gstry$upper$bound[2],
                              b = Inf)$h)
  #IA2 to accept H0, we integrate from -Inf to lower bound
  lower.alt.02 <- sum(hupdate(theta = gstry$theta[2],
                              thetam1 = gstry$theta[2],
                              I = gstry$n.I[2],
                              Im1 = gstry$n.I[1],
                              gm1 = alt.01,
                              a = -Inf,
                              b = gstry$lower$bound[2])$h)
  #probabilities calculated based on function gsProbability
  x <- gsDesign::gsProbability(
    k = 3,
    a = gstry$lower$bound,
    b = gstry$upper$bound,
    n.I = gstry$n.I,
    theta = gstry$theta
  )
  
  expect_equal(object = as.numeric(c(lower.null.02, lower.alt.02)), expected = x$lower$prob[2, ], tolerance = 0.0001)
  expect_equal(object = as.numeric(c(upper.null.02, upper.alt.02)), expected = x$upper$prob[2, ], tolerance = 0.0001)
  #problem with below code on extreme case:
  #hupdate(theta = gstry$theta[1], thetam1= gstry$theta[1],
  #     I=gstry$n.I[1]+0.00000000000001,Im1=gstry$n.I[1],gm1=null.01,
  #     a = gstry$upper$bound[2],b=Inf) %>% summarise(p = sum(h))
})

test_that("hupdate() returns probability almost zero for extreme case",{
  #the design
  gstry <- gsDesign::gsDesign(k = 3,
                              sfl = gsDesign::sfLDOF,
                              delta = 0)
  null.01 <- h1(theta = gstry$theta[1],
                I = gstry$n.I[1],
                a = gstry$lower$bound[1],
                b = gstry$upper$bound[1])
  #IA2 to reject H0, we integrate from upper bound to Inf
  #-8 is an arbitrary extreme case for theta
  poor.02 <- sum(hupdate(theta = -8,
                         thetam1 = gstry$theta[1],
                         I = gstry$n.I[2],
                         Im1 = gstry$n.I[1],
                         gm1 = null.01,
                         a = gstry$upper$bound[2],
                         b = Inf)$h)
  #IA2 to accept H0, we integrate from -Inf to lower bound
  #-8 is an arbitrary extreme case for the bound
  high.02 <- sum(hupdate(theta = gstry$theta[2],
                         thetam1 = gstry$theta[2],
                         I = gstry$n.I[2],
                         Im1 = gstry$n.I[1],
                         gm1 = null.01,
                         a = -Inf,
                         b = -8)$h)
  expect_equal(object = as.numeric(c(poor.02, high.02)), expected = c(0, 0), tolerance = 0.0001)
})