test_that("compare gs_spending_bound with gsDesign results with equal IA timing for upper and lower bound", {
  
  x <- gsDesign::gsSurv(k = 3,
                        test.type = 4,
                        alpha = 0.025,
                        beta = 0.2,
                        timing = 1,
                        sfu = gsDesign::sfLDOF,
                        sfupar = c( 0 ),
                        sfl = gsDesign::sfLDOF,
                        sflpar = c( 0 ),
                        lambdaC = c( 0.1 ),
                        hr = 0.6,
                        hr0 = 1,
                        eta = 0.01,
                        gamma = c( 10 ),
                        R = c( 12 ),
                        S = NULL,
                        T = 36,
                        minfup = 24,
                        ratio = 1 )
  
  info <- x$n.I
  a <- -Inf
  b <- Inf
  b <- gs_spending_bound()
  hgm1_0 <- h1(theta = 0,
               I = info[1],
               a = a,
               b = b)
  b2 <- gs_spending_bound(k = 2,
                          theta = 0,
                          hgm1 = hgm1_0,
                          info = info)
  hgm2_0<-hupdate(theta = 0,
                  I = info[2],
                  a = a,
                  b = b2,
                  Im1 = info[1],
                  gm1 = hgm1_0)
  b3 <- gs_spending_bound(k = 3,
                          theta = 0,
                          hgm1 = hgm2_0,
                          info = info)
  test1<- cbind(b, b2, b3)
  
  a <- gs_spending_bound(k = 1,
                         par = list(sf = gsDesign::sfLDOF, total_spend = 0.2, param = NULL, timing = NULL,max_info = NULL),
                         theta = x$theta[2],
                         hgm1 = NULL,
                         efficacy = FALSE,
                         info = info)
  hgm1_1 <- h1(theta = x$theta[2], I = info[1], a = a, b = b)
  
  a2 <- gs_spending_bound(k = 2,
                          par = list(sf = gsDesign::sfLDOF, total_spend = 0.2, param = NULL, timing = NULL, max_info = NULL),
                          theta = x$theta[2],
                          hgm1 = hgm1_1,
                          efficacy = FALSE,
                          info = info)
  
  hgm2_1 <- hupdate(theta = x$theta[2],
                    I = info[2],
                    a = a2,
                    b = b2,
                    Im1=info[1],
                    gm1 = hgm1_1,
                    thetam1 = x$theta[2])
  
  a3 <- gs_spending_bound(k = 3,
                          par = list(sf = gsDesign::sfLDOF, total_spend = 0.2, param = NULL, timing = NULL  ,max_info = NULL),
                          theta = x$theta[2],
                          hgm1 = hgm2_1,
                          efficacy = FALSE,
                          info = info)
  
  test2 <- cbind(a,a2,a3)
  
  expect_equal(object = as.numeric(test1), expected = x$upper$bound, tolerance = 0.0001)
  expect_equal(object = as.numeric(test2), expected = x$lower$bound, tolerance = 0.0001)
  
})


test_that("compare gs_spending_bound with gsDesign results with unequal IA timing for upper and lower bound", {
  y<- gsDesign::gsSurv(k = 3,
                       test.type = 4,
                       alpha = 0.025,
                       beta = 0.2,
                       timing = c( 0.6,0.8),
                       sfu = gsDesign::sfLDOF,
                       sfupar = c( 0 ),
                       sfl = gsDesign::sfLDOF,
                       sflpar = c( 0 ),
                       lambdaC = c( 0.1 ),
                       hr = 0.6,
                       hr0 = 1,
                       eta = 0.01,
                       gamma = c( 10 ),
                       R = c( 12 ),
                       S = NULL,
                       T = 36,
                       minfup = 24,
                       ratio = 1 )
  info <- y$n.I
  a <- -Inf
  b <- Inf
  b <- gs_spending_bound(k = 1,
                         theta = 0,
                         hgm1 = NULL,
                         info = info)
  hgm1_0 <- h1(theta = 0,
               I = info[1],
               a = a,
               b = b)
  b2 <- gs_spending_bound(k = 2,
                          theta = 0,
                          hgm1 = hgm1_0,
                          info=info)
  hgm2_0 <- hupdate(theta = 0,
                    I = info[2],
                    a = a,
                    b = b2,
                    Im1 = info[1],
                    gm1 = hgm1_0)
  b3 <- gs_spending_bound(k = 3,
                          theta = 0,
                          hgm1 = hgm2_0,
                          info = info)
  
  test3<-cbind(b, b2, b3)
  a <- gs_spending_bound(k = 1,
                         par = list(sf = gsDesign::sfLDOF, total_spend = 0.2, param = NULL, timing = NULL, max_info = NULL),
                         theta = y$theta[2],
                         hgm1 = NULL,
                         efficacy = FALSE,
                         info = info)
  
  hgm1_1 <- h1(theta = y$theta[2], I = info[1], a = a, b = b)
  
  a2 <- gs_spending_bound(k = 2,
                          par = list(sf = gsDesign::sfLDOF, total_spend = 0.2, param = NULL, timing = NULL, max_info = NULL),
                          theta = y$theta[2],
                          hgm1 = hgm1_1,
                          efficacy = FALSE,
                          info = info)
  
  hgm2_1 <- hupdate(theta = y$theta[2],
                    I = info[2],
                    a = a2,
                    b = b2,
                    Im1 = info[1],
                    gm1 = hgm1_1,
                    thetam1 = y$theta[2])
  
  a3 <- gs_spending_bound(k = 3, par = list(sf = gsDesign::sfLDOF,
                                            total_spend = 0.2,
                                            param = NULL,
                                            timing = NULL  ,
                                            max_info = NULL),theta = y$theta[2], hgm1 = hgm2_1, efficacy=FALSE, info=info)
  
  test4 <- cbind(a, a2, a3)
  
  expect_equal(object = as.numeric(test3), expected = y$upper$bound, tolerance = 0.0001)
  expect_equal(object = as.numeric(test4), expected = y$lower$bound, tolerance = 0.0001)
})