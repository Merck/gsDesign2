library(gsDesign)
# Parameters used repeatedly
K <- 3
timing <- c(.6, .8, 1)
sfu <- sfHSD
sfupar <- -12
sfl <- sfPower
sflpar <- 4
delta <- .2
alpha <- .025
beta <- .1

testthat::test_that("One-sided design to reproduce gsDesign package bounds", {
  gsd <- gsDesign::gsDesign(test.type = 1, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
                            delta = delta, alpha = alpha, beta = beta)
  gsdv <- gs_design_npe(theta = delta, info = timing, beta = beta,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_b,
                        lpar = rep(-Inf, K)
  ) %>% filter(Bound == "Upper")
  expect_equal(gsd$upper$bound, gsdv$Z, tolerance = 0.0001)
  expect_equal(gsd$n.I, gsdv$info, tolerance = 0.5)
  
  gsdv0 <- gs_power_npe(theta = 0, info = gsdv$info,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_b,
                        lpar = rep(-Inf, K)) %>% filter(Bound == "Upper")
  expect_equal(gsdv0$Probability, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
  expect_equal((gsdv %>% filter(Bound == "Upper"))$Probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
})


testthat::test_that("Two-sided symmetric design to reproduce gsDesign test.type=2 bounds", {
  gsd <- gsDesign(test.type = 2, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
                  delta = delta, alpha = alpha, beta = beta, tol = 1e-6)
  gsdv <- gs_design_npe(theta = delta, info = timing, beta = beta,
                        theta1 = rep(0,3),
                        binding = FALSE,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        tol = 1e-6)
  
  expect_equal(gsd$upper$bound, (gsdv %>% filter(Bound == "Upper"))$Z, tolerance = 0.0001)
  expect_equal(gsd$lower$bound, (gsdv %>% filter(Bound == "Lower"))$Z, tolerance = 0.0001)
  expect_equal(gsd$n.I, (gsdv %>% filter(Bound == "Upper"))$info, tolerance = 0.5)
  
  gsdv0 <- gs_power_npe(theta = 0, info = (gsdv %>% filter(Bound == "Upper"))$info,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfu, total_spend = alpha, param = sfupar))
  expect_equal(gsdv0$Probability[1:K], sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
  expect_equal((gsdv %>% filter(Bound == "Upper"))$Probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  expect_equal((gsdv %>% filter(Bound == "Lower"))$Probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
})

testthat::test_that("Two-sided asymmetric design to reproduce gsDesign test.type=3 bounds", {
  gsd <- gsDesign(test.type = 3, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
                  delta = delta, alpha = alpha, beta = beta)
  gsdv <- gs_design_npe(theta = delta, info = timing, beta = beta,
                        binding = TRUE,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfl, total_spend = beta, param = sflpar))
  
  expect_equal(gsd$upper$bound, (gsdv%>% filter(Bound == "Upper"))$Z, tolerance = 0.0001)
  expect_equal(gsd$lower$bound, (gsdv%>% filter(Bound == "Lower"))$Z, tolerance = 0.0001)
  expect_equal(gsd$n.I, (gsdv %>% filter(Bound == "Upper"))$info, tolerance = 0.5)
  
  gsdv0 <- gs_power_npe(theta = 0, info = (gsdv %>% filter(Bound == "Upper"))$info,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfu, total_spend = alpha, param = sfupar))
  expect_equal((gsdv0 %>% filter(Bound == "Upper"))$Probability, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
  expect_equal((gsdv %>% filter(Bound == "Upper"))$Probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  #expect_equal((gsdv %>% filter(Bound == "Lower"))$Probability0, sfu(alpha = beta, t = timing, param = sflpar)$spend)
})

testthat::test_that("Two-sided asymmetric design fails to reproduce gsDesign test.type=4 bounds", {
  gsd <- gsDesign(test.type = 4, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
                  delta = delta, alpha = alpha, beta = beta)
  gsdv <- gs_design_npe(theta = delta, info = timing, beta = beta,
                        binding = FALSE, # Use this for test.type=4 and 6
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfl, total_spend = beta, param = sflpar))
  
  expect_equal(gsd$upper$bound, (gsdv%>% filter(Bound == "Upper"))$Z, tolerance = 0.0001)
  expect_equal(gsd$lower$bound, (gsdv%>% filter(Bound == "Lower"))$Z, tolerance = 0.0001)
  expect_equal(gsd$n.I, (gsdv %>% filter(Bound == "Upper"))$info, tolerance = 0.5)
  
  
  gsdv0 <- gs_power_npe(theta = 0, info = (gsdv %>% filter(Bound == "Upper"))$info,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_b,
                        lpar = rep(-Inf, K)) 
  expect_equal((gsdv0 %>% filter(Bound == "Upper"))$Probability, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
  expect_equal((gsdv %>% filter(Bound == "Upper"))$Probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend, tolerance = 0.01)
})


testthat::test_that("Two-sided asymmetric design fails to reproduce gsDesign test.type=5 bounds", {
  astar <- 0.2
  gsd <- gsDesign(test.type = 5, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
                  delta = delta, alpha = alpha, beta = beta, astar = astar)
  gsdv <- gs_design_npe(theta = delta, info = timing, beta = beta,
                        theta1 = 0,
                        binding = TRUE,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfl, total_spend = astar, param = sflpar))
  
  expect_equal(gsd$upper$bound, (gsdv%>% filter(Bound == "Upper"))$Z, tolerance = 0.0001)
  expect_equal(gsd$lower$bound, (gsdv%>% filter(Bound == "Lower"))$Z, tolerance = 0.0001)
  expect_equal(gsd$n.I, (gsdv %>% filter(Bound == "Upper"))$info, tolerance = 0.5)
  
  
  gsdv0 <- gs_power_npe(theta = 0, info = (gsdv %>% filter(Bound == "Upper"))$info,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfu, total_spend = alpha, param = sfupar))
  expect_equal((gsdv0 %>% filter(Bound == "Upper"))$Probability, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
  expect_equal((gsdv %>% filter(Bound == "Upper"))$Probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
})

testthat::test_that("Two-sided asymmetric design fails to reproduce gsDesign test.type=6 bounds", {
  astar <- 0.2
  gsd <- gsDesign(test.type = 6, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
                  delta = delta, alpha = alpha, beta = beta, astar = astar)
  gsdv <- gs_design_npe(theta = delta, info = timing, beta = beta,
                        theta1 = 0,      # Spending for lower bound under H0
                        binding = FALSE, # Use this for test.type=3 and 5
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfl, total_spend = astar, param = sflpar))
  
  expect_equal(gsd$upper$bound, (gsdv%>% filter(Bound == "Upper"))$Z, tolerance = 0.0001)
  expect_equal(gsd$lower$bound, (gsdv%>% filter(Bound == "Lower"))$Z, tolerance = 0.0001)
  expect_equal(gsd$n.I, (gsdv %>% filter(Bound == "Upper"))$info, tolerance = 0.5)
  
  gsdv0 <- gs_power_npe(theta = 0, info = (gsdv %>% filter(Bound == "Upper"))$info,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfu, total_spend = alpha, param = sfupar))
  expect_equal((gsdv0 %>% filter(Bound == "Upper"))$Probability, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
  expect_equal((gsdv %>% filter(Bound == "Upper"))$Probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
})
