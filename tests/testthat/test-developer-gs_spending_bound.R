test_that("gs_spending_bound() does not execute as expected", {
  expect_true(is.numeric(b <- gs_spending_bound()))
  expect_true(is.numeric(a <- gs_spending_bound(efficacy=FALSE)))
  hgm1_0 <- h1(theta=0, I = 1, a = a, b = b)
  hgm1_1 <- h1(theta=.1, I = 1, a = a, b = b)
  expect_true(is.numeric(b2 <- gs_spending_bound(k = 2, theta = 0, hgm1 = hgm1_0)))
  expect_true(is.numeric(a2 <- gs_spending_bound(k = 2, theta = .1, hgm1 = hgm1_1, efficacy = FALSE)))
})


# Parameters used repeatedly
library(gsDesign)
library(gsDesign2)
K <- 3
timing <- c(.45, .8, 1)
sfu <- gsDesign::sfPower
sfupar <- 4
sfl <- gsDesign::sfHSD
sflpar <- 2
delta <- .2
alpha <- .02
beta <- .15


test_that("One-sided design fails to reproduce gsDesign package bounds", {
  gsd <- gsDesign(test.type = 1, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
                  delta = delta, alpha = alpha, beta = beta)
  gsdv <- gs_design_npe(theta = delta, info = timing, beta = beta,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_b,
                        lpar = rep(-Inf, K)) %>% filter(Bound == "Upper")
  expect_equal(gsd$upper$bound, gsdv$Z, tolerance = 7e-6)
  expect_equal(gsd$n.I, gsdv$info, tolerance = .001)
  
  # get design properties under null hypothesis (theta = 0)
  gsdv0 <- gs_power_npe(theta = 0, info = gsdv$info,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_b,
                        lpar = rep(-Inf, K)) %>% filter(Bound == "Upper")
  expect_equal(gsdv0$Probability, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
  # get design properties under null hypothesis (theta = 0)
  expect_equal(gsdv$Probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
})


test_that("Two-sided symmetric design fails to reproduce gsDesign test.type=2 bounds", {
  gsd <- gsDesign(test.type = 2, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
                  delta = delta, alpha = alpha, beta = beta, tol = 1e-6)
  gsdv <- gs_design_npe(theta = delta, info = timing, beta = beta,
                        theta1 = rep(0,3), # Use this for lower bound spending under null hypothesis
                        binding = TRUE, # Use this for 2-sided symmetric design
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        tol = 1e-6)
  
  expect_equal(gsd$upper$bound, (gsdv %>% filter(Bound == "Upper"))$Z, tolerance = 7e-6)
  expect_equal(gsd$lower$bound, (gsdv %>% filter(Bound == "Lower"))$Z, tolerance = 7e-6)
  expect_equal(gsd$n.I, (gsdv %>% filter(Bound == "Upper"))$info, tolerance = .04) # While tolerance should not be problematic, it seems large
  
  # get design properties under null hypothesis (theta = 0)
  gsdv0 <- gs_power_npe(theta = 0, info = (gsdv %>% filter(Bound == "Upper"))$info,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfu, total_spend = alpha, param = sfupar))
  
  expect_equal((gsdv0 %>% filter(Bound == "Upper"))$Probability, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
  # get design properties under null hypothesis (theta = 0)
  expect_equal((gsdv %>% filter(Bound == "Upper"))$Probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
})

test_that("Two-sided asymmetric design fails to reproduce gsDesign test.type=3 bounds", {
  gsd <- gsDesign(test.type = 3, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
                  delta = delta, alpha = alpha, beta = beta)
  
  gsdv <- gs_design_npe(theta = delta, info = timing, beta = beta,
                        binding = TRUE, # Use this for test.type=3 and 5
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfl, total_spend = beta, param = sflpar))
  
  expect_equal(gsd$upper$bound, (gsdv %>% filter(Bound == "Upper"))$Z, tolerance = 7e-6)
  expect_equal(gsd$lower$bound, (gsdv %>% filter(Bound == "Lower"))$Z, tolerance = 9e-6)
  expect_equal(gsd$n.I, (gsdv %>% filter(Bound == "Upper"))$info, tolerance = .04) # While tolerance should not be problematic, it seems large
  
  # get design properties under null hypothesis (theta = 0)
  gsdv0 <- gs_power_npe(theta = 0, info = (gsdv %>% filter(Bound == "Upper"))$info,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfu, total_spend = alpha, param = sfupar))
  expect_equal((gsdv0 %>% filter(Bound == "Upper"))$Probability, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
  # get design properties under null hypothesis (theta = 0)
  expect_equal((gsdv %>% filter(Bound == "Upper"))$Probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
})

test_that("Two-sided asymmetric design fails to reproduce gsDesign test.type=4 bounds", {
  gsd <- gsDesign(test.type = 4, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
                  delta = delta, alpha = alpha, beta = beta)
  gsdv <- gs_design_npe(theta = delta, info = timing, beta = beta,
                        binding = FALSE, # Use this for test.type=4 and 6
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfl, total_spend = beta, param = sflpar))
  
  expect_equal(gsd$upper$bound, (gsdv %>% filter(Bound == "Upper"))$Z, tolerance = 7e-6)
  expect_equal(gsd$lower$bound, (gsdv %>% filter(Bound == "Lower"))$Z, tolerance = 9e-6)
  expect_equal(gsd$n.I, (gsdv %>% filter(Bound == "Upper"))$info, tolerance = .04) # While tolerance should not be problematic, it seems large
  
  # get design properties under null hypothesis (theta = 0)
  gsdv0 <- gs_power_npe(theta = 0, info = (gsdv %>% filter(Bound == "Upper"))$info,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_b,
                        lpar = rep(-Inf, K)) %>% filter(Bound == "Upper")
  expect_equal(gsdv0$Probability, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
  # get design properties under null hypothesis (theta = 0)
  #expect_equal((gsdv %>% filter(Bound == "Upper"))$Probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
})


test_that("Two-sided asymmetric design fails to reproduce gsDesign test.type=5 bounds", {
  astar <- 0.2
  gsd <- gsDesign(test.type = 5, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
                  delta = delta, alpha = alpha, beta = beta, astar = astar)
  gsdv <- gs_design_npe(theta = delta, info = timing, beta = beta,
                        theta1 = 0, # Spending for lower bound under H0
                        binding = TRUE, # Use this for test.type=3 and 5
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfl, total_spend = astar, param = sflpar))
  
  expect_equal(gsd$upper$bound, (gsdv %>% filter(Bound == "Upper"))$Z, tolerance = 7e-6)
  expect_equal(gsd$lower$bound, (gsdv %>% filter(Bound == "Lower"))$Z, tolerance = 9e-6)
  expect_equal(gsd$n.I, (gsdv %>% filter(Bound == "Upper"))$info, tolerance = .04) # While tolerance should not be problematic, it seems large
  # get design properties under null hypothesis (theta = 0)
  gsdv0 <- gs_power_npe(theta = 0, info = (gsdv %>% filter(Bound == "Upper"))$info,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfu, total_spend = alpha, param = sfupar))
  expect_equal((gsdv0 %>% filter(Bound == "Upper"))$Probability, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
  # get design properties under null hypothesis (theta = 0)
  expect_equal((gsdv %>% filter(Bound == "Upper"))$Probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
})

test_that("Two-sided asymmetric design fails to reproduce gsDesign test.type=6 bounds", {
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
  
  expect_equal(gsd$upper$bound, (gsdv %>% filter(Bound == "Upper"))$Z, tolerance = 7e-6)
  expect_equal(gsd$lower$bound, (gsdv %>% filter(Bound == "Lower"))$Z, tolerance = 9e-6)
  expect_equal(gsd$n.I, (gsdv %>% filter(Bound == "Upper"))$info, tolerance = .04) # While tolerance should not be problematic, it seems large
  # get design properties under null hypothesis (theta = 0)
  gsdv0 <- gs_power_npe(theta = 0, info = (gsdv %>% filter(Bound == "Upper"))$info,
                        upper = gs_spending_bound,
                        upar = list(sf = sfu, total_spend = alpha, param = sfupar),
                        lower = gs_spending_bound,
                        lpar = list(sf = sfu, total_spend = alpha, param = sfupar))
  expect_equal((gsdv0 %>% filter(Bound == "Upper"))$Probability, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  
  # get design properties under null hypothesis (theta = 0)
  expect_equal((gsdv %>% filter(Bound == "Upper"))$Probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend, tolerance = 1e-3)
  
})