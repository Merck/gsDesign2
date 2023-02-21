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
  gsd <- gsDesign(
    test.type = 1, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
    delta = delta, alpha = alpha, beta = beta
  )
  gsdv <- gs_design_npe(
    theta = delta, info = timing, beta = beta,
    upper = gs_spending_bound,
    upar = list(sf = sfu, total_spend = alpha, param = sfupar),
    lower = gs_b,
    lpar = rep(-Inf, K)
  ) %>% filter(bound == "upper")

  # compare boundaries
  expect_equal(gsd$upper$bound, gsdv$z, tolerance = 7e-6)
  expect_equal(gsd$n.I, gsdv$info, tolerance = .001)

  # compare statistical information
  # While tolerance should not be problematic, it seems large
  expect_equal(gsd$n.I, (gsdv %>% filter(bound == "upper"))$info, tolerance = .04)

  # compare crossing boundaries probability
  expect_equal(gsdv$probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
})


test_that("Two-sided symmetric design fails to reproduce gsDesign test.type=2 bounds", {
  gsd <- gsDesign(
    test.type = 2, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
    delta = delta, alpha = alpha, beta = beta, tol = 1e-6
  )
  gsdv <- gs_design_npe(
    theta = delta, info = timing, beta = beta,
    theta1 = rep(0, 3), # Use this for lower bound spending under null hypothesis
    binding = TRUE, # Use this for 2-sided symmetric design
    upper = gs_spending_bound,
    upar = list(sf = sfu, total_spend = alpha, param = sfupar),
    lower = gs_spending_bound,
    lpar = list(sf = sfu, total_spend = alpha, param = sfupar),
    tol = 1e-6
  )
  # compare boundaries
  expect_equal(gsd$upper$bound, (gsdv %>% filter(bound == "upper"))$z, tolerance = 7e-6)
  expect_equal(gsd$lower$bound, (gsdv %>% filter(bound == "lower"))$z, tolerance = 7e-6)

  # compare statistical information
  # While tolerance should not be problematic, it seems large
  expect_equal(gsd$n.I, (gsdv %>% filter(bound == "upper"))$info, tolerance = .04)

  # compare crossing boundaries probability
  expect_equal((gsdv %>% filter(bound == "upper"))$probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
})

test_that("Two-sided asymmetric design fails to reproduce gsDesign test.type=3 bounds", {
  gsd <- gsDesign(
    test.type = 3, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
    delta = delta, alpha = alpha, beta = beta
  )

  gsdv <- gs_design_npe(
    theta = delta, info = timing, beta = beta,
    binding = TRUE, # Use this for test.type=3 and 5
    upper = gs_spending_bound,
    upar = list(sf = sfu, total_spend = alpha, param = sfupar),
    lower = gs_spending_bound,
    lpar = list(sf = sfl, total_spend = beta, param = sflpar)
  )
  # compare boundaries
  expect_equal(gsd$upper$bound, (gsdv %>% filter(bound == "upper"))$z, tolerance = 7e-6)
  expect_equal(gsd$lower$bound, (gsdv %>% filter(bound == "lower"))$z, tolerance = 9e-6)

  # compare statistical information
  # While tolerance should not be problematic, it seems large
  expect_equal(gsd$n.I, (gsdv %>% filter(bound == "upper"))$info, tolerance = .04)

  # compare crossing boundaries probability under null hypothesis (theta = 0)
  expect_equal((gsdv %>% filter(bound == "upper"))$probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  expect_equal((gsdv %>% filter(bound == "lower"))$probability, sfl(alpha = beta, t = timing, param = sflpar)$spend)
})

test_that("Two-sided asymmetric design fails to reproduce gsDesign test.type=4 bounds", {
  gsd <- gsDesign(
    test.type = 4, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
    delta = delta, alpha = alpha, beta = beta
  )

  gsdv <- gs_design_npe(
    theta = delta, info = timing, beta = beta,
    binding = FALSE, # Use this for test.type=4 and 6
    upper = gs_spending_bound,
    upar = list(sf = sfu, total_spend = alpha, param = sfupar),
    lower = gs_spending_bound,
    lpar = list(sf = sfl, total_spend = beta, param = sflpar)
  )

  # compare boundaries
  expect_equal(gsd$upper$bound, (gsdv %>% filter(bound == "upper"))$z, tolerance = 7e-6)
  expect_equal(gsd$lower$bound, (gsdv %>% filter(bound == "lower"))$z, tolerance = 9e-6)

  # compare statistical information
  # While tolerance should not be problematic, it seems large
  expect_equal(gsd$n.I, (gsdv %>% filter(bound == "upper"))$info, tolerance = .04)

  # compare crossing boundaries probability under null hypothesis (theta = 0)
  expect_equal(
    gsdv$probability0,
    gsDesign::gsBoundSummary(gsd) %>%
      subset(Value == "P(Cross) if delta=0") %>%
      select(Efficacy, Futility) %>%
      t() %>%
      as.numeric(),
    tolerance = .0001
  )

  expect_equal(
    gsdv$probability,
    gsDesign::gsBoundSummary(gsd) %>%
      subset(Value == "P(Cross) if delta=1") %>%
      select(Efficacy, Futility) %>%
      t() %>%
      as.numeric(),
    tolerance = .0001
  )
})


test_that("Two-sided asymmetric design fails to reproduce gsDesign test.type=5 bounds", {
  astar <- 0.2
  gsd <- gsDesign(
    test.type = 5, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
    delta = delta, alpha = alpha, beta = beta, astar = astar
  )
  gsdv <- gs_design_npe(
    theta = delta, info = timing, beta = beta,
    theta1 = 0, # Spending for lower bound under H0
    binding = TRUE, # Use this for test.type=3 and 5
    upper = gs_spending_bound,
    upar = list(sf = sfu, total_spend = alpha, param = sfupar),
    lower = gs_spending_bound,
    lpar = list(sf = sfl, total_spend = astar, param = sflpar)
  )

  # compare boundaries
  expect_equal(gsd$upper$bound, (gsdv %>% filter(bound == "upper"))$z, tolerance = 7e-6)
  expect_equal(gsd$lower$bound, (gsdv %>% filter(bound == "lower"))$z, tolerance = 9e-6)

  # compare statistical information
  # While tolerance should not be problematic, it seems large
  expect_equal(gsd$n.I, (gsdv %>% filter(bound == "upper"))$info, tolerance = .04)

  # compare crossing boundaries probability under null hypothesis (theta = 0)
  expect_equal((gsdv %>% filter(bound == "upper"))$probability0, sfu(alpha = alpha, t = timing, param = sfupar)$spend)
  expect_equal(
    (gs_power_npe(
      theta = 0, info = (gsdv %>% filter(bound == "upper"))$info,
      theta1 = 0, # Spending for lower bound under H0
      binding = TRUE, # Use this for test.type=3 and 5
      upper = gs_spending_bound,
      upar = list(sf = sfu, total_spend = alpha, param = sfupar),
      lower = gs_spending_bound,
      lpar = list(sf = sfl, total_spend = astar, param = sflpar)
    ) %>%
      filter(bound == "lower")
    )$probability,
    sfl(alpha = astar, t = timing, param = sflpar)$spend
  )
})

test_that("Two-sided asymmetric design fails to reproduce gsDesign test.type=6 bounds", {
  astar <- 0.2
  gsd <- gsDesign(
    test.type = 6, k = K, sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar, timing = timing,
    delta = delta, alpha = alpha, beta = beta, astar = astar
  )
  gsdv <- gs_design_npe(
    theta = delta, info = timing, beta = beta,
    theta1 = 0, # Spending for lower bound under H0
    binding = FALSE, # Use this for test.type=3 and 5
    upper = gs_spending_bound,
    upar = list(sf = sfu, total_spend = alpha, param = sfupar),
    lower = gs_spending_bound,
    lpar = list(sf = sfl, total_spend = astar, param = sflpar)
  )

  # compare boundaries
  expect_equal(gsd$upper$bound, (gsdv %>% filter(bound == "upper"))$z, tolerance = 7e-6)
  expect_equal(gsd$lower$bound, (gsdv %>% filter(bound == "lower"))$z, tolerance = 9e-6)

  # compare statistical information
  # While tolerance should not be problematic, it seems large
  expect_equal(gsd$n.I, (gsdv %>% filter(bound == "upper"))$info, tolerance = .04)

  # compare crossing boundaries probability under null hypothesis (theta = 0)
  expect_equal(
    (gsdv %>% filter(bound == "upper"))$probability0,
    sfu(alpha = alpha, t = timing, param = sfupar)$spend,
    tolerance = 1e-5
  )
  expect_equal(
    (gs_power_npe(
      theta = 0, info = (gsdv %>% filter(bound == "upper"))$info,
      theta1 = 0, # Spending for lower bound under H0
      binding = TRUE, # Use this for test.type=3 and 5
      upper = gs_spending_bound,
      upar = list(sf = sfu, total_spend = alpha, param = sfupar),
      lower = gs_spending_bound,
      lpar = list(sf = sfl, total_spend = astar, param = sflpar)
    ) %>%
      filter(bound == "lower")
    )$probability,
    sfl(alpha = astar, t = timing, param = sflpar)$spend
  )
})
