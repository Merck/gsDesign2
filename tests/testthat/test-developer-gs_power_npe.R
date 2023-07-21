source_files <- list.files("./old_function/", "*.R$")
sapply(paste0("./old_function/", source_files), source)

library(dplyr)

test_that("The default of `gs_power_npe` is a single analysis with type I error controlled.", {
  x1 <- gs_power_npe(theta = 0) %>%
    filter(bound == "upper") %>%
    select(-info_frac)
  x2 <- gs_power_npe_(theta = 0) %>%
    filter(Bound == "Upper") %>%
    rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) %>%
    mutate(bound = tolower(bound))
  expect_equal(x1, x2)
})

test_that("fixed bound", {
  x1 <- gs_power_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    upper = gs_b,
    upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b,
    lpar = c(-1, 0, 0)
  ) %>%
    select(-info_frac)
  x2 <- gs_power_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    upper = gs_b,
    upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b,
    lpar = c(-1, 0, 0)
  ) %>%
    rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) %>%
    mutate(bound = tolower(bound))
  expect_equal(x1, x2)
})

test_that("Same fixed efficacy bounds, no futility bound (i.e., non-binding bound), null hypothesis", {
  x1 <- gs_power_npe(
    theta = rep(0, 3),
    info = (1:3) * 40,
    upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lpar = rep(-Inf, 3)
  ) %>%
    select(-info_frac)
  x2 <- gs_power_npe_(
    theta = rep(0, 3),
    info = (1:3) * 40,
    upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lpar = rep(-Inf, 3)
  ) %>%
    rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) %>%
    mutate(bound = tolower(bound))
  expect_equal(x1, x2)
})

test_that("Fixed bound with futility only at analysis 1; efficacy only at analyses 2, 3", {
  x1 <- gs_power_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    upper = gs_b,
    upar = c(Inf, 3, 2),
    lower = gs_b,
    lpar = c(qnorm(.1), -Inf, -Inf)
  ) %>%
    select(-info_frac)
  x2 <- gs_power_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    upper = gs_b,
    upar = c(Inf, 3, 2),
    lower = gs_b,
    lpar = c(qnorm(.1), -Inf, -Inf)
  ) %>%
    rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) %>%
    mutate(bound = tolower(bound))
  expect_equal(x1, x2)
})

test_that("Spending function bounds - Lower spending based on non-zero effect", {
  x1 <- gs_power_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
  ) %>%
    select(-info_frac)
  x2 <- gs_power_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
  ) %>%
    rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) %>%
    mutate(bound = tolower(bound))
  expect_equal(x1, x2)
})

test_that("Same bounds, but power under different theta", {
  x1 <- gs_power_npe(
    theta = c(.15, .25, .35),
    info = (1:3) * 40,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
  ) %>%
    select(-info_frac)
  x2 <- gs_power_npe_(
    theta = c(.15, .25, .35),
    info = (1:3) * 40,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
  ) %>%
    rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) %>%
    mutate(bound = tolower(bound))
  expect_equal(x1, x2)
})

test_that("Two-sided symmetric spend, O'Brien-Fleming spending", {
  x1 <- gs_power_npe(
    theta = rep(0, 3),
    info = (1:3) * 40,
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  ) %>%
    select(-info_frac)
  x2 <- gs_power_npe_(
    theta = rep(0, 3),
    info = (1:3) * 40,
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  ) %>%
    rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) %>%
    mutate(bound = tolower(bound))
  expect_equal(x1, x2)
})

test_that("Re-use these bounds under alternate hypothesis - Always use binding = TRUE for power calculations", {
  x <- gs_power_npe(
    theta = rep(0, 3),
    info = (1:3) * 40,
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  ) %>%
    select(-info_frac)
  x1 <- gs_power_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    binding = TRUE,
    upar = (x %>% filter(bound == "upper"))$z,
    lpar = -(x %>% filter(bound == "upper"))$z
  ) %>%
    select(-info_frac)
  x2 <- gs_power_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    binding = TRUE,
    upar = (x %>% filter(bound == "upper"))$z,
    lpar = -(x %>% filter(bound == "upper"))$z
  ) %>%
    rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) %>%
    mutate(bound = tolower(bound))
  expect_equal(x1, x2)
})


test_that("info != info0 != info1 - If one inputs info in upar", {
  x1_a <- gs_power_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info0 = (1:3) * 90 + 10, info1 = (1:3) * 70 - 5, info_scale = "h0_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) %>%
    select(-info_frac)
  x1_b <- gs_power_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info0 = (1:3) * 90 + 10, info1 = (1:3) * 70 - 5, info_scale = "h1_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) %>%
    select(-info_frac)
  x1_c <- gs_power_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info0 = (1:3) * 90 + 10, info1 = (1:3) * 70 - 5, info_scale = "h0_h1_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) %>%
    select(-info_frac)
  x2 <- gs_power_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info0 = (1:3) * 90 + 10, info1 = (1:3) * 70 - 5,
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) %>%
    rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) %>%
    mutate(bound = tolower(bound))
  expect_equal(x1_c, x2)
})

test_that("Developer Tests 1-sided test", {
  r <- 80
  x <- gs_power_npe(
    theta = 0,
    info = (1:3) * 400,
    binding = FALSE, r = r,
    upper = gs_b, # gs_spending_bound,
    upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF)$upper$bound,
    lower = gs_b,
    lpar = rep(-Inf, 3)
  ) %>%
    select(-info_frac)
  y <- gs_power_npe_(
    theta = 0,
    info = (1:3) * 400,
    binding = FALSE, r = r,
    upper = gs_b, # gs_spending_bound,
    upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF)$upper$bound,
    lower = gs_b,
    lpar = rep(-Inf, 3)
  ) %>%
    rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) %>%
    mutate(bound = tolower(bound))
  z <- gsDesign::gsProbability(
    k = 3,
    theta = 0,
    n.I = (1:3) * 400,
    b = gsDesign(k = 3, test.type = 1, sfu = sfLDOF)$upper$bound, a = rep(-20, 3), r = r
  )
  expect_equal(x, y)
  expect_equal(x$z[x$bound == "upper"], z$upper$bound)
  expect_equal(x$probability[x$bound == "upper"], cumsum(z$upper$prob))
})

test_that("Independent Tests - Expect equal with mvtnorm for efficacy and futility bounds", {
  info <- c(40, 100)
  r <- info[1] / info[2]
  test <- gs_power_npe(
    theta = 0,
    info = info,
    info0 = NULL,
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, param = NULL, total_spend = 0.025),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, param = NULL, total_spend = 0.02)
  )
  test1 <- test %>% filter(bound == "upper")
  test2 <- test %>% filter(bound == "lower")
  alpha_t <- 0.025
  b_ia <- gsDesign::sfLDOF(alpha = alpha_t, t = r)
  alpha_ia <- b_ia$spend
  prob_b <- function(alpha_t, alpha_ia, r, b) {
    temp <- mvtnorm::pmvnorm(
      lower = c(-Inf, b),
      upper = c(qnorm(1 - alpha_ia), Inf),
      corr = rbind(c(1, sqrt(r)), c(sqrt(r), 1))
    )
    return(alpha_t - alpha_ia - temp)
  }
  b <- uniroot(prob_b, c(1.96, 4), alpha_t = alpha_t, alpha_ia = alpha_ia, r = r)
  pb <- 1 - pnorm(b$root)
  expect_equal(
    object = test1$z,
    expected = c(qnorm(1 - alpha_ia), b$root),
    tolerance = 0.001
  )
  expect_equal(
    object = test1$probability,
    expected = cumsum(c(b_ia$spend, pb)),
    tolerance = 0.001
  )
  beta_t <- 0.02
  a_ia <- gsDesign::sfLDOF(alpha = beta_t, t = r)
  beta_ia <- a_ia$spend
  prob_a <- function(beta_t, beta_ia, r, a) {
    temp <- mvtnorm::pmvnorm(
      lower = c(-Inf, qnorm(beta_ia)),
      upper = c(a, Inf),
      corr = rbind(c(1, sqrt(r)), c(sqrt(r), 1))
    )
    return(beta_t - beta_ia - temp)
  }
  a <- uniroot(prob_a, c(-4, 1.96), beta_t = beta_t, beta_ia = beta_ia, r = r)
  pa <- pnorm(a$root)
  expect_equal(
    object = test2$z,
    expected = c(qnorm(beta_ia), a$root),
    tolerance = 0.001
  )
  expect_equal(
    object = test2$probability,
    expected = cumsum(c(a_ia$spend, pa)),
    tolerance = 0.001
  )
})

test_that("Expect equal with gsDesign::gsProbability outcome for efficacy bounds", {
  info <- c(40, 150, 200)
  x <- gs_power_npe(
    theta = .1,
    info = info, binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, param = NULL, total_spend = 0.025),
    lower = gs_b,
    lpar = rep(-Inf, 3)
  ) %>%
    filter(bound == "upper")
  y <- gs_power_npe(
    theta = .1,
    info = info, binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, param = NULL, total_spend = 0.025),
    lower = gs_b,
    lpar = rep(-Inf, 3)
  ) %>%
    filter(bound == "upper")
  z <- gsDesign::gsProbability(
    k = 3, theta = .1,
    n.I = info,
    a = rep(-20, 3),
    b = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, n.I = info)$upper$bound
  )
  expect_equal(x, y)
  expect_equal(x$z[x$bound == "upper"], z$upper$bound, tolerance = 1e-5)
  expect_equal(x$probability[x$bound == "upper"], cumsum(z$upper$prob), tolerance = 1e-5)
})
