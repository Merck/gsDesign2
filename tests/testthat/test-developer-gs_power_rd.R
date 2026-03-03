test_that("fixed design", {
  p1 <- 0.2
  p2 <- 0.1
  alpha <- 0.025
  beta <- 0.1
  rd0 <- 0
  ratio <- 1
  n <- 500

  x1 <- gsDesign::nBinomial(
    p1 = p1,
    p2 = p2,
    n = n,
    alpha = alpha, beta = beta,
    delta0 = rd0, ratio = ratio, sided = 1,
    outtype = 1, scale = "Difference"
  )

  x2 <- gs_power_rd(
    p_c = tibble::tibble(stratum = "All", rate = p1),
    p_e = tibble::tibble(stratum = "All", rate = p2),
    n = tibble::tibble(stratum = "All", n = n, analysis = 1),
    rd0 = rd0, ratio = 1,
    upper = gs_b,
    lower = gs_b,
    upar = -qnorm(.025),
    lpar = -Inf
  )

  expect_equal(x1, x2$bound$probability)
})

test_that("Stratified GSD: if RD is constant across strata, then MR weights are equal to the INVAR weights", {
  # Reference: Section 3 of Mehrotra, Devan V., and Radha Railkar.
  # "Minimum risk weights for comparing treatments in stratified binomial trials." Statistics in Medicine 19.6 (2000): 811-825.
  x_invar <- gs_power_rd(
    p_c = tibble::tibble(stratum = c("biomarker positive", "biomarker negative"),
                         rate = c(.2, .25)),
    p_e = tibble::tibble(stratum = c("biomarker positive", "biomarker negative"),
                         rate = c(.15, .20)),
    n = tibble::tibble(stratum = rep(c("biomarker positive", "biomarker negative"), each = 2),
                       n = c(1000, 1500, 1000, 1500),
                       analysis = c(1, 2, 1, 2)),
    rd0 = 0, ratio = 1,
    weight = "invar",
    upper = gs_spending_bound, lower = gs_b,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lpar = rep(-Inf, 2))

  x_mr <- gs_power_rd(
    p_c = tibble::tibble(stratum = c("biomarker positive", "biomarker negative"),
                         rate = c(.2, .25)),
    p_e = tibble::tibble(stratum = c("biomarker positive", "biomarker negative"),
                         rate = c(.15, .20)),
    n = tibble::tibble(stratum = rep(c("biomarker positive", "biomarker negative"), each = 2),
                       n = c(1000, 1500, 1000, 1500),
                       analysis = c(1, 2, 1, 2)),
    rd0 = 0, ratio = 1,
    weight = "mr",
    upper = gs_spending_bound, lower = gs_b,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lpar = rep(-Inf, 2))

  expect_equal(x_invar$analysis, x_mr$analysis)
  expect_equal(x_invar$bound, x_mr$bound)
})

