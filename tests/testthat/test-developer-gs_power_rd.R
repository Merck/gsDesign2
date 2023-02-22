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
