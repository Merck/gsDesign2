test_that("fixed design", {
  p1 <- 0.2
  p2 <- 0.1
  alpha <- 0.025
  beta <- 0.1
  rd0 <- 0
  ratio <- 1

  x1 <- gsDesign::nBinomial(
    p1 = p1,
    p2 = p2,
    alpha = alpha, beta = beta,
    delta0 = rd0, ratio = ratio, sided = 1,
    outtype = 1, scale = "Difference", n = NULL
  )

  x2 <- gs_design_rd(
    p_c = tibble(stratum = "All", rate = p1),
    p_e = tibble(stratum = "All", rate = p2),
    alpha = alpha, beta = beta,
    rd0 = rd0, ratio = 1, info_frac = 1,
    upper = gs_b,
    lower = gs_b,
    upar = -qnorm(.025),
    lpar = -Inf,
    h1_spending = FALSE
  )

  expect_equal(x1, x2$analysis$n)
})
