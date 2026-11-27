assert("Testing weights calculation", {

  # This example following the second example in the paper "Minimum risk weights for comparing treatments in stratified binomial trials"
  p_c <- data.frame(stratum = c("Stratum1", "Stratum2"), rate = c(0.48, 0.8))
  p_e <- data.frame(stratum = c("Stratum1", "Stratum2"), rate = c(0.53, 0.95))
  n <- data.frame(stratum = c("Stratum1", "Stratum2"), n = c(63, 37), analysis = 1)

  # sample size
  n_c_stratum1 <- n$n[1] / 2
  n_e_stratum1 <- n$n[1] / 2
  n_c_stratum2 <- n$n[2] / 2
  n_e_stratum2 <- n$n[2] / 2
  n_stratum1 <- n_c_stratum1 + n_e_stratum1
  n_stratum2 <- n_c_stratum2 + n_e_stratum2

  # failure rate
  p_c_stratum1 <- p_c$rate[1]
  p_e_stratum1 <- p_e$rate[1]
  p_c_stratum2 <- p_c$rate[2]
  p_e_stratum2 <- p_e$rate[2]
  p_pool_stratum1 <- (p_c_stratum1 + p_e_stratum1)/2
  p_pool_stratum2 <- (p_c_stratum2 + p_e_stratum2)/2

  # variance for each stratum under H1
  var_H1_stratum1 <- p_c_stratum1 * (1 - p_c_stratum1) / n_c_stratum1 + p_e_stratum1 * (1 - p_e_stratum1) / n_e_stratum1
  var_H1_stratum2 <- p_c_stratum2 * (1 - p_c_stratum2) / n_c_stratum2 + p_e_stratum2 * (1 - p_e_stratum2) / n_e_stratum2

  # variance for each stratum under H0
  var_H0_stratum1 <- p_pool_stratum1 * (1 - p_pool_stratum1) * (1/n_c_stratum1 + 1/n_e_stratum1)
  var_H0_stratum2 <- p_pool_stratum2 * (1 - p_pool_stratum2) * (1/n_c_stratum2 + 1/n_e_stratum2)

  # Testing the INVAR weight via the aggregated info0, info1
  # the weight 0.41 and 0.59 comes from Table IV of "Minimum risk weights for comparing treatments in stratified binomial trials"
  x <- gs_info_rd(p_c = p_c, p_e = p_e, n = n, rd0 = 0, ratio = 1, weight = "invar")

  res <- 1 / x$info0
  expected <- 0.41^2 * p_pool_stratum1 * (1 - p_pool_stratum1) *
    (1/n_c_stratum1 + 1/n_e_stratum1) +
    0.59^2 * p_pool_stratum2 * (1 - p_pool_stratum2) *
    (1/n_c_stratum2 + 1/n_e_stratum2)
  (all.equal(res, expected, tolerance = 4e-6, scale = 1))

  res <- 1 / x$info1
  expected <- 0.41^2 * p_c_stratum1 * (1 - p_c_stratum1)/n_c_stratum1 +
    0.41^2 * p_e_stratum1 * (1 - p_e_stratum1)/n_e_stratum1 +
    0.59^2 * p_c_stratum2 * (1 - p_c_stratum2)/n_c_stratum2 +
    0.59^2 * p_e_stratum2 * (1 - p_e_stratum2)/n_e_stratum2
  (all.equal(res, expected, tolerance = 0.0001))

  # Testing the SS weight via the aggregated info0, info1
  # the weight 0.63 and 0.37 comes from Table IV of "Minimum risk weights for comparing treatments in stratified binomial trials"
  x <- gs_info_rd(p_c = p_c, p_e = p_e, n = n, rd0 = 0, ratio = 1, weight = "ss")

  res <- 1 / x$info0
  expected <- 0.63^2 * p_pool_stratum1 * (1 - p_pool_stratum1) *
    (1/n_c_stratum1 + 1/n_e_stratum1) +
    0.37^2 * p_pool_stratum2 * (1 - p_pool_stratum2) *
    (1/n_c_stratum2 + 1/n_e_stratum2)
  (res %==% expected)

  res <- 1 / x$info1
  expected <- 0.63^2 * p_c_stratum1 * (1 - p_c_stratum1)/n_c_stratum1 +
    0.63^2 * p_e_stratum1 * (1 - p_e_stratum1)/n_e_stratum1 +
    0.37^2 * p_c_stratum2 * (1 - p_c_stratum2)/n_c_stratum2 +
    0.37^2 * p_e_stratum2 * (1 - p_e_stratum2)/n_e_stratum2
  (all.equal(res, expected, tolerance = 0.0001))

  # Testing the MR weight following formula (10)
  # the weight 0.47 and 0.53 comes from Table IV of "Minimum risk weights for comparing treatments in stratified binomial trials"
  x_mr <- gs_info_rd(p_c = p_c, p_e = p_e, n = n, rd0 = 0, ratio = 1, weight = "mr")
  V1 <- var_H1_stratum1
  V2 <- var_H1_stratum2
  delta1 <- p_e_stratum1 - p_c_stratum1
  delta2 <- p_e_stratum2 - p_c_stratum2
  f1 <- n_stratum1 /  (n_stratum1 + n_stratum2)
  f2 <- n_stratum2 /  (n_stratum1 + n_stratum2)

  w1 <- (V2+(delta1-delta2)^2*f1) / (V1 + V2 + (delta1 - delta2)^2)
  w2 <- 1 - w1
  (all.equal(c(w1, w2), c(0.47, 0.53), tolerance = 0.003, scale = 1))

  x <- gs_info_rd(p_c = p_c, p_e = p_e, n = n, rd0 = 0, ratio = 1, weight = "mr")

  res <- 1 / x$info0
  expected <- w1^2 * p_pool_stratum1 * (1 - p_pool_stratum1) *
    (1/n_c_stratum1 + 1/n_e_stratum1) +
    w2^2 * p_pool_stratum2 * (1 - p_pool_stratum2) *
    (1/n_c_stratum2 + 1/n_e_stratum2)
  (res %==% expected)

  res <- 1 / x$info1
  expected <- w1^2 * p_c_stratum1 * (1 - p_c_stratum1)/n_c_stratum1 +
    w1^2 * p_e_stratum1 * (1 - p_e_stratum1)/n_e_stratum1 +
    w2^2 * p_c_stratum2 * (1 - p_c_stratum2)/n_c_stratum2 +
    w2^2 * p_e_stratum2 * (1 - p_e_stratum2)/n_e_stratum2
  (all.equal(res, expected, tolerance = 0.0001))


})

assert("Unbalanced randomization ratio uses n_e / n_c = ratio", {
  p_c <- 0.2
  p_e <- 0.1
  n <- 300
  ratio <- 2

  n_c <- n / (1 + ratio)
  n_e <- n * ratio / (1 + ratio)
  p_pool <- (n_c * p_c + n_e * p_e) / n
  expected_info0 <- 1 / (p_pool * (1 - p_pool) * (1 / n_c + 1 / n_e))
  expected_info1 <- 1 / (p_c * (1 - p_c) / n_c + p_e * (1 - p_e) / n_e)

  x <- gs_info_rd(
    p_c = tibble::tibble(stratum = "All", rate = p_c),
    p_e = tibble::tibble(stratum = "All", rate = p_e),
    n = tibble::tibble(stratum = "All", n = n, analysis = 1),
    rd0 = 0,
    ratio = ratio
  )

  (all.equal(x$info0, expected_info0))
  (all.equal(x$info1, expected_info1))

  # Fixed sample size with unbalanced ratio matches gsDesign::nBinomial()
  n_gsd <- gsDesign::nBinomial(
    p1 = p_c, p2 = p_e, alpha = 0.025, beta = 0.1,
    delta0 = 0, ratio = ratio, sided = 1, outtype = 1, scale = "Difference"
  )
  d_gsd2 <- gs_design_rd(
    p_c = tibble::tibble(stratum = "All", rate = p_c),
    p_e = tibble::tibble(stratum = "All", rate = p_e),
    alpha = 0.025, beta = 0.1, rd0 = 0, ratio = ratio, info_frac = 1,
    upper = gs_b, lower = gs_b, upar = -qnorm(0.025), lpar = -Inf,
    h1_spending = FALSE
  )
  (all.equal(d_gsd2$analysis$n, n_gsd))
})

assert("Non-zero rd0 uses null rates p_c0 and p_e0 for info0", {
  p_c <- 0.2
  p_e <- 0.1
  n <- 300
  rd0 <- 0.05
  ratio <- 2
  d <- 1

  n_c <- n / (1 + ratio)
  n_e <- n * ratio / (1 + ratio)
  p_e0 <- (p_c + ratio * p_e - d * rd0) / (ratio + 1)
  p_c0 <- p_e0 + d * rd0
  expected_info0 <- 1 / (p_c0 * (1 - p_c0) / n_c + p_e0 * (1 - p_e0) / n_e)
  expected_info1 <- 1 / (p_c * (1 - p_c) / n_c + p_e * (1 - p_e) / n_e)

  x <- gs_info_rd(
    p_c = tibble::tibble(stratum = "All", rate = p_c),
    p_e = tibble::tibble(stratum = "All", rate = p_e),
    n = tibble::tibble(stratum = "All", n = n, analysis = 1),
    rd0 = rd0,
    ratio = ratio
  )

  (all.equal(x$info0, expected_info0))
  (all.equal(x$info1, expected_info1))
})
