test_that("Testing weights calculation", {

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

  expect_equal(1/x$info0,
               0.41^2 * p_pool_stratum1 * (1 - p_pool_stratum1) * (1 / n_c_stratum1 + 1 / n_e_stratum1) +
                 0.59^2 * p_pool_stratum2 * (1 - p_pool_stratum2) * (1 / n_c_stratum2 + 1 / n_e_stratum2),
               tolerance = 1e-4)

  expect_equal(1/x$info1,
               0.41^2 * p_c_stratum1 * (1 - p_c_stratum1) / n_c_stratum1 + 0.41^2 * p_e_stratum1 * (1 - p_e_stratum1) / n_e_stratum1 +
                 0.59^2 * p_c_stratum2 * (1 - p_c_stratum2) / n_c_stratum2 + 0.59^2 * p_e_stratum2 * (1 - p_e_stratum2) / n_e_stratum2,
               tolerance = 1e-4)

  # Testing the SS weight via the aggregated info0, info1
  # the weight 0.63 and 0.37 comes from Table IV of "Minimum risk weights for comparing treatments in stratified binomial trials"
  x <- gs_info_rd(p_c = p_c, p_e = p_e, n = n, rd0 = 0, ratio = 1, weight = "ss")

  expect_equal(1/x$info0,
               0.63^2 * p_pool_stratum1 * (1 - p_pool_stratum1) * (1 / n_c_stratum1 + 1 / n_e_stratum1) +
                 0.37^2 * p_pool_stratum2 * (1 - p_pool_stratum2) * (1 / n_c_stratum2 + 1 / n_e_stratum2),
               tolerance = 1e-4)

  expect_equal(1/x$info1,
               0.63^2 * p_c_stratum1 * (1 - p_c_stratum1) / n_c_stratum1 + 0.63^2 * p_e_stratum1 * (1 - p_e_stratum1) / n_e_stratum1 +
                 0.37^2 * p_c_stratum2 * (1 - p_c_stratum2) / n_c_stratum2 + 0.37^2 * p_e_stratum2 * (1 - p_e_stratum2) / n_e_stratum2,
               tolerance = 1e-4)

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
  expect_equal(c(w1, w2), c(0.47, 0.53), tolerance = 5e-3)

  x <- gs_info_rd(p_c = p_c, p_e = p_e, n = n, rd0 = 0, ratio = 1, weight = "mr")

  expect_equal(1/x$info0,
               w1^2 * p_pool_stratum1 * (1 - p_pool_stratum1) * (1 / n_c_stratum1 + 1 / n_e_stratum1) +
                 w2^2 * p_pool_stratum2 * (1 - p_pool_stratum2) * (1 / n_c_stratum2 + 1 / n_e_stratum2),
               tolerance = 1e-4)

  expect_equal(1/x$info1,
               w1^2 * p_c_stratum1 * (1 - p_c_stratum1) / n_c_stratum1 + w1^2 * p_e_stratum1 * (1 - p_e_stratum1) / n_e_stratum1 +
                 w2^2 * p_c_stratum2 * (1 - p_c_stratum2) / n_c_stratum2 + w2^2 * p_e_stratum2 * (1 - p_e_stratum2) / n_e_stratum2,
               tolerance = 1e-4)


})

