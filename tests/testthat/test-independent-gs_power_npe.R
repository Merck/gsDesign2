test_that("expect equal with mvtnorm for efficacy and futility bounds", {
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

  test1 <- test %>% dplyr::filter(bound == "upper")
  test2 <- test %>% dplyr::filter(bound == "lower")

  alpha.t <- 0.025
  b.ia <- gsDesign::sfLDOF(alpha = alpha.t, t = r)
  alpha.ia <- b.ia$spend

  Pb <- function(alpha.t, alpha.ia, r, b) {
    temp <- cache_fun(mvtnorm::pmvnorm,
      lower = c(-Inf, b),
      upper = c(qnorm(1 - alpha.ia), Inf),
      corr = rbind(c(1, sqrt(r)), c(sqrt(r), 1))
    )
    return(alpha.t - alpha.ia - temp)
  }

  b <- uniroot(Pb, c(1.96, 4), alpha.t = alpha.t, alpha.ia = alpha.ia, r = r)

  pb <- 1 - pnorm(b$root)

  expect_equal(object = test1$z, expected = c(qnorm(1 - alpha.ia), b$root), tolerance = 0.001)
  expect_equal(object = test1$probability, expected = cumsum(c(b.ia$spend, pb)), tolerance = 0.001)

  beta.t <- 0.02
  a.ia <- gsDesign::sfLDOF(alpha = beta.t, t = r)
  beta.ia <- a.ia$spend

  Pa <- function(beta.t, beta.ia, r, a) {
    temp <- cache_fun(mvtnorm::pmvnorm,
      lower = c(-Inf, qnorm(beta.ia)),
      upper = c(a, Inf),
      corr = rbind(c(1, sqrt(r)), c(sqrt(r), 1))
    )
    return(beta.t - beta.ia - temp)
  }

  a <- uniroot(Pa, c(-4, 1.96), beta.t = beta.t, beta.ia = beta.ia, r = r)

  pa <- pnorm(a$root)

  expect_equal(object = test2$z, expected = c(qnorm(beta.ia), a$root), tolerance = 0.001)
  expect_equal(object = test2$probability, expected = cumsum(c(a.ia$spend, pa)), tolerance = 0.001)
})

test_that("expect equal with gsDesign::gsProbability outcome for efficacy bounds", {
  info <- c(40, 150, 200)

  test3 <-
    gs_power_npe(
      theta = .1,
      info = info, info0 = NULL, binding = FALSE,
      upper = gs_spending_bound,
      upar = list(sf = gsDesign::sfLDOF, param = NULL, total_spend = 0.025),
      lower = gs_b,
      lpar = rep(-Inf, 3)
    ) %>% dplyr::filter(bound == "upper")

  x3 <- gsDesign::gsProbability(
    k = 3,
    theta = .1,
    n.I = info,
    a = rep(-20, 3),
    b = gsDesign::gsDesign(k = 3, test.type = 1, sfu = gsDesign::sfLDOF, n.I = info)$upper$bound
  )


  expect_equal(ifelse(is.infinite(test3$z), 20, test3$z), x3$upper$bound, tolerance = 0.0001)
  expect_equal(test3$probability, cumsum(x3$upper$prob), tolerance = 0.0001)
})
