assert("The default of `gs_power_npe` is a single analysis with type I error controlled.", {
  x1 <- gs_power_npe(theta = 0) |>
    dplyr::filter(bound == "upper") |>
    dplyr::select(-info_frac)
  x2 <- gs_power_npe_(theta = 0) |>
    dplyr::filter(Bound == "Upper") |>
    dplyr::rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) |>
    dplyr::mutate(bound = tolower(bound))
  (x1 %==% as.data.frame(x2))
})

assert("fixed bound", {
  x1 <- gs_power_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    upper = gs_b,
    upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b,
    lpar = c(-1, 0, 0)
  ) |>
    dplyr::select(-info_frac)
  x2 <- gs_power_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    upper = gs_b,
    upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b,
    lpar = c(-1, 0, 0)
  ) |>
    dplyr::rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) |>
    dplyr::mutate(bound = tolower(bound))
  (x1 %==% as.data.frame(x2))
})

assert("Same fixed efficacy bounds, no futility bound (i.e., non-binding bound), null hypothesis", {
  x1 <- gs_power_npe(
    theta = rep(0, 3),
    info = (1:3) * 40,
    upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lpar = rep(-Inf, 3)
  ) |>
    dplyr::select(-info_frac)
  x2 <- gs_power_npe_(
    theta = rep(0, 3),
    info = (1:3) * 40,
    upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lpar = rep(-Inf, 3)
  ) |>
    dplyr::rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) |>
    dplyr::mutate(bound = tolower(bound))
  (x1 %==% as.data.frame(x2))
})

assert("Fixed bound with futility only at analysis 1; efficacy only at analyses 2, 3", {
  x1 <- gs_power_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    upper = gs_b,
    upar = c(Inf, 3, 2),
    lower = gs_b,
    lpar = c(qnorm(.1), -Inf, -Inf)
  ) |>
    dplyr::select(-info_frac)
  x2 <- gs_power_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    upper = gs_b,
    upar = c(Inf, 3, 2),
    lower = gs_b,
    lpar = c(qnorm(.1), -Inf, -Inf)
  ) |>
    dplyr::rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) |>
    dplyr::mutate(bound = tolower(bound))
  (x1 %==% as.data.frame(x2))
})

assert("Spending function bounds - Lower spending based on non-zero effect", {
  x1 <- gs_power_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
  ) |>
    dplyr::select(-info_frac)
  x2 <- gs_power_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
  ) |>
    dplyr::rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) |>
    dplyr::mutate(bound = tolower(bound))
  (x1 %==% as.data.frame(x2))
})

assert("Same bounds, but power under different theta", {
  x1 <- gs_power_npe(
    theta = c(.15, .25, .35),
    info = (1:3) * 40,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
  ) |>
    dplyr::select(-info_frac)
  x2 <- gs_power_npe_(
    theta = c(.15, .25, .35),
    info = (1:3) * 40,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
  ) |>
    dplyr::rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) |>
    dplyr::mutate(bound = tolower(bound))
  (x1 %==% as.data.frame(x2))
})

assert("Two-sided symmetric spend, O'Brien-Fleming spending", {
  x1 <- gs_power_npe(
    theta = rep(0, 3),
    info = (1:3) * 40,
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  ) |>
    dplyr::select(-info_frac)
  x2 <- gs_power_npe_(
    theta = rep(0, 3),
    info = (1:3) * 40,
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  ) |>
    dplyr::rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) |>
    dplyr::mutate(bound = tolower(bound))
  (x1 %==% as.data.frame(x2))
})

assert("Re-use these bounds under alternate hypothesis - Always use binding = TRUE for power calculations", {
  x <- gs_power_npe(
    theta = rep(0, 3),
    info = (1:3) * 40,
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  ) |>
    dplyr::select(-info_frac)
  x1 <- gs_power_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    binding = TRUE,
    upar = (x |> dplyr::filter(bound == "upper"))$z,
    lpar = -(x |> dplyr::filter(bound == "upper"))$z
  ) |>
    dplyr::select(-info_frac)
  x2 <- gs_power_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    binding = TRUE,
    upar = (x |> dplyr::filter(bound == "upper"))$z,
    lpar = -(x |> dplyr::filter(bound == "upper"))$z
  ) |>
    dplyr::rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) |>
    dplyr::mutate(bound = tolower(bound))
  (x1 %==% as.data.frame(x2))
})

assert("info != info0 != info1 - If one inputs info in upar", {
  x1_a <- gs_power_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info0 = (1:3) * 90 + 10, info1 = (1:3) * 70 - 5, info_scale = "h0_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) |>
    dplyr::select(-info_frac)
  x1_b <- gs_power_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info0 = (1:3) * 90 + 10, info1 = (1:3) * 70 - 5, info_scale = "h1_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) |>
    dplyr::select(-info_frac)
  x1_c <- gs_power_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info0 = (1:3) * 90 + 10, info1 = (1:3) * 70 - 5, info_scale = "h0_h1_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) |>
    dplyr::select(-info_frac)
  x2 <- gs_power_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info0 = (1:3) * 90 + 10, info1 = (1:3) * 70 - 5,
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) |>
    dplyr::rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) |>
    dplyr::mutate(bound = tolower(bound))
  (x1_c %==% as.data.frame(x2))
})

assert("Developer Tests 1-sided test", {
  r <- 80
  x <- gs_power_npe(
    theta = 0,
    info = (1:3) * 400,
    binding = FALSE, r = r,
    upper = gs_b, # gs_spending_bound,
    upar = gsDesign::gsDesign(k = 3, test.type = 1, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b,
    lpar = rep(-Inf, 3)
  ) |>
    dplyr::select(-info_frac)
  y <- gs_power_npe_(
    theta = 0,
    info = (1:3) * 400,
    binding = FALSE, r = r,
    upper = gs_b, # gs_spending_bound,
    upar = gsDesign::gsDesign(k = 3, test.type = 1, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b,
    lpar = rep(-Inf, 3)
  ) |>
    dplyr::rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) |>
    dplyr::mutate(bound = tolower(bound))
  z <- gsDesign::gsProbability(
    k = 3,
    theta = 0,
    n.I = (1:3) * 400,
    b = gsDesign::gsDesign(k = 3, test.type = 1, sfu = gsDesign::sfLDOF)$upper$bound, a = rep(-20, 3), r = r
  )
  (x %==% as.data.frame(y))
  (x$z[x$bound == "upper"] %==% z$upper$bound)
  (all.equal(x$probability[x$bound == "upper"], cumsum(z$upper$prob), tolerance = 1e-5))
})

assert("Independent Tests - Expect equal with mvtnorm for efficacy and futility bounds", {
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
  test1 <- test |> dplyr::filter(bound == "upper")
  test2 <- test |> dplyr::filter(bound == "lower")
  alpha_t <- 0.025
  b_ia <- gsDesign::sfLDOF(alpha = alpha_t, t = r)
  alpha_ia <- b_ia$spend
  prob_b <- function(alpha_t, alpha_ia, r, b) {
    temp <- cache_fun(mvtnorm::pmvnorm,
      lower = c(-Inf, b),
      upper = c(qnorm(1 - alpha_ia), Inf),
      corr = rbind(c(1, sqrt(r)), c(sqrt(r), 1))
    )
    return(alpha_t - alpha_ia - temp)
  }
  b <- uniroot(prob_b, c(1.96, 4), alpha_t = alpha_t, alpha_ia = alpha_ia, r = r)
  pb <- 1 - pnorm(b$root)
  (all.equal(test1$z, c(qnorm(1 - alpha_ia), b$root), tolerance = 0.001))
  (all.equal(test1$probability, cumsum(c(b_ia$spend, pb)), tolerance = 0.001, scale = 1))
  beta_t <- 0.02
  a_ia <- gsDesign::sfLDOF(alpha = beta_t, t = r)
  beta_ia <- a_ia$spend
  prob_a <- function(beta_t, beta_ia, r, a) {
    temp <- cache_fun(mvtnorm::pmvnorm,
      lower = c(-Inf, qnorm(beta_ia)),
      upper = c(a, Inf),
      corr = rbind(c(1, sqrt(r)), c(sqrt(r), 1))
    )
    return(beta_t - beta_ia - temp)
  }
  a <- uniroot(prob_a, c(-4, 1.96), beta_t = beta_t, beta_ia = beta_ia, r = r)
  pa <- pnorm(a$root)
  (all.equal(test2$z, c(qnorm(beta_ia), a$root), tolerance = 0.001))
  (all.equal(test2$probability, cumsum(c(a_ia$spend, pa)), tolerance = 0.001, scale = 1))
})

assert("Expect equal with gsDesign::gsProbability outcome for efficacy bounds", {
  info <- c(40, 150, 200)
  x <- gs_power_npe(
    theta = .1,
    info = info, binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, param = NULL, total_spend = 0.025),
    lower = gs_b,
    lpar = rep(-Inf, 3)
  ) |>
    dplyr::filter(bound == "upper")
  y <- gs_power_npe(
    theta = .1,
    info = info, binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, param = NULL, total_spend = 0.025),
    lower = gs_b,
    lpar = rep(-Inf, 3)
  ) |>
    dplyr::filter(bound == "upper")
  z <- gsDesign::gsProbability(
    k = 3, theta = .1,
    n.I = info,
    a = rep(-20, 3),
    b = gsDesign::gsDesign(k = 3, test.type = 1, sfu = gsDesign::sfLDOF, n.I = info)$upper$bound
  )
  (x %==% y)
  (all.equal(x$z[x$bound == "upper"], z$upper$bound, tolerance = 1e-5))
  (all.equal(x$probability[x$bound == "upper"], cumsum(z$upper$prob), tolerance = 1e-5))
})
