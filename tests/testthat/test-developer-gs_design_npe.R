source_files <- list.files("./old_function/", "*.R$")
sapply(paste0("./old_function/", source_files), source)

library(dplyr)

test_that("verify by gs_power_npe", {
  beta <- 0.1
  # new version
  x <- gs_design_npe(
    theta = c(.1, .2, .3), info = (1:3) * 40, beta = 0.1,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL), binding = TRUE
  )
  # The power is 0.9. If we re-use these bounds under alternate hypothesis, then we can get a power close to 0.9.
  y <- gs_power_npe(
    theta = c(.1, .2, .3), info = (1:3) * 40,
    upper = gs_b, upar = (x %>% filter(bound == "upper"))$z,
    lower = gs_b, lpar = -(x %>% filter(bound == "upper"))$z,
    binding = TRUE # Always use binding = TRUE for power calculations
  )
  expect_equal(y$probability[y$analysis == 3 & y$bound == "upper"], 1 - beta, tolerance = 1e-2)
  # old version
  x <- gs_design_npe_(
    theta = c(.1, .2, .3), info = (1:3) * 40, beta = 0.1,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL), binding = TRUE
  )
  # The power is 0.9. If we re-use these bounds under alternate hypothesis, then we can get a power close to 0.9.
  y <- gs_power_npe_(
    theta = c(.1, .2, .3), info = (1:3) * 40,
    upper = gs_b, upar = (x %>% filter(Bound == "Upper"))$Z,
    lower = gs_b, lpar = -(x %>% filter(Bound == "Upper"))$Z,
    binding = TRUE # Always use binding = TRUE for power calculations
  )
  expect_equal(y$Probability[y$Analysis == 3 & y$Bound == "Upper"], 1 - beta, tolerance = 1e-2)
})

test_that("examples in spec - Lachin book p71", {
  pc <- .28 # Control response rate
  pe <- .40 # Experimental response rate
  p0 <- (pc + pe) / 2 # Ave response rate under H0
  # Information per increment of 1 in sample size
  info0 <- 1 / (p0 * (1 - p0) * 4)
  info <- 1 / (pc * (1 - pc) * 2 + pe * (1 - pe) * 2)
  # Result should round up to next even number = 652
  # Divide information needed under H1 by information per patient added
  x1_a <- gs_design_npe(theta = pe - pc, info = info, info0 = info0, info_scale = "h0_info") %>%
    select(-c(info_frac, probability0, info1))
  x1_b <- gs_design_npe(theta = pe - pc, info = info, info0 = info0, info_scale = "h1_info") %>%
    select(-c(info_frac, probability0, info1))
  x1_c <- gs_design_npe(theta = pe - pc, info = info, info0 = info0, info_scale = "h0_h1_info") %>%
    select(-c(info_frac, probability0, info1))
  x2 <- gs_design_npe_(theta = pe - pc, info = info, info0 = info0) %>%
    rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) %>%
    mutate(bound = tolower(bound))
  expect_equal(x1_c, x2)
})

test_that("fixed design with 3 equal info", {
  x1_a <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info_scale = "h0_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) %>%
    select(-c(info_frac, probability0, info1)) %>%
    arrange(analysis, bound)
  x1_b <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info_scale = "h1_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) %>%
    select(-c(info_frac, probability0, info1)) %>%
    arrange(analysis, bound)
  x1_c <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info_scale = "h0_h1_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) %>%
    select(-c(info_frac, probability0, info1)) %>%
    arrange(analysis, bound)
  x2 <- gs_design_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 80,
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) %>%
    rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) %>%
    mutate(bound = tolower(bound)) %>%
    select(-c(theta1, info1)) %>%
    arrange(analysis, bound)
  expect_equal(x1_c, x2)
})

test_that("fixed design with 3 unequal info", {
  x1_a <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info0 = (1:3) * 90 + 10, info1 = (1:3) * 70 - 5, info_scale = "h0_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) %>%
    select(-c(info_frac, probability0, info1)) %>%
    arrange(analysis, bound)
  x1_b <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info0 = (1:3) * 90 + 10, info1 = (1:3) * 70 - 5, info_scale = "h1_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) %>%
    select(-c(info_frac, probability0, info1)) %>%
    arrange(analysis, bound)
  x1_c <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info0 = (1:3) * 90 + 10, info1 = (1:3) * 70 - 5, info_scale = "h0_h1_info",
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) %>%
    select(-c(info_frac, probability0, info1)) %>%
    arrange(analysis, bound)
  x2 <- gs_design_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 80, info0 = (1:3) * 90 + 10, info1 = (1:3) * 70 - 5,
    upper = gs_b, upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
    lower = gs_b, lpar = c(-1, 0, 0)
  ) %>%
    rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) %>%
    mutate(bound = tolower(bound)) %>%
    select(-c(theta1, info1)) %>%
    arrange(analysis, bound)
  expect_equal(x1_c, x2)
})

test_that("futility at IA1; efficacy only at IA2 +FA", {
  x1_a <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info0 = (1:3) * 40, info_scale = "h0_info",
    upper = gs_spending_bound, upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_b, lpar = c(-1, -Inf, -Inf),
    test_upper = c(FALSE, TRUE, TRUE)
  ) %>%
    select(-c(info_frac, probability0, info1)) %>%
    arrange(analysis, bound)
  x1_b <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info0 = (1:3) * 40, info_scale = "h1_info",
    upper = gs_spending_bound, upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_b, lpar = c(-1, -Inf, -Inf),
    test_upper = c(FALSE, TRUE, TRUE)
  ) %>%
    select(-c(info_frac, probability0, info1)) %>%
    arrange(analysis, bound)
  x1_c <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info0 = (1:3) * 40, info_scale = "h0_h1_info",
    upper = gs_spending_bound, upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_b, lpar = c(-1, -Inf, -Inf),
    test_upper = c(FALSE, TRUE, TRUE)
  ) %>%
    select(-c(info_frac, probability0, info1)) %>%
    arrange(analysis, bound)
  x2 <- gs_design_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info0 = (1:3) * 40,
    upper = gs_spending_bound, upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_b, lpar = c(-1, -Inf, -Inf),
    test_upper = c(FALSE, TRUE, TRUE)
  ) %>%
    rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) %>%
    mutate(bound = tolower(bound)) %>%
    select(-c(theta1, info1)) %>%
    arrange(analysis, bound)
  expect_equal(x1_c, x2)
})

test_that("spending bounds", {
  x1_a <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info0 = (1:3) * 50, info_scale = "h0_info",
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
  ) %>%
    select(analysis, bound, z, probability, theta, info, info0, info1) %>%
    arrange(analysis, bound)
  x1_b <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info0 = (1:3) * 50, info_scale = "h1_info",
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
  ) %>%
    select(analysis, bound, z, probability, theta, info, info0, info1) %>%
    arrange(analysis, bound)
  x1_c <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info0 = (1:3) * 50, info_scale = "h0_h1_info",
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
  ) %>%
    select(analysis, bound, z, probability, theta, info, info0, info1) %>%
    arrange(analysis, bound)
  x2 <- gs_design_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info0 = (1:3) * 50,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
  ) %>%
    rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) %>%
    mutate(bound = tolower(bound)) %>%
    select(analysis, bound, z, probability, theta, info, info0, info1) %>%
    arrange(analysis, bound)
  expect_equal(x1_c, x2)
})

test_that("2-sided symmetric spend", {
  x1_a <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info_scale = "h0_info",
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  ) %>%
    select(analysis, bound, z, probability, theta, info, info0, info1) %>%
    arrange(analysis, bound)
  x1_b <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info_scale = "h1_info",
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  ) %>%
    select(analysis, bound, z, probability, theta, info, info0, info1) %>%
    arrange(analysis, bound)
  x1_c <- gs_design_npe(
    theta = c(.1, .2, .3),
    info = (1:3) * 40, info_scale = "h0_h1_info",
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  ) %>%
    select(analysis, bound, z, probability, theta, info, info0, info1) %>%
    arrange(analysis, bound)
  x2 <- gs_design_npe_(
    theta = c(.1, .2, .3),
    info = (1:3) * 40,
    binding = TRUE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
  ) %>%
    rename(analysis = Analysis, bound = Bound, z = Z, probability = Probability) %>%
    mutate(bound = tolower(bound)) %>%
    select(analysis, bound, z, probability, theta, info, info0, info1) %>%
    arrange(analysis, bound)
  expect_equal(x1_c, x2)
})
