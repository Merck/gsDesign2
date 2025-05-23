test_that("gs_bound_summary() summarizes the correct number of analyses", {

  x <- gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
  x_bound <- gs_bound_summary(x)
  expect_equal(nrow(x_bound), 3 * 5)

  x <- gs_design_ahr(info_frac = c(.25, 1), analysis_time = c(12, 36))
  x_bound <- gs_bound_summary(x)
  expect_equal(nrow(x_bound), 2 * 5)

  x <- gs_design_ahr(info_frac = 1, analysis_time = 36)
  x_bound <- gs_bound_summary(x)
  expect_equal(nrow(x_bound), 1 * 5)

})

test_that("gs_bound_summary() uses correct HR label", {

  x_ahr <- gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
  x_ahr_bound <- gs_bound_summary(x_ahr)
  expect_identical(x_ahr_bound$Value[5], "P(Cross) if AHR=0.81")

  x_wlr <- gs_design_wlr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
  x_wlr_bound <- gs_bound_summary(x_wlr)
  expect_identical(x_wlr_bound$Value[5], "P(Cross) if wAHR=0.81")

})

test_that("gs_bound_summary() uses correct column names", {
  # column names for single implicit alpha
  col_expected <- c("Analysis", "Value", "Efficacy", "Futility")
  x <- gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
  x_bound <- gs_bound_summary(x)
  expect_equal(colnames(x_bound), col_expected)

  # column names for multiple alpha values
  col_expected <- c("Analysis", "Value", "α=0.0125", "α=0.025", "α=0.05", "Futility")
  x <- gs_design_ahr(analysis_time = 1:3*12, alpha = 0.0125)
  x_bound <- gs_bound_summary(x, alpha = c(0.025, 0.05))
  expect_identical(colnames(x_bound), col_expected)
})

test_that("gs_bound_summary() supports multiple alpha values", {
  # gs_design_ahr()
  x_0125 <- gs_design_ahr(analysis_time = 1:3 * 12, alpha = 0.0125)
  x_0250 <- gs_update_ahr(x_0125, alpha = 0.0250)
  x_0500 <- gs_update_ahr(x_0125, alpha = 0.0500)

  x_0125_bound <- gs_bound_summary(x_0125)
  x_0250_bound <- gs_bound_summary(x_0250)
  x_0500_bound <- gs_bound_summary(x_0500)

  expected <- cbind(
    x_0125_bound[, c("Analysis", "Value")],
    `α=0.0125` = x_0125_bound[, "Efficacy"],
    `α=0.025`  = x_0250_bound[, "Efficacy"],
    `α=0.05`   = x_0500_bound[, "Efficacy"],
    x_0125_bound[, "Futility", drop = FALSE]
  )
  x_bound <- gs_bound_summary(x_0125, alpha = c(0.025, 0.05))
  expect_equal(x_bound, expected)

  # gs_power_ahr()
  x_0250 <- gs_power_ahr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1))
  expect_equal(x_0250[["input"]][["alpha"]], 0.0250)
  expect_equal(x_0250[["input"]][["upar"]][["total_spend"]], 0.0250)
  x_0125 <- gs_update_ahr(x_0250, alpha = 0.0125)
  x_0500 <- gs_update_ahr(x_0250, alpha = 0.0500)

  x_0125_bound <- gs_bound_summary(x_0125)
  x_0250_bound <- gs_bound_summary(x_0250)
  x_0500_bound <- gs_bound_summary(x_0500)

  expected <- cbind(
    x_0250_bound[, c("Analysis", "Value")],
    `α=0.0125` = x_0125_bound[, "Efficacy"],
    `α=0.025`  = x_0250_bound[, "Efficacy"],
    `α=0.05`   = x_0500_bound[, "Efficacy"],
    x_0250_bound[, "Futility", drop = FALSE]
  )
  x_bound <- gs_bound_summary(x_0250, alpha = c(0.0125, 0.05))
  expect_equal(x_bound, expected)
})

test_that("The arg `alpha` is only supported for AHR design objects", {
  x_wlr <- gs_design_wlr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))

  expect_error(
    gs_bound_summary(x_wlr, alpha = 0.5),
    "The argument `alpha` is only supported for AHR design objects"
  )
})

test_that("The arg `alpha` is required to be a numeric vector", {
  x <- gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))

  expect_error(
    gs_bound_summary(x, alpha = "alternative"),
    "The argument `alpha` must be a numeric vector"
  )
})

test_that("Edge case: when arg `alpha` matches original alpha", {
  # Redundantly specifying the original alpha does not affect results
  x <- gs_design_ahr(analysis_time = 1:3*12, alpha = 0.0125)

  x_bound <- gs_bound_summary(x, alpha = c(0.025, 0.05))
  x_bound_redundant <- gs_bound_summary(x, alpha = c(0.0125, 0.025, 0.05))
  expect_equal(x_bound_redundant, x_bound)

  # Only specifying the original alpha only affects the column name
  x_bound <- gs_bound_summary(x)
  x_bound_same <- gs_bound_summary(x, alpha = 0.0125)
  expect_equal(colnames(x_bound_same)[3], "α=0.0125")
  expect_equal(unname(x_bound_same), unname(x_bound))
})

test_that("One-sided design should not have column Futility", {
  x <- gs_design_ahr(info_frac = 1:3/3, lower = gs_b, lpar = rep(-Inf, 3))
  x_bound <- gs_bound_summary(x)

  expect_false("Futility" %in% colnames(x_bound))

  x_bound_alpha <- gs_bound_summary(x, alpha = c(0.025, 0.05))

  expect_false("Futility" %in% colnames(x_bound_alpha))
})

test_that("Arg `digits` controls number of digits in table body", {
  x <- gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
  x_bound_5 <- gs_bound_summary(x, digits = 5)
  x_bound_1 <- gs_bound_summary(x, digits = 1)

  efficacy_5 <- nchar(format(x_bound_5$Efficacy))
  efficacy_1 <- nchar(format(x_bound_1$Efficacy))
  expect_equal(unique(efficacy_5 - efficacy_1), 5 - 1)

  futility_5 <- nchar(format(x_bound_5$Futility))
  futility_1 <- nchar(format(x_bound_1$Futility))
  expect_equal(unique(futility_5 - futility_1), 5 - 1)
})

test_that("Arg `ddigits` controls number of digits for delta value", {
  x <- gs_design_ahr(info_frac = 1:3 / 3)

  x_bound_1 <- gs_bound_summary(x, ddigits = 1)
  expect_identical(
    x_bound_1$Value[c(5, 10, 15)],
    c("P(Cross) if AHR=0.8", "P(Cross) if AHR=0.7", "P(Cross) if AHR=0.7")
  )

  x_bound_2 <- gs_bound_summary(x, ddigits = 2)
  expect_identical(
    x_bound_2$Value[c(5, 10, 15)],
    c("P(Cross) if AHR=0.81", "P(Cross) if AHR=0.73", "P(Cross) if AHR=0.68")
  )

  x_bound_3 <- gs_bound_summary(x, ddigits = 3)
  expect_identical(
    x_bound_3$Value[c(5, 10, 15)],
    c("P(Cross) if AHR=0.806", "P(Cross) if AHR=0.729", "P(Cross) if AHR=0.683")
  )
})

test_that("Arg `tdigits` controls number of digits for estimated timing", {
  x <- gs_design_ahr(info_frac = 1:3 / 3)
  x_bound <- gs_bound_summary(x, tdigits = 2)

  expect_identical(
    x_bound$Analysis[c(4, 9, 14)],
    c("Month: 12.53", "Month: 21.29", "Month: 36.00")
  )
})

test_that("Arg `timename` controls time unit label", {
  x <- gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
  x_bound <- gs_bound_summary(x, timename = "Fortnight")

  expect_identical(
    x_bound$Analysis[c(4, 9, 14)],
    c("Fortnight: 12", "Fortnight: 25", "Fortnight: 36")
  )
})
