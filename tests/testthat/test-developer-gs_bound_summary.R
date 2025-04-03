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
  expect_identical(x_ahr_bound$Value[5], "P(Cross) if AHR=0.8")

  x_wlr <- gs_design_wlr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
  x_wlr_bound <- gs_bound_summary(x_wlr)
  expect_identical(x_wlr_bound$Value[5], "P(Cross) if wAHR=0.8")

})
