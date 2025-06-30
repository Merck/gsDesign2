test_that("Validate 2-sided symetric design", {
  x <- gs_power_wlr(enroll_rate = define_enroll_rate(duration = 12, rate = 50),
                    analysis_time = NULL, event = c(100, 200, 300),
                    upper = gs_spending_bound,
                    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
                    lower = gs_spending_bound,
                    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
                    binding = TRUE, h1_spending = FALSE)

  expect_equal(x$bound$z[x$bound$bound == "upper"],
               -x$bound$z[x$bound$bound == "lower"])

})
