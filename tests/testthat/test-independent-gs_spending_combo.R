test_that("gs_spending_combo spend output identical as gsDesign spending", {
  IF <- c(.6, .8, 1)
  alpha <- 0.02
  par_test <- gsDesign::gsDesign(
    alpha = alpha,
    k = length(IF),
    test.type = 1,
    sfu = gsDesign::sfLDOF, timing = IF
  )$upper$spend
  spend_test <- cumsum(par_test)

  par <- list(sf = gsDesign::sfLDOF, total_spend = alpha)
  spend_combo <- gsDesign2::gs_spending_combo(par, info = IF)

  expect_equal(spend_test, spend_combo)
})
