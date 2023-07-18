test_that("gs_spending_bound() does not execute as expected", {
  expect_true(is.numeric(b <- gs_spending_bound()))
  expect_true(is.numeric(a <- gs_spending_bound(efficacy = FALSE)))
  hgm1_0 <- gsDesign2:::h1(theta = 0, info = 1, a = a, b = b)
  hgm1_1 <- gsDesign2:::h1(theta = .1, info = 1, a = a, b = b)
  expect_true(is.numeric(b2 <- gs_spending_bound(k = 2, theta = 0, hgm1 = hgm1_0)))
  expect_true(is.numeric(a2 <- gs_spending_bound(k = 2, theta = .1, hgm1 = hgm1_1, efficacy = FALSE)))
})
