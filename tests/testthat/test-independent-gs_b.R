test_that("gs_b() returns values as expected", {
  IF <- c(.6, .8, 1)
  par = gsDesign::gsDesign(alpha = .02, k = length(IF), test.type = 1, sfu = gsDesign::sfLDOF, timing = IF)$upper$bound
  expect_equal(par, gs_b(par))
  
  par = 1:10
  k = 5
  expect_equal(par[5], gs_b(par,k = k))
})

testthat::test_that("gs_b() returns NA if the number of interim analysis is larger than the length of par", {
  IF <- c(.8, 1)
  par = gsDesign::gsDesign(alpha = .025, k = length(IF), test.type = 1, sfu = gsDesign::sfLDOF, timing = IF)$upper$bound
  expect_true(is.na(gs_b(par,k = 3)))
})