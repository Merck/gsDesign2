test_that("Test that gs_b() returns intended values", {
  expect_equal(1:3, gs_b(1:3))
  expect_equal(2, gs_b(1:3, k = 2))
})