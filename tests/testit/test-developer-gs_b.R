assert("Test that gs_b() returns intended values", {
  (all_equal(1:3, gs_b(1:3)))
  (all_equal(2, gs_b(1:3, k = 2)))
})
