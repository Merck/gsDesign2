assert("Test that gs_b() returns intended values", {
  (1:3 %==% gs_b(1:3))
  (2L %==% gs_b(1:3, k = 2))
})
