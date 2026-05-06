assert("Test that gs_b() returns intended values", {
  (isTRUE(all.equal(1:3, gs_b(1:3))))
  (isTRUE(all.equal(2, gs_b(1:3, k = 2))))
})
