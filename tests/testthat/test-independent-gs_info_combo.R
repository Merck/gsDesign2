test_that("gs_info_combo correctly use gs_info_wlr 1", {
  res <- test_gs_info_combo()
  rho <- res$rho
  gamma <- res$gamma
  tau <- res$tau
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  info_combo <- res$info_combo

  for (i in 1:4) {
    weight_test_i <- gsDesign2:::get_combo_weight(rho[i], gamma[i], tau[i])
    info_wlr <- gsDesign2::gs_info_wlr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      ratio = 1, # Experimental:Control randomization ratio
      event = NULL, # Events at analyses
      analysis_time = 30, # Times of analyses
      weight = eval(parse(text = weight_test_i)),
      approx = "asymptotic"
    )
    expect_equal(info_combo$info[i], info_wlr$info[1])
  }
})
