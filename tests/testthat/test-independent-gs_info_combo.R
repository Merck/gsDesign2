rho <- c(1, 1, 0, 0)
gamma <- c(0, 1, 0, 1)
tau <- c(-1, -1, -1, -1)
enroll_rate <- tibble::tibble(
  stratum = "All",
  duration = c(2, 2, 30),
  rate = c(3, 6, 9)
)
fail_rate <- tibble::tibble(
  stratum = "All",
  duration = c(3, 100),
  fail_rate = log(2) / c(9, 18),
  hr = c(.9, .6),
  dropout_rate = rep(.001, 2)
)
info_combo <- gsDesign2::gs_info_combo(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  ratio = 1, # Experimental:Control randomization ratio
  event = NULL, # Events at analyses
  analysis_time = 30, # Times of analyses
  rho = rho,
  gamma = gamma,
  tau = rep(-1, length(rho)),
  approx = "asymptotic"
)

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

  test_that("gs_info_combo correctly use gs_info_wlr 1", {
    expect_equal(info_combo$info[i], info_wlr$info[1])
  })
}
