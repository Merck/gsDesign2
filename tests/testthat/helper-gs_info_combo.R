# Helper functions used by test-independent-gs_info_combo.R

test_gs_info_combo <- function() {
  rho <- c(1, 1, 0, 0)
  gamma <- c(0, 1, 0, 1)
  tau <- c(-1, -1, -1, -1)
  enroll_rate <- define_enroll_rate(
    duration = c(2, 2, 30),
    rate = c(3, 6, 9)
  )
  fail_rate <- define_fail_rate(
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    dropout_rate = rep(.001, 2),
    hr = c(.9, .6)
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

  list(
    "rho" = rho,
    "gamma" = gamma,
    "tau" = tau,
    "enroll_rate" = enroll_rate,
    "fail_rate" = fail_rate,
    "info_combo" = info_combo
  )
}
