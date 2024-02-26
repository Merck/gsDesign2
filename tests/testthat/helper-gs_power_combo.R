# Helper functions used by test-independent-gs_power_combo.R

test_gs_power_combo <- function() {
  enroll_rate <- define_enroll_rate(duration = 12, rate = 500 / 12)

  fail_rate <- define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 15, # Median survival 15 month
    dropout_rate = 0.001,
    hr = c(1, .6)
  )

  fh_test <- rbind(
    data.frame(
      rho = 0,
      gamma = 0,
      tau = -1,
      test = 1,
      analysis = 1:3,
      analysis_time = c(12, 24, 36)
    ),
    data.frame(
      rho = c(0, 0.5),
      gamma = 0.5,
      tau = -1,
      test = 2:3,
      analysis = 3,
      analysis_time = 36
    )
  )

  # User-defined bound
  gs_power_combo_test1 <- gsDesign2::gs_power_combo(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    fh_test = fh_test,
    upper = gs_b, upar = c(3, 2, 1),
    lower = gs_b, lpar = c(-1, 0, 1)
  )

  # Minimal Information Fraction derived bound
  gs_power_combo_test2 <- gsDesign2::gs_power_combo(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    fh_test,
    upper = gs_spending_combo,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_combo,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2)
  )

  list(
    "enroll_rate" = enroll_rate,
    "fail_rate" = fail_rate,
    "fh_test" = fh_test,
    "gs_power_combo_test1" = gs_power_combo_test1,
    "gs_power_combo_test2" = gs_power_combo_test2
  )
}
