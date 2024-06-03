# Helper functions used by test-independent_as_rtf.R

fixed_design_ahr_example <- function() {
  fixed_design_ahr(
    alpha = 0.025, power = 1 - 0.1,
    enroll_rate = define_enroll_rate(
      duration = 18,
      rate = 20
    ),
    fail_rate = define_fail_rate(
      duration = c(4, 100),
      fail_rate = log(2) / 12,
      dropout_rate = 0.001,
      hr = c(1, 0.6)
    ),
    study_duration = 36,
    ratio = 1
  )
}

gs_power_wlr_example <- function(binding = FALSE) {
  gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1), binding = binding)
}
