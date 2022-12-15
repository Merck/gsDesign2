utils::globalVariables(
  unique(
    c(
      # AHR()
      c("stratum", "rate", "hr", "Treatment", "Events", "failRate", "HR", "Time", "info0", "info"),
      # as_gt.gs_design()
      c("Bound", "Alternate hypothesis", "Null hypothesis", "Analysis"),
      # expected_accrual()
      c("stratum", "rate", "duration"),
      # expected_event()
      c("startEnroll", "endFail", "endEnroll", "duration", "failRate", "dropoutRate", "enrollRate", "g", "Q", "G", "d", "nbar", "Events"),
      # fixed_design()
      c("Bound", "rate", "rho"),
      # fixed_design_size_rmst()
      c("rate"),
      # gs_design_ahr()
      c("Time", "Events", "info", "info0", "Z", "theta", "Analysis", "Bound", "N", "rate"),
      # gs_design_combo()
      c("Bound", "Analysis", "Time", "N", "Events", "Bound", "Z", "Probability", "Probability_Null", "Probability0", "Nominal p", "rho", "tau", "test", "event_frac", "rate", "duration"),
      # gs_design_npe()
      c("Analysis", "Bound", "Probability", "Z", "Probability0", "info_frac"),
      # gs_design_rd()
      c("prevalence", "Z", "info", "theta", "rd", "info0", "Bound", "Analysis", "N", "Probability", "Probability0", "info_frac0", "~Risk difference at bound", "Nominal p"),
      # gs_design_wlr()
      c("IF", "Time", "Events", "info", "info0", "theta", "Analysis", "Bound", "Z", "N", "rate"),
      # gs_info_ahr()
      c("Analysis", "Time", "Events", "theta", "info", "info0"),
      # gs_info_rd()
      c("rate", "N_c", "N_e", "d", "p_e0", "p_pool_per_k_per_s", "p_c0", "analysis", "sum_ss", "sigma2_H0_per_k_per_s", "sum_inv_var_per_s", "weight_per_k_per_s", "rd", "sigma2_H1", "sigma2_H0", "theta1", "theta0", "info1", "info0"),
      # gs_power_ahr()
      c("Z", "info", "Analysis", "Bound", "Probability", "Probability0", "~HR at bound", "Nominal p", "Time", "Events", "info_frac", "theta", "N", "info0", "info_frac0"),
      # gs_power_combo()
      c("Analysis", "Time", "N", "Events", "Bound", "Z", "Probability", "Probability_Null", "Nominal p", "rho", "tau", "test", "event_frac", "rate", "duration"),
      # gs_power_npe()
      c("Bound", "Analysis"),
      # gs_power_rd()
      c("N", "Z", "info", "theta", "Analysis", "Bound", "Probability", "Probability0", "~Risk difference at bound", "Nominal p", "rd", "theta1", "theta0", "info_frac", "info0", "info_frac0"),
      # gs_power_wlr()
      c("Analysis", "Bound", "Z", "Probability", "Events", "Probability0", "~HR at bound", "Nominal p", "Time", "info", "info_frac", "theta", "N", "info0", "info_frac0"),
      # ppwe()
      c("h", "duration", "H"),
      # s2pwe()
      c("Times", "Survival", "H", "duration", "rate"),
      # summary.gs_design()
      c("Analysis", "info_frac", "event_frac", "Probability", "Probability0", "Bound", "rd", "Z", "~HR at bound", "Nominal p", "Alternate hypothesis", "Null hypothesis", "~wHR at bound", "~Risk difference at bound"),
      # to_integer.fixed_design()
      c("rate", "Bound"),
      # to_integer.gs_design()
      c("rate")
    )
  )
)
