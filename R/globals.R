#  Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
#  All rights reserved.
#
#  This file is part of the gsDesign2 program.
#
#  gsDesign2 is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

utils::globalVariables(
  unique(
    c(
      # From `AHR()`
      c(
        "stratum", "rate", "hr", "Treatment", "Events", "failRate", "HR",
        "Time", "info0", "info"
      ),
      # From `as_gt.gs_design()`
      c("Bound", "Alternate hypothesis", "Null hypothesis", "Analysis"),
      # From `expected_accrual()`
      c("stratum", "rate", "duration"),
      # From `expected_event()`
      c(
        "startEnroll", "endFail", "endEnroll", "duration", "failRate",
        "dropoutRate", "enrollRate", "g", "Q", "G", "d", "nbar", "Events"
      ),
      # From `fixed_design()`
      c("Bound", "rate", "rho"),
      # From `fixed_design_size_rmst()`
      c("rate"),
      # From `gs_design_ahr()`
      c(
        "Time", "Events", "info", "info0", "Z", "theta", "Analysis",
        "Bound", "N", "rate"
      ),
      # From `gs_design_combo()`
      c(
        "Bound", "Analysis", "Time", "N", "Events", "Bound", "Z",
        "Probability", "Probability_Null", "Probability0", "Nominal p",
        "rho", "tau", "test", "event_frac", "rate", "duration"
      ),
      # From `gs_design_npe()`
      c("Analysis", "Bound", "Probability", "Z", "Probability0", "info_frac"),
      # From `gs_design_rd()`
      c(
        "prevalence", "Z", "info", "theta", "rd", "info0", "Bound",
        "Analysis", "N", "Probability", "Probability0", "info_frac0",
        "~Risk difference at bound", "Nominal p"
      ),
      # From `gs_design_wlr()`
      c(
        "IF", "Time", "Events", "info", "info0", "theta", "Analysis",
        "Bound", "Z", "N", "rate"
      ),
      # From `gs_info_ahr()`
      c("Analysis", "Time", "Events", "theta", "info", "info0"),
      # From `gs_info_rd()`
      c(
        "rate", "N_c", "N_e", "d", "p_e0", "p_pool_per_k_per_s", "p_c0",
        "analysis", "sum_ss", "sigma2_H0_per_k_per_s", "sum_inv_var_per_s",
        "weight_per_k_per_s", "rd", "sigma2_H1", "sigma2_H0", "theta1",
        "theta0", "info1", "info0"
      ),
      # From `gs_power_ahr()`
      c(
        "Z", "info", "Analysis", "Bound", "Probability", "Probability0",
        "~HR at bound", "Nominal p", "Time", "Events", "info_frac",
        "theta", "N", "info0", "info_frac0"
      ),
      # From `gs_power_combo()`
      c(
        "Analysis", "Time", "N", "Events", "Bound", "Z", "Probability",
        "Probability_Null", "Nominal p", "rho", "tau", "test",
        "event_frac", "rate", "duration"
      ),
      # From `gs_power_npe()`
      c("Bound", "Analysis"),
      # From `gs_power_rd()`
      c(
        "N", "Z", "info", "theta", "Analysis", "Bound", "Probability",
        "Probability0", "~Risk difference at bound", "Nominal p", "rd",
        "theta1", "theta0", "info_frac", "info0", "info_frac0"
      ),
      # From `gs_power_wlr()`
      c(
        "Analysis", "Bound", "Z", "Probability", "Events", "Probability0",
        "~HR at bound", "Nominal p", "Time", "info", "info_frac", "theta",
        "N", "info0", "info_frac0"
      ),
      # From `ppwe()`
      c("h", "duration", "H"),
      # From `s2pwe()`
      c("Times", "Survival", "H", "duration", "rate"),
      # From `summary.gs_design()`
      c(
        "Analysis", "info_frac", "event_frac", "Probability", "Probability0",
        "Bound", "rd", "Z", "~HR at bound", "Nominal p", "Alternate hypothesis",
        "Null hypothesis", "~wHR at bound", "~Risk difference at bound"
      ),
      # From `to_integer.fixed_design()`
      c("rate", "Bound"),
      # From `to_integer.gs_design()`
      c("rate")
    )
  )
)
