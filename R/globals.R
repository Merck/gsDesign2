#  Copyright (c) 2025 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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
      # From data.table expressions
      ".", ".SD",
      # From `ahr()`
      c("stratum", "rate", "hr", "treatment", "time", "info0", "info"),
      # From `ahr_blinded()`
      c("status"),
      # From `as_gt.gs_design()`
      c("Bound", "Alternate hypothesis", "Null hypothesis", "Analysis"),
      # From `expected_accrual()`
      c("stratum", "rate", "duration"),
      # From `expected_event()`
      c(
        "start_enroll", "end_fail", "end_enroll", "duration", "fail_rate_var",
        "dropout_rate_var", "enroll_rate_var", "g", "big_q", "big_g", "d", "nbar", "event"
      ),
      # From `fixed_design()`
      c("dropout_rate", "bound", "rate", "rho"),
      # From `fixed_design_size_rmst()`
      c("rate"),
      # From `gs_design_ahr()`
      c("time", "event", "info", "info0", "z", "theta", "n", "rate"),
      # From `gs_design_combo()`
      c(
        "time", "n", "event", "z", "probability", "probability_null",
        "probability0", "nominal p", "rho", "tau", "test", "event_frac",
        "rate", "duration"
      ),
      # From `gs_design_npe()`
      c("analysis", "bound", "probability", "z", "probability0", "info_frac"),
      # From `gs_design_rd()`
      c(
        "prevalence", "z", "info", "theta", "rd", "info0", "n",
        "probability", "probability0", "info_frac0",
        "~risk difference at bound", "nominal p"
      ),
      # From `gs_design_wlr()`
      c(
        "IF", "time", "event", "info", "info0", "theta", "bound",
        "z", "n", "rate", "delta", "sigma2"
      ),
      # From `gs_info_ahr()`
      c("analysis", "time", "theta", "info", "info0"),
      # From `gs_info_rd()`
      c(
        "rate", "n_c", "n_e", "d", "p_e0", "p_pool_per_k_per_s", "p_c0",
        "analysis", "sum_ss", "sigma2_H0_per_k_per_s", "sum_inv_var_per_s",
        "sigma2_H1_per_k_per_s", "weight_per_k_per_s", "rd",
        "sigma2_H1", "sigma2_H0", "theta1", "theta0", "info1", "info0"
      ),
      # From `gs_power_ahr()`
      c(
        "z", "info", "probability", "probability0",
        "~hr at bound", "nominal p", "time", "info_frac",
        "theta", "n", "info0", "info_frac0"
      ),
      # From `gs_power_combo()`
      c(
        "time", "n", "event", "z", "probability", "probability_null",
        "nominal p", "rho", "tau", "test", "event_frac", "rate", "duration"
      ),
      # From `gs_power_npe()`
      c("bound", "analysis"),
      # From `gs_power_rd()`
      c(
        "z", "info", "theta", "probability", "probability0",
        "~risk difference at bound", "nominal p", "rd",
        "theta1", "theta0", "info_frac", "info0", "info_frac0"
      ),
      # From `gs_power_wlr()`
      c(
        "bound", "z", "probability", "probability0",
        "~hr at bound", "nominal p", "time", "info", "info_frac", "theta",
        "n", "info0", "info_frac0"
      ),
      # From `ppwe()`
      c("h", "duration", "H"),
      # From `pw_info()`
      c("t_end", "t_min"),
      # From `s2pwe()`
      c("Times", "Survival", "H", "duration", "rate"),
      # From `summary.fixed_design()`
      c("design", "n", "event", "time", "bound", "power"),
      # From `summary.gs_design()`
      c(
        "analysis", "time", "event", "n", "info_frac", "event_frac",
        "probability", "probability0", "bound", "rd", "z",
        "Analysis", "Bound", "Z", "~HR at bound", "Nominal p",
        "Alternate hypothesis", "Null hypothesis", "~wHR at bound",
        "~Risk difference at bound"
      ),
      # From `to_integer.fixed_design()`
      c("rate", "bound"),
      # From `to_integer.gs_design()`
      c("rate", "n_new")
    )
  )
)
