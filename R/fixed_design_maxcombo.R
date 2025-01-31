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

#' @param alpha One-sided Type I error (strictly between 0 and 1).
#' @param power Power (`NULL` to compute power or strictly between 0
#'   and `1 - alpha` otherwise).
#' @param ratio Experimental:Control randomization ratio.
#' @param study_duration Study duration.
#' @param rho A vector of numbers paring with gamma and tau for MaxCombo test.
#' @param gamma A vector of numbers paring with rho and tau for MaxCombo test.
#' @param tau A vector of numbers paring with gamma and rho for MaxCombo test.
#' @inheritParams gs_design_combo
#'
#' @export
#'
#' @rdname fixed_design
#'
#' @examples
#' # MaxCombo test ----
#'
#' # Example 1: given power and compute sample size
#' x <- fixed_design_maxcombo(
#'   alpha = .025, power = .9,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 1),
#'   fail_rate = define_fail_rate(
#'     duration = c(4, 100),
#'     fail_rate = log(2) / 12,
#'     hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36,
#'   rho = c(0, 0.5), gamma = c(0, 0), tau = c(-1, -1)
#' )
#' x |> summary()
#'
#' # Example 2: given sample size and compute power
#' x <- fixed_design_maxcombo(
#'   alpha = .025,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 20),
#'   fail_rate = define_fail_rate(
#'     duration = c(4, 100),
#'     fail_rate = log(2) / 12,
#'     hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36,
#'   rho = c(0, 0.5), gamma = c(0, 0), tau = c(-1, -1)
#' )
#' x |> summary()
#'
fixed_design_maxcombo <- function(
    alpha = 0.025,
    power = NULL,
    ratio = 1,
    study_duration = 36,
    enroll_rate,
    fail_rate,
    rho = c(0, 0, 1),
    gamma = c(0, 1, 0),
    tau = rep(-1, 3)) {
  # Check inputs ----
  check_enroll_rate(enroll_rate)
  check_fail_rate(fail_rate)
  check_enroll_rate_fail_rate(enroll_rate, fail_rate)

  # Save inputs ----
  input <- list(
    alpha = alpha, power = power, ratio = ratio, study_duration = study_duration,
    rho = rho, gamma = gamma, tau = tau,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate
  )

  # Generate design ----
  # organize the tests in MaxCombo
  max_combo_test <- data.frame(
    rho = rho,
    gamma = gamma,
    tau = tau
  ) |>
    mutate(test = seq(1, length(rho)), analysis = 1, analysis_time = study_duration)
  # check if power is NULL or not
  if (is.null(power)) {
    d <- gs_power_combo(
      ratio = ratio,
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      fh_test = max_combo_test,
      upper = gs_spending_combo,
      upar = list(sf = gsDesign::sfLDOF, total_spend = alpha),
      lower = gs_b, lpar = -Inf
    )
  } else {
    d <- gs_design_combo(
      alpha = alpha, beta = 1 - power, ratio = ratio,
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      fh_test = max_combo_test,
      upper = gs_spending_combo,
      upar = list(sf = gsDesign::sfLDOF, total_spend = alpha),
      lower = gs_b, lpar = -Inf
    )
  }

  # Prepare output ----
  ans <- tibble(
    design = "maxcombo",
    n = d$analysis$n,
    event = d$analysis$event,
    time = d$analysis$time,
    bound = (d$bound |> filter(bound == "upper"))$z,
    alpha = alpha,
    power = (d$bound |> filter(bound == "upper"))$probability
  )
  design_display <- gsub(
    "FH(0, 0)", "logrank", paste(
      "MaxCombo:", paste0(
        "FHC(", rho, ", ", gamma, ")",
        collapse = ", "
      )
    ),
    fixed = TRUE
  )
  y <- structure(
    list(
      input = input, enroll_rate = d$enroll_rate, fail_rate = d$fail_rate,
      analysis = ans, design = "maxcombo",
      design_par = list(rho = rho, gamma = gamma, tau = tau)
    ),
    class = "fixed_design",
    design_display = design_display,
    title = "Fixed Design under MaxCombo Method",
    footnote = paste0(
      "Power for MaxCombo test with Fleming-Harrington tests ",
      substring(design_display, 9),
      "."
    )
  )

  return(y)
}
