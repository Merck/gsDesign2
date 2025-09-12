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

#' Fixed design under non-proportional hazards
#'
#' @description
#' Computes fixed design sample size (given power)
#' or power (given sample size) by:
#' - [fixed_design_ahr()] - Average hazard ratio method.
#' - [fixed_design_fh()] - Weighted logrank test with Fleming-Harrington
#'   weights (Farrington and Manning, 1990).
#' - [fixed_design_mb()] - Weighted logrank test with Magirr-Burman weights.
#' - [fixed_design_lf()] - Lachin-Foulkes method (Lachin and Foulkes, 1986).
#' - [fixed_design_maxcombo()] - MaxCombo method.
#' - [fixed_design_rmst()] - RMST method.
#' - [fixed_design_milestone()] - Milestone method.
#'
#' Additionally, [fixed_design_rd()] provides fixed design for binary endpoint
#' with treatment effect measuring in risk difference.
#'
#' @inheritParams gs_design_ahr
#' @inheritParams gs_power_ahr
#' @param power Power (`NULL` to compute power or strictly between 0
#'   and `1 - alpha` otherwise).
#' @param study_duration Study duration.
#'
#' @returns A list of design characteristic summary.
#'
#' @export
#'
#' @rdname fixed_design
#'
#' @examples
#' # AHR method ----
#'
#' # Example 1: given power and compute sample size
#' x <- fixed_design_ahr(
#'   alpha = .025, power = .9,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 1),
#'   fail_rate = define_fail_rate(
#'     duration = c(4, 100),
#'     fail_rate = log(2) / 12,
#'     hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36
#' )
#' x |> summary()
#'
#' # Example 2: given sample size and compute power
#' x <- fixed_design_ahr(
#'   alpha = .025,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 20),
#'   fail_rate = define_fail_rate(
#'     duration = c(4, 100),
#'     fail_rate = log(2) / 12,
#'     hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36
#' )
#' x |> summary()
#'
fixed_design_ahr <- function(
    enroll_rate,
    fail_rate,
    alpha = 0.025,
    power = NULL,
    ratio = 1,
    study_duration = 36,
    event = NULL,
    info_scale = c("h0_h1_info", "h0_info", "h1_info")) {
  # Check inputs ----
  check_enroll_rate(enroll_rate)
  check_fail_rate(fail_rate)
  check_enroll_rate_fail_rate(enroll_rate, fail_rate)
  info_scale <- match.arg(info_scale)

  # Save inputs ----
  input <- list(
    alpha = alpha, power = power, ratio = ratio, study_duration = study_duration,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate
  )

  # Generate design ----
  if (is.null(power)) {
    d <- gs_power_ahr(
      upper = gs_b, lower = gs_b,
      upar = qnorm(1 - alpha), lpar = -Inf,
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      ratio = ratio,
      analysis_time = study_duration,
      event = event,
      info_scale = info_scale
    )
  } else {
    d <- gs_design_ahr(
      alpha = alpha, beta = 1 - power,
      upper = gs_b, lower = gs_b,
      upar = qnorm(1 - alpha), lpar = -Inf,
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      ratio = ratio,
      analysis_time = study_duration,
      info_scale = info_scale
    )
  }

  # Prepare output ----
  ans <- tibble(
    design = "ahr",
    n = d$analysis$n,
    event = d$analysis$event,
    time = d$analysis$time,
    ahr = d$analysis$ahr,
    bound = (d$bound |> filter(bound == "upper"))$z,
    alpha = alpha,
    power = (d$bound |> filter(bound == "upper"))$probability
  )
  y <- structure(
    list(
      input = input, enroll_rate = d$enroll_rate, fail_rate = d$fail_rate,
      analysis = ans, design = "ahr"
    ),
    class = "fixed_design",
    design_display = "Average hazard ratio",
    title = "Fixed Design under AHR Method",
    footnote = "Power computed with average hazard ratio method."
  )

  return(y)
}
