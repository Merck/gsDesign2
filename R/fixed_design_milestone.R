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

#' @inheritParams gs_design_ahr
#' @param alpha One-sided Type I error (strictly between 0 and 1).
#' @param power Power (`NULL` to compute power or strictly between 0
#'   and `1 - alpha` otherwise).
#' @param ratio Experimental:Control randomization ratio.
#' @param study_duration Study duration.
#' @param tau Test parameter of milestone method.
#'
#' @export
#'
#' @rdname fixed_design
#'
#' @examples
#' # Milestone method ----
#'
#' # Example 1: given power and compute sample size
#' x <- fixed_design_milestone(
#'   alpha = .025, power = .9,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 1),
#'   fail_rate = define_fail_rate(
#'     duration = 100,
#'     fail_rate = log(2) / 12,
#'     hr = .7,
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36,
#'   tau = 18
#' )
#' x |> summary()
#'
#' # Example 2: given sample size and compute power
#' x <- fixed_design_milestone(
#'   alpha = .025,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 20),
#'   fail_rate = define_fail_rate(
#'     duration = 100,
#'     fail_rate = log(2) / 12,
#'     hr = .7,
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36,
#'   tau = 18
#' )
#' x |> summary()
#'
fixed_design_milestone <- function(
    alpha = 0.025,
    power = NULL,
    ratio = 1,
    enroll_rate,
    fail_rate,
    study_duration = 36,
    tau = NULL) {
  # Check inputs ----
  check_enroll_rate(enroll_rate)
  check_fail_rate(fail_rate)
  check_enroll_rate_fail_rate(enroll_rate, fail_rate)
  if (is.null(tau)) {
    tau <- study_duration
  }
  if (!is.numeric(tau) || length(tau) > 1) {
    stop("fixed_design_milestone: tau should a scaler.")
  }

  # Save inputs ----
  input <- list(
    alpha = alpha, power = power, ratio = ratio, study_duration = study_duration,
    tau = tau, enroll_rate = enroll_rate, fail_rate = fail_rate
  )

  # Generate design ----
  if (is.null(power)) {
    d <- fixed_design_power_rmst(
      alpha = alpha, ratio = ratio,
      enroll_rate = enroll_rate, fail_rate = fail_rate,
      analysis_time = study_duration,
      test = "survival_difference",
      tau = tau
    )
  } else {
    d <- fixed_design_size_rmst(
      alpha = alpha, beta = 1 - power, ratio = ratio,
      enroll_rate = enroll_rate, fail_rate = fail_rate,
      analysis_time = study_duration,
      test = "survival_difference",
      tau = tau
    )
  }

  # Prepare output ----
  ans <- tibble(
    design = "milestone",
    n = d$analysis$n,
    event = d$analysis$event,
    time = d$analysis$time,
    bound = (d$bound |> filter(bound == "upper"))$z,
    alpha = alpha,
    power = (d$bound |> filter(bound == "upper"))$probability
  )
  design_display <- paste("Milestone: tau =", tau)
  y <- structure(
    list(
      input = input, enroll_rate = d$enroll_rate, fail_rate = d$fail_rate,
      analysis = ans, design = "milestone", design_par = list(tau = tau)
    ),
    class = "fixed_design",
    design_display = design_display,
    title = "Fixed Design under Milestone Method",
    footnote = paste(
      "Power for", design_display,
      "computed with method of Yung and Liu."
    )
  )

  return(y)
}
