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

#' @inheritParams gs_design_wlr
#' @inheritParams gs_power_wlr
#' @param power Power (`NULL` to compute power or strictly between 0
#'   and `1 - alpha` otherwise).
#' @param ratio Experimental:Control randomization ratio.
#' @param study_duration Study duration.
#' @param tau Test parameter of Magirr-Burman method.
#' @param w_max Test parameter of Magirr-Burman method.
#' @export
#'
#' @rdname fixed_design
#'
#' @examples
#' # WLR test with MB weights ----
#'
#' # Example 1: given power and compute sample size
#' x <- fixed_design_mb(
#'   alpha = .025, power = .9,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 1),
#'   fail_rate = define_fail_rate(
#'     duration = c(4, 100),
#'     fail_rate = log(2) / 12,
#'     hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36,
#'   tau = 4,
#'   w_max = 2
#' )
#' x |> summary()
#'
#' # Example 2: given sample size and compute power
#' x <- fixed_design_mb(
#'   alpha = .025,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 20),
#'   fail_rate = define_fail_rate(
#'     duration = c(4, 100),
#'     fail_rate = log(2) / 12,
#'     hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36,
#'   tau = 4,
#'   w_max = 2
#' )
#' x |> summary()
#'
fixed_design_mb <- function(
    alpha = 0.025,
    power = NULL,
    ratio = 1,
    study_duration = 36,
    enroll_rate,
    fail_rate,
    tau = 6,
    w_max = Inf,
    info_scale = c("h0_h1_info", "h0_info", "h1_info")) {
  # Check inputs ----
  check_enroll_rate(enroll_rate)
  check_fail_rate(fail_rate)
  check_enroll_rate_fail_rate(enroll_rate, fail_rate)
  info_scale <- match.arg(info_scale)
  if (length(tau) > 1) {
    stop("fixed_design: multiple tau can not be used in Magirr-Burman method!")
  }

  # Save inputs ----
  input <- list(
    alpha = alpha, power = power, ratio = ratio, study_duration = study_duration,
    tau = tau, w_max = w_max,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate
  )

  # Generate design ----
  weight <- list(method = "mb", param = list(tau = tau, w_max = w_max))
  if (is.null(power)) {
    d <- gs_power_wlr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      ratio = ratio,
      weight = weight,
      upper = gs_b, upar = qnorm(1 - alpha),
      lower = gs_b, lpar = -Inf,
      analysis_time = study_duration,
      event = NULL,
      info_scale = info_scale
    )
  } else {
    d <- gs_design_wlr(
      alpha = alpha,
      beta = 1 - power,
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      ratio = ratio,
      weight = weight,
      upper = gs_b, upar = qnorm(1 - alpha),
      lower = gs_b, lpar = -Inf,
      analysis_time = study_duration,
      info_scale = info_scale
    )
  }

  # Prepare output ----
  ans <- tibble(
    design = "mb",
    n = d$analysis$n,
    event = d$analysis$event,
    time = d$analysis$time,
    ahr = d$analysis$ahr,
    bound = (d$bound |> filter(bound == "upper"))$z,
    alpha = alpha,
    power = (d$bound |> filter(bound == "upper"))$probability
  )
  design_display <- paste0("Modestly weighted LR: tau = ", tau)
  y <- structure(
    list(
      input = input, enroll_rate = d$enroll_rate, fail_rate = d$fail_rate,
      analysis = ans, design = "mb", design_par = list(tau = tau, w_max = w_max)
    ),
    class = "fixed_design",
    design_display = design_display,
    title = "Fixed Design under Magirr-Burman Method",
    footnote = paste(
      "Power for", design_display,
      "computed with method of Yung and Liu."
    )
  )

  return(y)
}
