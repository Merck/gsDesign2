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
#' @param p_c A numerical value of the control arm rate.
#' @param p_e A numerical value of the experimental arm rate.
#' @param rd0 Risk difference under null hypothesis, default is 0.
#' @param n Sample size. If NULL with power input, the sample size will be
#' computed to achieve the targeted power
#' @param ratio Experimental:Control randomization ratio.
#' @param info_scale Information scale for calculation. Options are:
#'   - `"h0_h1_info"` (default): variance under both null and alternative hypotheses is used.
#'   - `"h0_info"`: variance under null hypothesis is used.
#'   - `"h1_info"`: variance under alternative hypothesis is used.
#' @export
#'
#' @rdname fixed_design
#'
#' @examples
#' # Binary endpoint with risk differences ----
#'
#' # Example 1: given power and compute sample size
#' x <- fixed_design_rd(
#'   alpha = 0.025, power = 0.9, p_c = .15, p_e = .1,
#'   rd0 = 0, ratio = 1
#' )
#' x |> summary()
#'
#' # Example 2: given sample size and compute power
#' x <- fixed_design_rd(
#'   alpha = 0.025, power = NULL, p_c = .15, p_e = .1,
#'   rd0 = 0, n = 2000, ratio = 1
#' )
#' x |> summary()
#'
fixed_design_rd <- function(
    alpha = 0.025,
    power = NULL,
    ratio = 1,
    p_c,
    p_e,
    rd0 = 0,
    n = NULL,
    info_scale = c("h0_h1_info", "h0_info", "h1_info")) {
  # Check inputs ----
  info_scale <- match.arg(info_scale)
  if (!is.numeric(p_c) || !is.numeric(p_e)) {
    stop("fixed_design_rd: p_c and p_e should be numerical values.")
  }
  if (length(p_c) > 1 || length(p_e) > 1) {
    stop("fixed_design_rd: p_c and p_e should be a scaler.")
  }
  if (!is.null(n) && !is.numeric(n)) {
    stop("fixed_design_rd: n should be numerical values.")
  }

  # Save inputs ----
  input <- list(
    alpha = alpha, power = power, ratio = ratio,
    p_c = p_c, p_e = p_e, n = n
  )

  # Generate design ----
  if (is.null(power)) {
    d <- gs_power_rd(
      p_c = tibble(stratum = "All", rate = p_c),
      p_e = tibble(stratum = "All", rate = p_e),
      ratio = ratio,
      upper = gs_b, upar = qnorm(1 - alpha),
      lower = gs_b, lpar = -Inf,
      n = tibble(stratum = "All", n = n, analysis = 1),
      rd0 = rd0, weight = "unstratified",
      info_scale = info_scale
    )
  } else {
    d <- gs_design_rd(
      p_c = tibble(stratum = "All", rate = p_c),
      p_e = tibble(stratum = "All", rate = p_e),
      alpha = alpha, beta = 1 - power, ratio = ratio,
      upper = gs_b, upar = qnorm(1 - alpha),
      lower = gs_b, lpar = -Inf,
      rd0 = rd0, weight = "unstratified",
      info_scale = info_scale
    )
  }

  # Prepare output ----
  ans <- tibble(
    design = "rd",
    n = d$analysis$n,
    bound = (d$bound |> filter(bound == "upper"))$z,
    alpha = alpha,
    power = (d$bound |> filter(bound == "upper"))$probability
  )
  y <- structure(
    list(
      input = input, enroll_rate = d$enroll_rate, fail_rate = d$fail_rate,
      analysis = ans, design = "rd"
    ),
    class = "fixed_design",
    design_display = "Risk difference",
    title = "Fixed Design of Risk Difference under Farrington-Manning Method",
    footnote = paste(
      "Risk difference power without continuity correction using method of",
      "Farrington and Manning."
    )
  )

  return(y)
}
