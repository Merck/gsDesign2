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

#' Information and effect size for MaxCombo test
#'
#' @param enroll_rate Enrollment rates.
#' @param fail_rate Failure and dropout rates.
#' @param ratio Experimental:Control randomization ratio (not yet implemented).
#' @param event Targeted events at each analysis.
#' @param analysis_time Minimum time of analysis.
#' @param rho Weighting parameters.
#' @param gamma Weighting parameters.
#' @param tau Weighting parameters.
#' @param approx Approximation method.
#'
#' @return A tibble with columns as test index, analysis index,
#'   analysis time, sample size, number of events, ahr, delta,
#'   sigma2, theta, and statistical information.
#'
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' gs_info_combo(rho = c(0, 0.5), gamma = c(0.5, 0), analysis_time = c(12, 24))
gs_info_combo <- function(enroll_rate = tibble(
                            stratum = "All",
                            duration = c(2, 2, 10),
                            rate = c(3, 6, 9)
                          ),
                          fail_rate = tibble(
                            stratum = "All",
                            duration = c(3, 100),
                            fail_rate = log(2) / c(9, 18),
                            hr = c(.9, .6),
                            dropout_rate = rep(.001, 2)
                          ),
                          ratio = 1,
                          event = NULL,
                          analysis_time = NULL,
                          rho,
                          gamma,
                          tau = rep(-1, length(rho)),
                          approx = "asymptotic") {
  weight <- get_combo_weight(rho, gamma, tau)

  info <- lapply(weight, function(x) {
    x <- eval(parse(text = x))
    gs_info_wlr(
      enroll_rate = enroll_rate, fail_rate = fail_rate,
      event = event, analysis_time = analysis_time,
      ratio = ratio, weight = x
    )
  })

  info <- dplyr::bind_rows(info, .id = "test")
  info$test <- as.numeric(info$test)

  return(info)
}
