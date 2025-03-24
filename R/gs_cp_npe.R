#  Copyright (c) 2024 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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

#' Conditional power computation with non-constant effect size
#'
#' @details
#' We assume \eqn{Z_1} and \eqn{Z_2} are the z-values at an interim analysis and later analysis, respectively.
#' We assume further \eqn{Z_1} and \eqn{Z_2} are bivariate normal with standard group sequential assumptions
#' on independent increments where for \eqn{i=1,2}
#' \deqn{E(Z_i) = \theta_i\sqrt{I_i}},
#' \deqn{Var(Z_i) = 1/I_i},
#' \cov{Z_1, Z_2} = t \equiv I_1/I_2,
#' where \eqn{\theta_1, \theta_2} are real values and \eqn{0<I_1<I_2}.
#' See https://merck.github.io/gsDesign2/articles/story-npe-background.html for assumption details.
#' Returned value is
#' \deqn{P(Z_2 > b | Z_1 = a)} =
#'       1 - \Phi\left(\frac{b - \sqrt{t}a-\sqrt{I_{2}}(\theta_{2}-\theta_1\sqrt{t}}{\sqrt(1 - t)}\right)}.}
#'
#' @param theta A vector of length two, which specifies the natural parameter for treatment effect.
#'              The first element of `theta` is the treatment effect of the interim analysis i.
#'              The second element of `theta` is the treatment effect of the future analysis j.
#' @param info A vector of two, which specifies the statistical information under the treatment effect `theta`.
#' @param max_info A scalar specifying the planned statistical information at final analysis.
#' @param a Interim z-value at analysis i (scalar).
#' @param b Future z-value at analysis j (scalar).
#' @value A scalar with the conditional power \eqn{P(Z_2>b|Z_1=a)}.
#' @export
#'
#' @examples
#' library(gsDesign2)
#' library(dplyr)
#'
#' # ---------------------------------- #
#' #             Example 1              #
#' # CP under arbitrary theta and info  #
#' # ---------------------------------- #
#' gs_cp_npe(theta = c(.1, .2),
#'           info = c(15, 35),
#'           a = 1.5, b = 1.96)
#'
#' # ---------------------------------- #
#' #             Example 2              #
#' # Calculate conditional power and    #
#' #       error of a design            #
#' # ---------------------------------- #
#' x <- gs_design_ahr(
#'   fail_rate = define_fail_rate(duration = c(4, Inf),
#'                                fail_rate = log(2) / 10,
#'                                hr = c(1, 0.7),
#'                                dropout_rate = 0.0001),
#'   analysis_time = c(12, 24, 36))
#'
#' # Example 2A ----
#' # Conditional error of FA given IA1 Z-score of 1.75
#' gs_cp_npe(theta = c(0, 0),
#'           info = x$analysis$info0[c(1, 3)],
#'           a = 1.75,
#'           b = x$bound$z[x$bound$bound == "upper" & x$bound$analysis == 3])
#'
#' # Example 2B ----
#' # Conditional power of FA given
#' # (1) IA1 Z-score of 1.75;
#' # (2) H1 assumed treatment effect
#' gs_cp_npe(theta = x$analysis$theta[c(1, 3)],
#'           # taking info0 in this example gives minor differences
#'           info = x$analysis$info[c(1, 3)],
#'           a = 1.75,
#'           b = x$bound$z[x$bound$bound == "upper" & x$bound$analysis == 3])
#'
#' # Example 2C ----
#' # Assume at IA1:
#' # (1) The Z-score is 1.75;
#' # (2) There are 50 events observed during the first 4 months since study starts
#' # (3) There are 150 events observed after the 4th month.
#' # For IA1, we take the blinded estimation of theta and info.
#' # For FA, we take the planned theta and info.
#' theta_blinded <- -sum(log(c(1, 0.7)) * c(50, 150)) / 200
#' info_blinded <- 200 / 4
#' gs_cp_npe(theta = c(theta_blinded, x$analysis$theta[3]),
#'           info = c(info_blinded, x$analysis$info0[3]),
#'           a = 1.75,
#'           b = x$bound$z[x$bound$bound == "upper" & x$bound$analysis == 3])
#'
#' # Example 2D ----
#' # If the HR is not assumed, say, HR = 1 for the first 4 months and 0.8 afterwards
#' # At FA, assume there are 70 events during the first 4 months and 700 events afterwards.
#' # We first calculate the expected events at IA1 under the new HR
#' e_event_ia1 <- expected_event(enroll_rate = x$enroll_rate,
#'                               fail_rate = x$fail_rate |> mutate(hr = c(1, 0.8)),
#'                               total_duration = x$analysis$time[1],
#'                               simple = FALSE)
#' # Under the new HR, there are 150 events during the first 4 months and 67 events afterwards.
#' # If 160 and 67 are the blinded events, we can derive the blinded estimation of treatment effect.
#' theta_blinded_ia1 <- -sum(log(c(1, 0.8)) * c(150, 67)) / (150 + 67)
#' info_blinded_ia1 <- (150 + 67) / 4
#'
#' # We further calculate the expected events at FA under the new HR
#' e_event_fa <- expected_event(enroll_rate = x$enroll_rate,
#'                              fail_rate = x$fail_rate |> mutate(hr = c(1, 0.8)),
#'                              total_duration = x$analysis$time[3],
#'                              simple = FALSE)
#' # Under the new HR, there are 223 events during the first 4 months and 562 events afterwards.
#' # If 160 and 67 are the blinded events, we can derive the blinded estimation of treatment effect.
#' theta_blinded_fa <- -sum(log(c(1, 0.8)) * c(223, 562)) / (223 + 562)
#' info_blinded_fa <- (223 + 562) / 4
#'
#' gs_cp_npe(theta = c(theta_blinded_ia1, theta_blinded_fa),
#'           info = c(info_blinded_ia1, info_blinded_fa),
#'           a = 1.75,
#'           b = x$bound$z[x$bound$bound == "upper" & x$bound$analysis == 3])
gs_cp_npe <- function(theta = NULL,
                      info = NULL,
                      max_info = max(info),
                      a = NULL, b = NULL
                      ) {
  # ----------------------------------------- #
  #       input checking                      #
  # ----------------------------------------- #
  # check theta
  if (is.null(theta)) {
    stop("Please provide theta (arbitrary treatment effect) to calculate conditional power.")
  } else if (length(theta) == 1) {
    theta <- rep(theta, 2)
  }

  # check info
  if (is.null(info)) {
    stop("Please provide info (statistical information given the treatment effect theta) to calculate conditional power.")
  }

  if (max(info) > max_info) {
    stop("The max(info) should be smaller than max_info.")
  }

  check_info(info)

  # calculate information fraction
  info_frac <- info / max_info

  # ----------------------------------------- #
  #   calculate conditional power under theta #
  # ----------------------------------------- #
  numerator1 <- sqrt(b * info_frac[2]) -  a * sqrt(info_frac[1])
  numerator2 <- theta[2] * sqrt(info_frac[2] * info[2]) - theta[1] * sqrt(info_frac[1] * info[1])
  denominator <- sqrt(info_frac[2] - info_frac[1])
  conditional_power <- pnorm((numerator1 - numerator2)  / denominator, lower.tail = FALSE)

  return(conditional_power)
}
