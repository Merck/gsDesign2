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

#' Weight functions for weighted log-rank test
#'
#' * `wlr_weight_fh` is Fleming-Harrington, FH(rho, gamma) weight function.
#' * `wlr_weight_1`  is constant for log rank test.
#' * `wlr_weight_power` is Gehan-Breslow and Tarone-Ware weight function.
#' * `wlr_weight_mb` is Magirr (2021) weight function.
#'
#' @param x A vector of numeric values.
#' @param arm0 An `arm` object defined in the npsurvSS package.
#' @param arm1 An `arm` object defined in the npsurvSS package.
#' @param rho A scalar parameter that controls the type of test.
#' @param gamma A scalar parameter that controls the type of test.
#' @param tau A scalar parameter of the cut-off time for modest weighted log rank test.
#' @param w_max A scalar parameter of the cut-off weight for modest weighted log rank test.
#' @param power A scalar parameter that controls the power of the weight function.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Compute the sample size via the sum of arm sizes.
#'    \item Compute the proportion of size in the two arms.
#'    \item If the input tau is specified, define time up to the cut off time tau.
#'    \item Compute the CDF using the proportion of the size in the two arms and \code{npsruvSS::psurv()}.
#'    \item Return the Fleming-Harrington weights for weighted Log-rank test.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @name wlr_weight

#' @rdname wlr_weight
#'
#' @return A vector of weights.
#'
#' @export
#'
#' @examples
#' enroll_rate <- define_enroll_rate(
#'   duration = c(2, 2, 10),
#'   rate = c(3, 6, 9)
#' )
#'
#' fail_rate <- define_fail_rate(
#'   duration = c(3, 100),
#'   fail_rate = log(2) / c(9, 18),
#'   hr = c(.9, .6),
#'   dropout_rate = .001
#' )
#'
#' gs_arm <- gs_create_arm(enroll_rate, fail_rate, ratio = 1)
#' arm0 <- gs_arm$arm0
#' arm1 <- gs_arm$arm1
#'
#' wlr_weight_fh(1:3, arm0, arm1, rho = 0, gamma = 0, tau = NULL)
wlr_weight_fh <- function(x, arm0, arm1, rho = 0, gamma = 0, tau = NULL) {
  n <- arm0$size + arm1$size
  p1 <- arm1$size / n
  p0 <- 1 - p1

  if (!is.null(tau)) {
    # Define time up to cut-off time tau
    if (tau > 0) {
      x <- pmin(x, tau)
    }
  }

  # CDF
  esurv <- p0 * npsurvSS::psurv(x, arm0) + p1 * npsurvSS::psurv(x, arm1)
  (1 - esurv)^rho * esurv^gamma
}

#' @rdname wlr_weight
#'
#' @return A vector of weights.
#'
#' @export
#'
#' @examples
#' enroll_rate <- define_enroll_rate(
#'   duration = c(2, 2, 10),
#'   rate = c(3, 6, 9)
#' )
#'
#' fail_rate <- define_fail_rate(
#'   duration = c(3, 100),
#'   fail_rate = log(2) / c(9, 18),
#'   hr = c(.9, .6),
#'   dropout_rate = .001
#' )
#'
#' gs_arm <- gs_create_arm(enroll_rate, fail_rate, ratio = 1)
#' arm0 <- gs_arm$arm0
#' arm1 <- gs_arm$arm1
#'
#' wlr_weight_1(1:3, arm0, arm1)
wlr_weight_1 <- function(x, arm0, arm1) {
  1
}

#' @rdname wlr_weight
#'
#' @return A vector of weights.
#'
#' @export
#'
#' @examples
#' enroll_rate <- define_enroll_rate(
#'   duration = c(2, 2, 10),
#'   rate = c(3, 6, 9)
#' )
#'
#' fail_rate <- define_fail_rate(
#'   duration = c(3, 100),
#'   fail_rate = log(2) / c(9, 18),
#'   hr = c(.9, .6),
#'   dropout_rate = .001
#' )
#'
#' gs_arm <- gs_create_arm(enroll_rate, fail_rate, ratio = 1)
#' arm0 <- gs_arm$arm0
#' arm1 <- gs_arm$arm1
#'
#' wlr_weight_n(1:3, arm0, arm1, power = 2)
wlr_weight_n <- function(x, arm0, arm1, power = 1) {
  n <- arm0$size + arm1$size
  p1 <- arm1$size / n
  p0 <- 1 - p1
  tmax <- arm0$total_time

  (n * (p0 * prob_risk(arm0, x, tmax) + p1 * prob_risk(arm1, x, tmax)))^power
}

#' @rdname wlr_weight
#'
#' @return A vector of weights.
#'
#' @export
#'
#' @examples
#' enroll_rate <- define_enroll_rate(
#'   duration = c(2, 2, 10),
#'   rate = c(3, 6, 9)
#' )
#'
#' fail_rate <- define_fail_rate(
#'   duration = c(3, 100),
#'   fail_rate = log(2) / c(9, 18),
#'   hr = c(.9, .6),
#'   dropout_rate = .001
#' )
#'
#' gs_arm <- gs_create_arm(enroll_rate, fail_rate, ratio = 1)
#' arm0 <- gs_arm$arm0
#' arm1 <- gs_arm$arm1
#'
#' wlr_weight_mb(1:3, arm0, arm1, tau = -1, w_max = 1.2)
wlr_weight_mb <- function(x, arm0, arm1, tau = NULL, w_max = Inf) {
  pmin(w_max, wlr_weight_fh(x, arm0, arm1, rho = -1, gamma = 0, tau = tau))
}
