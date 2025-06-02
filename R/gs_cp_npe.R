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

#' Conditional power computation with non-constant effect size
#'
#' @details
#' We assume \eqn{Z_1} and \eqn{Z_2} are the z-values at an interim analysis and later analysis, respectively.
#' We assume further \eqn{Z_1} and \eqn{Z_2} are bivariate normal with standard group sequential assumptions
#' on independent increments where for \eqn{i=1,2}
#' \deqn{E(Z_i) = \theta_i\sqrt{I_i}}
#' \deqn{Var(Z_i) = 1/I_i}
#' \deqn{Cov(Z_1, Z_2) = t \equiv I_1/I_2}
#' where \eqn{\theta_1, \theta_2} are real values and \eqn{0<I_1<I_2}.
#' See https://merck.github.io/gsDesign2/articles/story-npe-background.html for assumption details.
#' Returned value is
#' \deqn{P(Z_2 > b \mid Z_1 = a) = 1 - \Phi\left(\frac{b - \sqrt{t}a - \sqrt{I_2}(\theta_2 - \theta_1\sqrt{t})}{\sqrt{1 - t}}\right)}
#'
#' @param theta A vector of length two, which specifies the natural parameter for treatment effect.
#'              The first element of `theta` is the treatment effect of an interim analysis i.
#'              The second element of `theta` is the treatment effect of a future analysis j.
#' @param info A vector of two, which specifies the statistical information under the treatment effect `theta`.
#' @param a Interim z-value at analysis i (scalar).
#' @param b Future target z-value at analysis j (scalar).
#' @return A scalar with the conditional power \eqn{P(Z_2>b\mid Z_1=a)}.
#' @export
#'
#' @examples
#' library(gsDesign2)
#' library(dplyr)
#'
#' # Calculate conditional power under arbitrary theta and info
#' # In practice, the value of theta and info commonly comes from a design.
#' # More examples are available at the pkgdown vignettes.
#' gs_cp_npe(theta = c(.1, .2),
#'           info = c(15, 35),
#'           a = 1.5, b = 1.96)
gs_cp_npe <- function(theta = NULL,
                      info = NULL,
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

  check_info(info)

  # ----------------------------------------- #
  #   calculate conditional power under theta #
  # ----------------------------------------- #

  t <- info[1] / info[2]
  numerator1 <- b -  a * sqrt(t)
  numerator2 <- theta[2] * sqrt(info[2]) - theta[1] * sqrt(t * info[1])
  denominator <- sqrt(1 - t)
  conditional_power <- pnorm((numerator1 - numerator2)  / denominator, lower.tail = FALSE)

  return(conditional_power)
}
