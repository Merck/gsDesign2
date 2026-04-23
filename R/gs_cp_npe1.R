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
#' Suppose there are \eqn{K} analyses. Let \eqn{Z_i} and \eqn{Z_j} be the Z-statistics at a current analysis \eqn{i} and a future analysis \eqn{j}, respectively.
#' Let's denote the statistical information at the \eqn{i}-th analysis as \eqn{I_i}.
#' We further assume \eqn{Z_i} and \eqn{Z_j} are bivariate normal with standard group sequential assumptions on independent increments, then
#' \deqn{E(Z_i) = \theta_i\sqrt{I_i}}
#' \deqn{Var(Z_i) = 1/I_i}
#' \deqn{Cov(Z_i, Z_j) = t \equiv I_i/I_j}.
#' See https://merck.github.io/gsDesign2/articles/story-npe-background.html for assumption details.
#' Returned value is
#' \deqn{P(Z_j > z_j \mid Z_i = z_i) = 1 - \Phi\left(\frac{z_j - \sqrt{t}z_i - \sqrt{I_j}(\theta_j - \theta_i\sqrt{t})}{\sqrt{1 - t}}\right)}
#'
#' @param theta A numeric vector of length two, which specifies the natural parameter for treatment effect.
#'              The first element of `theta` is the treatment effect of an interim analysis i.
#'              The second element of `theta` is the treatment effect of a future analysis j.
#' @param info A vector of length two, which specifies the statistical information under the treatment effect `theta`.
#' @param zi Numeric scalar z-value observed at analysis \eqn{i}.
#' @param zj Numeric scalar at the future analysis \eqn{j}.
#' @return A scalar with the conditional power \eqn{P(Z_j > z_i \mid Z_i = z_i)}.
#' @export
#'
#' @examples
#' library(gsDesign2)
#'
#' # Calculate conditional power under arbitrary theta and info
#' # In practice, the value of theta and info commonly comes from a design.
#' # More examples are available at the pkgdown vignettes.
#' gs_cp_npe1(theta = c(.1, .2), info = c(15, 35), zi = 1.5, zj = 1.96)
#'
gs_cp_npe1 <- function(theta = NULL, info = NULL, zi = NULL, zj = NULL
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

  # ----------------------------------------- #
  #   calculate conditional power under theta #
  # ----------------------------------------- #

  t <- info[1] / info[2]
  numerator1 <- zj -  zi * sqrt(t)
  numerator2 <- theta[2] * sqrt(info[2]) - theta[1] * sqrt(t * info[1])
  denominator <- sqrt(1 - t)
  conditional_power <- pnorm((numerator1 - numerator2)  / denominator, lower.tail = FALSE)

  return(conditional_power)
}
