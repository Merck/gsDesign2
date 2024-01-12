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

#' Grid points for group sequential design numerical integration
#'
#' Points and weights for Simpson's rule numerical integration from
#' p 349--350 of Jennison and Turnbull book.
#' This is not used for arbitrary integration, but for the canonical form of Jennison and Turnbull.
#' mu is computed elsewhere as drift parameter times sqrt of information.
#' Since this is a lower-level routine, no checking of input is done; calling routines should
#' ensure that input is correct.
#' Lower limit of integration can be `-Inf` and upper limit of integration can be `Inf`.
#'
#' @details
#' Jennison and Turnbull (p 350) claims accuracy of `10e-6` with `r=16`.
#' The numerical integration grid spreads out at the tail to enable accurate tail probability calculations.
#'
#' @param r Integer, at least 2; default of 18 recommended by Jennison and Turnbull.
#' @param mu Mean of normal distribution (scalar) under consideration.
#' @param a Lower limit of integration (scalar).
#' @param b Upper limit of integration (scalar `> a`).
#'
#' @return A list with grid points in `z` and numerical integration weights in `w`.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Define odd numbered grid points for real line.
#'    \item Trim points outside of $[a, b]$ and include those points.
#'    \item If extreme, include only 1 point where density will be essentially 0.
#'    \item Define even numbered grid points between the odd ones.
#'    \item Compute weights for odd numbered grid points.
#'    \item Combine odd- and even-numbered grid points with their corresponding weights.
#'    \item Return a tibble of with grid points in z and numerical integration weights in z.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @noRd
#'
#' @examples
#' # Approximate variance of standard normal (i.e., 1)
#' g <- gridpts()
#' sum((g$z)^2 * g$w * dnorm(g$z))
#'
#' # Approximate probability above .95 quantile (i.e., 0.05)
#' g <- gridpts(a = qnorm(0.95), b = Inf)
#' sum(g$w * dnorm(g$z))
gridpts <- function(r = 18, mu = 0, a = -Inf, b = Inf) {
  gridpts_rcpp(r = r, mu = mu, a = a, b = b)
}

#' Initialize numerical integration for group sequential design
#'
#' Compute grid points for first interim analysis in a group sequential design.
#'
#' @param r Integer, at least 2; default of 18 recommended by Jennison and Turnbull.
#' @param theta Drift parameter for first analysis.
#' @param info Information at first analysis.
#' @param a Lower limit of integration (scalar).
#' @param b Upper limit of integration (scalar `> a`).
#'
#' @return A list with grid points in `z`, numerical integration weights in `w`,
#'   and a normal density with mean `mu = theta * sqrt{I}`
#'   and variance 1 times the weight in `h`.
#'
#' @details
#' Mean for standard normal distribution under consideration is `mu = theta * sqrt(I)`.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Compute drift at analysis 1.
#'    \item Compute deviation from drift.
#'    \item Compute standard normal density, multiply by grid weight.
#'    \item Return a tibble of z, w, and h.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @noRd
#'
#' @examples
#' # Replicate variance of 1, mean of 35
#' g <- h1(theta = 5, info = 49)
#' mu <- sum(g$z * g$h)
#' var <- sum((g$z - mu)^2 * g$h)
#'
#' # Replicate p-value of 0.0001 by numerical integration of tail
#' g <- h1(a = qnorm(0.9999))
#' sum(g$h)
h1 <- function(r = 18, theta = 0, info = 1, a = -Inf, b = Inf) {
  h1_rcpp(r = r, theta = theta, I = info, a = a, b = b)
}

#' Update numerical integration for group sequential design
#'
#' Update grid points for numerical integration from one analysis to the next.
#'
#' @param r Integer, at least 2; default of 18 recommended by Jennison and Turnbull.
#' @param theta Drift parameter for current analysis.
#' @param info Information at current analysis.
#' @param a Lower limit of integration (scalar).
#' @param b Upper limit of integration (scalar `> a`).
#' @param thetam1 Drift parameter for previous analysis.
#' @param im1 Information at previous analysis.
#' @param gm1 Numerical integration grid from [h1()] or previous run of [hupdate()].
#'
#' @return A list with grid points in `z`,
#'   numerical integration weights in `w`,
#'   a normal density with mean `mu = theta * sqrt{I}`
#'   and variance 1 times the weight in `h`.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Compute the square root of the change in information.
#'    \item Compute the grid points for group sequential design numerical integration.
#'    \item Update the integration.
#'    \item Return a tibble of z, w, and h.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @noRd
#'
#' @examples
#' # 2nd analysis with no interim bound and drift 0 should have mean 0, variance 1
#' g <- hupdate()
#' mu <- sum(g$z * g$h)
#' var <- sum((g$z - mu)^2 * g$h)
hupdate <- function(r = 18, theta = 0, info = 2, a = -Inf, b = Inf, thetam1 = 0, im1 = 1, gm1 = h1()) {
  hupdate_rcpp(r = r, theta = theta, I = info, a = a, b = b, thetam1 = thetam1, Im1 = im1, gm1 = gm1)
}
