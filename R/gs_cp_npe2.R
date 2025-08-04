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

#' Conditional power computation with non-constant effect size for first crossing an upper boundary at analysis j given observed Z value at analysis i
#'
#' @details
#' We assume \eqn{Z_i, i = 1, ..., K} are the z-values at an interim analysis i, respectively.
#' We assume further \eqn{Z_i, i = 1, ..., K} follows multivariate normal distribution
#' \deqn{E(Z_i) = \theta_i\sqrt{I_i}}
#' \deqn{Cov(Z_i, Z_j) = \sqrt{t_i/t_j}
#' See https://merck.github.io/gsDesign2/articles/story-npe-background.html for assumption details.
#' Returned value is
#' \deqn{P(\{Z_j > b_j\} \& \{\cap_{m=i+1}^{j-1} a_m \leq Z_m < b_m\} \mid Z_i = c)}.
#'
#' @param theta A vector of j-i+1, which specifies the natural parameter for treatment effect.
#'              The first element of `theta` is the treatment effect of an interim analysis i.
#'              The second element of `theta` is the treatment effect of an interim analysis i+1.
#'              ...
#'              The last element of `theta` is the treatment effect of a future analysis j.
#' @param t A vector of j-i+1, which specifies the information fraction under the treatment effect `theta`.
#' @param info A vector of j-i+1, which specifies the statistical information under the treatment effect `theta`.
#' @param a A vector of length j-i-1, which specifies the futility bounds from analysis i+1 to analysis j-1.
#' @param b A vector of length j-i, which specifies the efficacy bounds from analysis i+1 to analysis j.
#' @param c Interim z-value at analysis i (scalar).
#' @return A scalar with the conditional power \eqn{P(\{Z_j > b_j\} \& \{\cap_{m=i+1}^{j-1} a_m \leq Z_m < b_m\} \mid Z_i = c)}.
#' @export
#'
#' @examples
#' library(gsDesign2)
#' library(dplyr)
#' library(mvtnorm)
#' # Calculate conditional power under arbitrary theta, info and lower/upper bound
#' # In practice, the value of theta and info commonly comes from a design.
#' # More examples are available at the pkgdown vignettes.
#' gs_cp_npe2(theta = c(),
#'            t = c(),
#'            info = c(),
#'            a = c(),
#'            b = c(),
#'            c = 1.96)

gs_cp_npe2 <- function(theta = NULL,
                       t = NULL,
                       info = NULL,
                       a = NULL,
                       b = NULL,
                       c = NULL){
  # Input check
  if(length(theta) != length(t))
    stop("The input of theta should have the same length of the input of t.")

  if(length(theta) != length(info))
    stop("The input of theta should have the same length of the input of info.")

  if(length(a) != (length(b) - 1))
    stop("The vector of lower bound should have the length of vector of lower bound - 1.")

  # Build mean vector of length M = j-i
  i <- 1
  j <- length(b)
  dim <- j - i # number of increments

  # Build mean vector
  mu <- sapply(seq_len(dim), function(k){
    idx <- i + k
    theta[idx] * sqrt(t[idx] * info[idx]) - theta[i] * sqrt(t[i] * info[i])
  })

  # Build covariance matrix Sigma (dim x dim)
  Sigma <- matrix(0, nrow = dim, ncol = dim)

  for(k in seq_len(dim)) {
    for(l in seq_len(dim)) {
      Sigma[k, l] <- t[i + min(k,l)] - t[i]
    }
  }

  # Integration limits: D_m = B_m - B_i
  # for D_{i+1},...,D_{j-1} use [a, b); for D_j use [b_j, +Inf)
  # lower bound
  lower <- rep(0, j - i)
  for(m in 1:(j-i-1)){
    lower[m] <- a[m] * sqrt(t[m]) - c * sqrt(t[i])
  }
  lower[j-i] <- b[j-i] * sqrt(t[j]) - c * sqrt(t[i])

  # upper bound
  upper <- rep(0, j - i)
  for(m in 1:(j-i-1)){
    upper[m] <- b[m] * sqrt(t[m]) - c * sqrt(t[i])
  }
  upper[j-i] <- Inf

  # Compute multivariate normal probability
  prob <- mvtnorm::pmvnorm(lower=lower,
                           upper=upper,
                           mean=mu,
                           sigma=Sigma)[1]
  return(prob)


}








