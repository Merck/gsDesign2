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

#' Conditional power computation with non-constant effect size for non-/crossing an upper boundary at analysis j given observed Z value at analysis i
#'
#' @details
#' We assume \eqn{Z_i, i = 1, ..., K} are the z-values at an interim analysis i, respectively.
#' We assume further \eqn{Z_i, i = 1, ..., K} follows multivariate normal distribution
#' \deqn{E(Z_i) = \theta_i\sqrt{I_i}}
#' \deqn{Cov(Z_i, Z_j) = \sqrt{t_i/t_j}
#' See https://merck.github.io/gsDesign2/articles/story-npe-background.html for assumption details.
#' Returned value is list of
#' \deqn{P(\{Z_j \geq b_j\} \& \{\cap_{m=i+1}^{j-1} a_m \leq Z_m < b_m\} \mid Z_i = c)}.
#' \deqn{P(\{Z_j \geq b_j\} \& \{\cap_{m=i+1}^{j-1} Z_m < b_m\} \mid Z_i = c)}.
#' \deqn{P(\{Z_j \leq b_j\} \& \{\cap_{m=i+1}^{j-1} a_m \leq Z_m < b_m\} \mid Z_i = c)}.
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
#' @return A list of conditional powers: prob_alpha = \eqn{P(\{Z_j \geq b_j\} \& \{\cap_{m=i+1}^{j-1} a_m \leq Z_m < b_m\} \mid Z_i = c)}.
#'                                       prob_alpha_plus = \eqn{P(\{Z_j \geq b_j\} \& \{\cap_{m=i+1}^{j-1} Z_m < b_m\} \mid Z_i = c)}.
#'                                       prob_beta = eqn{P(\{Z_j \leq b_j\} \& \{\cap_{m=i+1}^{j-1} a_m \leq Z_m < b_m\} \mid Z_i = c)}.
#' @export
#'
#' @examples
#' library(gsDesign2)
#' library(dplyr)
#' library(mvtnorm)
#' # Calculate conditional power under arbitrary theta, info and lower/upper bound
#' # In practice, the value of theta and info commonly comes from a design.
#' # More examples are available at the pkgdown vignettes.
#' gs_cp_npe2(theta = c(0.1, 0.2, 0.3),
#'            t = c(0.15, 0.35, 0.6),
#'            info = c(15, 35, 60),
#'            a = c(-0.5),
#'            b = c(1.8, 2.1),
#'            c = 1.5)
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

  # ---------------------------------------------------------------------
  #             Calculate Lower/upper bounds for alpha
  # first cross efficacy bound at analysis j given no efficacy/futility
  # bound crossing at analysis i+1 to j-1
  # ---------------------------------------------------------------------
  # Integration limits: D_m = B_m - B_i
  # for D_{i+1},...,D_{j-1} use [a, b); for D_j use [b_j, +Inf)
  # lower bound
  lower_alpha <- rep(0, j - i)
  for(m in 1:(j-i-1)){
    lower_alpha[m] <- a[m] * sqrt(t[m]) - c * sqrt(t[i])
  }
  lower_alpha[j-i] <- b[j-i] * sqrt(t[j]) - c * sqrt(t[i])

  # upper bound
  upper_alpha <- rep(0, j - i)
  for(m in 1:(j-i-1)){
    upper_alpha[m] <- b[m] * sqrt(t[m]) - c * sqrt(t[i])
  }
  upper_alpha[j-i] <- Inf

  # ---------------------------------------------------------------------
  #             Calculate Lower/upper bounds for alpha_plus
  # first cross efficacy bound at analysis j given no efficacy bound
  # crossing at analysis i+1 to j-1
  # ---------------------------------------------------------------------
  # Integration limits: D_m = B_m - B_i
  # for D_{i+1},...,D_{j-1} use (-Inf, b); for D_j use [b_j, +Inf)
  # lower bound
  lower_alpha_plus <- rep(-Inf, j - i)
  lower_alpha_plus[j-i] <- b[j-i] * sqrt(t[j]) - c * sqrt(t[i])

  # upper bound
  upper_alpha_plus <- rep(0, j - i)
  for(m in 1:(j-i-1)){
    upper_alpha_plus[m] <- b[m] * sqrt(t[m]) - c * sqrt(t[i])
  }
  upper_alpha_plus[j-i] <- Inf



  # ---------------------------------------------------------------------
  #             Calculate Lower/upper bounds for beta
  # Not cross efficacy bound at analysis j given no efficacy/futility
  # bound crossing at analysis i+1 to j-1
  # ---------------------------------------------------------------------
  # Integration limits: D_m = B_m - B_i
  # for D_{i+1},...,D_{j-1} use [a, b); for D_j use [-Inf, b_j)
  # lower bound
  lower_beta <- rep(0, j - i)
  for(m in 1:(j-i-1)){
    lower_beta[m] <- a[m] * sqrt(t[m]) - c * sqrt(t[i])
  }
  lower_beta[j-i] <- -Inf

  # upper bound
  upper_beta <- rep(0, j - i)
  for(m in 1:(j-i)){
    upper_beta[m] <- b[m] * sqrt(t[m]) - c * sqrt(t[i])
  }


  # Compute multivariate normal probability
  prob_alpha <- mvtnorm::pmvnorm(
    lower = lower_alpha,
    upper = upper_alpha,
    mean = mu,
    sigma = Sigma)[1]

  prob_alpha_plus <- mvtnorm::pmvnorm(
    lower=lower_alpha_plus,
    upper=upper_alpha_plus,
    mean=mu,
    sigma=Sigma)[1]

  prob_beta <- mvtnorm::pmvnorm(
    lower=lower_beta,
    upper=upper_beta,
    mean=mu,
    sigma=Sigma)[1]


  return(prob = list(prob_alpha = prob_alpha,
                     prob_alpha_plus = prob_alpha_plus,
                     prob_beta = prob_beta))


}








