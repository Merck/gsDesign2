#  Copyright (c) 2026 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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
#' We assume \eqn{Z_i, i = 1, ..., K} are the z-statistics at an interim analysis i, respectively.
#' We assume further \eqn{Z_i, i = 1, ..., K} follows multivariate normal distribution
#' \deqn{E(Z_i) = \theta_i\sqrt{I_i}}
#' \deqn{Cov(Z_i, Z_j) = I_i/I_j.
#' See https://merck.github.io/gsDesign2/articles/story-npe-background.html for assumption details.
#' Returned value is list of
#' \deqn{P(\{Z_j \geq b_j\} \& \{\cap_{m=i+1}^{j-1} a_m \leq Z_m < b_m\} \mid Z_i = z_i)}.
#' \deqn{P(\{Z_j \geq b_j\} \& \{\cap_{m=i+1}^{j-1} Z_m < b_m\} \mid Z_i = z_i)}.
#' \deqn{P(\{Z_j \leq b_j\} \& \{\cap_{m=i+1}^{j-1} a_m \leq Z_m < b_m\} \mid Z_i = z_i)}.
#'
#' @param x An object of type gsDesign2.
#' @param theta Optional numeric vector with length \eqn{j-i+1}, which specifies the natural parameter for treatment effect of interim analysis \eqn{i} through analysis \eqn{j}.
#' @param i Index of current analysis, with default of 1.
#' @param zi Numeric scalar z-value observed at analysis \eqn{i}.
#' @return A list of conditional powers: prob_alpha is a numeric vector of (\eqn{alpha_i,i+1, ..., alpha_i,j-1, alpha_i,j}), where alpha_i,j = \eqn{P(\{Z_j \geq b_j\} \& \{\cap_{m=i+1}^{j-1} a_m \leq Z_m < b_m\} \mid Z_i = z_i)}.
#'                                       prob_alpha_plus is a numeric vector of (\eqn{alpha^+_i,i+1, ..., alpha^+_i,j-1, alpha^+_i,j}), where alpha^+_i,j = \eqn{P(\{Z_j \geq b_j\} \& \{\cap_{m=i+1}^{j-1} Z_m < b_m\} \mid Z_i = z_i)}.
#'                                       prob_beta is a numeric vector of (\eqn{beta_i,i+1, ..., beta_i,j-1, beta_i,j}) where beta_i,j = \eqn{P(\{Z_j \leq b_j\} \& \{\cap_{m=i+1}^{j-1} a_m \leq Z_m < b_m\} \mid Z_i = z_i)}.
#' @export
#'
#' @examples
#' library(gsDesign2)
#' library(dplyr)
#' library(mvtnorm)
#' # Example 1
#' # original design ----
#' enroll_rate <- define_enroll_rate(duration = c(2, 2, 2, 18),
#'                                   rate = c(1, 2, 3, 4))
#' fail_rate <- define_fail_rate(duration = c(3, Inf),
#'                               fail_rate = log(2) / 10,
#'                               dropout_rate = 0.001,
#'                               hr = c(1, 0.7))
#' x <- gs_design_ahr(enroll_rate = enroll_rate, fail_rate = fail_rate,
#'                    alpha = 0.025, beta = 0.1, ratio = 1,
#'                    info_frac = c(0.4, 0.6, 0.8, 1), analysis_time = 30,
#'                    binding = FALSE,
#'                    upper = gs_spending_bound,
#'                    upar = list(sf = sfLDOF, total_spend = 0.025, param = NULL),
#'                    lower = gs_spending_bound,
#'                    lpar = list(sf = sfLDOF, total_spend = 0.1),
#'                    h1_spending = TRUE,
#'                    test_lower = TRUE,
#'                    info_scale = "h0_h1_info") |> to_integer()
#'
#' # calculate conditional power
#' # case 1: currently at IA1, compute conditional power at IA2, IA3 and FA
#' gs_cp(x = x, i = 1, zi = -gsDesign::hrn2z(hr = 0.8, n = 150+180, ratio = 1))
#'
#' # case 2: currently at IA1, compute conditional power at IA2, IA3 and FA, with user-input theta
#' gs_cp(x = x, i = 1, zi = -gsDesign::hrn2z(hr = 0.8, n = 150+180, ratio = 1))
#'
gs_cp <- function(x = NULL, theta = NULL, i = 1, zi = NULL){
  # ------------------------------ #
  #        Input checking
  # ------------------------------ #
  # check x
  if(is.null(x)){
    stop("Please provide a design object of type gsDesign2 to calculate conditional power.")
  }

  # check theta
  if(!is.null(theta) && length(theta) != dim(x$analysis)[1] - i + 1){
    stop("Please provide the vector of theta from current analysis i to final analysis.")
  }
  # check i
  if(i > dim(x$analysis)[1]){
    stop("i cannot be larger than the number of analysis of design x.")
  }

  # check zi
  if (is.null(zi) || length(zi) != 1 || !is.finite(zi)) {
    stop("Please provide a finite scalar Z-score `zi` for analysis `i`.")
  }
  # ------------------------------ #a
  #        Initialization
  # ------------------------------ #

  # the conditional power is calculated from analysis i to analysis j
  # the analysis j is decided by the length of b (efficacy bound)
  # let D_m = B_m - B_i, where m = i+1, i+2, ..., j

  k_max <- dim(x$analysis)[1]
  n_future_analysis <- k_max - i
  prob_alpha <- rep(0, n_future_analysis)
  prob_alpha_plus <- rep(0, n_future_analysis)
  prob_beta <- rep(0, n_future_analysis)

  # futility bound from analysis i+1 to analysis j
  if("lower" %in% unique(x$bound$bound)){
    a <- x$bound$z[x$bound$bound == "lower"][(i+1):k_max]
  }else{
    a <- rep(-Inf, k_max)
  }

  # efficacy bound from analysis i+1 to analysis j
  if("upper" %in% unique(x$bound$bound)){
    b <- x$bound$z[x$bound$bound == "upper"][(i+1):k_max]
  }else{
    b <- rep(Inf, k_max)
  }

  # statistical information from analysis i to analysis j
  info <- x$analysis$info[i:k_max]
  # information fraction from analysis i to analysis j
  t <- x$analysis$info_frac[i:k_max]

  if(is.null(theta)){theta <- x$analysis$theta[i:k_max]}

  for(y in seq_len(n_future_analysis)){
    # y ranges from 1 to j-i, represents cases for alpha_{i,i+1}, ..., {alpha_i,j-1}, alpha_{i,j}
    # y is the increment from i

    # ------------------------------ #
    #       Build the asymptotic
    #         mean of B_k - B_i
    #       vector of length y
    # ------------------------------ #
    mu <- sapply(seq_len(y), function(k){
      idx <- k + 1 # i.e., start from i+1
      theta[idx] * sqrt(t[idx] * info[idx]) - theta[1] * sqrt(t[1] * info[1]) #first element of `theta` is the treatment effect of IA i.
    })

    # ---------------------------------- #
    #       Build the asymptotic
    #     covariance matrix of B_y - B_i
    # ---------------------------------- #
    cov_matrix <- matrix(0, nrow = y, ncol = y)

    for(k in seq_len(y)) {
      for(l in seq_len(y)) {
        cov_matrix[k, l] <- t[1 + min(k, l)] - t[1]
      }
    }

    # ----------------------------------------------------------------------------------------- #
    #             Calculate Lower/upper bounds for alpha
    # first cross efficacy bound at analysis y given no efficacy/futility bound crossing before
    # ----------------------------------------------------------------------------------------- #
    # Integration limits: D_m = B_m - B_i
    # for D_{i+1},...,D_{j-1} use [a, b); for D_j use [b_j, +Inf)
    # lower bound
    lower_alpha <- rep(0, y)
    if(y == 1){
      lower_alpha[y] <- b[y] * sqrt(t[y + 1]) - zi * sqrt(t[1])
    }else{
      for (m in 1:(y - 1)) {
        lower_alpha[m] <- a[m] * sqrt(t[m + 1]) - zi * sqrt(t[1])
      }
      lower_alpha[y] <- b[y] * sqrt(t[y + 1]) - zi * sqrt(t[1])
    }

    # upper bound
    upper_alpha <- rep(0, y)
    if(y == 1){
      upper_alpha[y] = Inf
    }else{
      for(m in 1:(y - 1)){
        upper_alpha[m] <- b[m] * sqrt(t[m + 1]) - zi * sqrt(t[1])
      }
      upper_alpha[y] <- Inf
    }

    # Compute multivariate normal probability
    prob_alpha[y] <- mvtnorm::pmvnorm(
      lower = lower_alpha,
      upper = upper_alpha,
      mean = mu,
      sigma = cov_matrix)[1]

    # --------------------------------------------------------------------------------- #
    #             Calculate Lower/upper bounds for alpha_plus
    # first cross efficacy bound at analysis y given no efficacy bound crossing before
    # --------------------------------------------------------------------------------- #
    # Integration limits: D_m = B_m - B_i
    # for D_{i+1},...,D_{j-1} use (-Inf, bm); for D_j use [b_j, +Inf)
    # lower bound
    lower_alpha_plus <- rep(0, y)
    if(y == 1){
      lower_alpha_plus[y] = b[y] * sqrt(t[y + 1]) - zi * sqrt(t[1])
    }else{
      lower_alpha_plus <- rep(-Inf, y - 1)
      lower_alpha_plus[y] <- b[y] * sqrt(t[y + 1]) - zi * sqrt(t[1])
    }

    # upper bound
    upper_alpha_plus <- rep(0, y)
    if(y == 1){
      upper_alpha_plus[y] <- Inf
    }else{
      for(m in 1:(y - 1)){
        upper_alpha_plus[m] <- b[m] * sqrt(t[m + 1]) - zi * sqrt(t[1])
      }
      upper_alpha_plus[y] <- Inf
    }

    prob_alpha_plus[y] <- mvtnorm::pmvnorm(
      lower = lower_alpha_plus,
      upper = upper_alpha_plus,
      mean = mu,
      sigma = cov_matrix)[1]

    # ---------------------------------------------------------------------------------------------- #
    #             Calculate Lower/upper bounds for beta
    # First crossing the futility bound at analysis y given no efficacy/futility bound crossing before
    # ---------------------------------------------------------------------------------------------- #
    # Integration limits: D_m = B_m - B_i
    # for D_{i+1},...,D_{j-1} use [a, b); for D_j use [-Inf, a_j)
    # lower bound
    lower_beta <- rep(0, y)
    if(y == 1){
      lower_beta[y] <- -Inf
    }else{
      for(m in 1:(y - 1)){
        lower_beta[m] <- a[m] * sqrt(t[m + 1]) - zi * sqrt(t[1])
      }
      lower_beta[y] <- -Inf
    }

    # upper bound
    upper_beta <- rep(0, y)
    if(y == 1){
      upper_beta[y] <- b[y] * sqrt(t[y + 1]) - zi * sqrt(t[1])
    }else{
      for(m in 1:y){
        upper_beta[m] <- a[m] * sqrt(t[m + 1]) - zi * sqrt(t[1])
      }
    }

    prob_beta[y] <- mvtnorm::pmvnorm(
      lower = lower_beta,
      upper = upper_beta,
      mean = mu,
      sigma = cov_matrix)[1]

  }

  return(list(prob_alpha = prob_alpha,
              prob_alpha_plus = prob_alpha_plus,
              prob_beta = prob_beta))

}
