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
#' # Example 1 ----
#' # Calculate conditional power under arbitrary theta, info and lower/upper bound
#' # In practice, the value of theta and info commonly comes from a design.
#' # More examples are available at the pkgdown vignettes.
#' gs_cp_npe2(theta = c(0.1, 0.2, 0.3),
#'            t = c(0.15, 0.35, 0.6),
#'            info = c(15, 35, 60),
#'            a = c(-0.5),
#'            b = c(1.8, 2.1),
#'            c = 1.5)
#'
#' # Example 2
#' # original design ----
#' enroll_rate <- define_enroll_rate(duration = c(2, 2, 2, 18), rate = c(1, 2, 3, 4))
#' fail_rate <- define_fail_rate(duration = c(3, Inf), fail_rate = log(2) / 10, dropout_rate = 0.001, hr = c(1, 0.7))
#' x <- gs_design_ahr(enroll_rate = enroll_rate, fail_rate = fail_rate,
#'                    alpha = 0.025, beta = 0.1, ratio = 1,
#'                    info_frac = c(0.4, 0.8, 1), analysis_time = 30,
#'                    binding = FALSE,
#'                    upper = "gs_spending_bound",
#'                    upar = list(sf = "sfLDOF", total_spend = 0.025, param = NULL),
#'                    lower = "gs_b",
#'                    lpar = c(-Inf, -Inf, -Inf),
#'                    h1_spending = TRUE,
#'                    test_lower = FALSE,
#'                    info_scale = "h0_h1_info") |> to_integer()
#' # update design based on IA1 observed data ----
#' # assume at IA1, we observed 150 events (planned of 140) during the non-effect period, and 180 events (planned of 170) afterwards.
#' event_tbl <- data.frame(analysis = c(1, 1), event = c(150, 180))
#' ustime <- c(150+180, x$analysis$event[2:3]) / max(x$analysis$event)
#' xu <- gs_update_ahr(x, alpha = NULL, ustime = ustime, lstime = NULL, event_tbl = event_tbl)
#' # calculate conditional power at IA1 based on non-constant treatment effect ----
#' gs_cp_npe2(theta = x$analysis$theta,
#'            t = x$analysis$info_frac,
#'            info = x$analysis$info,
#'            a = rep(-Inf, 3),
#'            b = x$bound$z[x$bound$bound == "upper"],
#'            c = -gsDesign::hrn2z(hr = 0.8, n = 150+180, ratio = 1))
gs_cp_npe2 <- function(theta = NULL,
                       t = NULL,
                       info = NULL,
                       a = NULL,
                       b = NULL,
                       c = NULL){
  # ------------------------------ #
  #        Input checking
  # ------------------------------ #
  if(length(theta) != length(t))
    stop("The input of theta should have the same length of the input of t.")

  if(length(theta) != length(info))
    stop("The input of theta should have the same length of the input of info.")

  # ------------------------------ #
  #        Initialization
  # ------------------------------ #
  i <- 1
  # the conditional power is calculated from analysis i to analysis j
  # the analysis j is decided by the length of b (efficacy bound)
  j <- length(b)
  # let D_m = B_m - B_i, where m = i+1, i+2, ..., j
  dim <- j - i

  # ------------------------------ #
  #       Build the asymptotic
  #         mean of B_j - B_i
  #       vector of length dim
  # ------------------------------ #

  mu <- sapply(seq_len(dim), function(k){
    idx <- i + k
    theta[idx] * sqrt(t[idx] * info[idx]) - theta[i] * sqrt(t[i] * info[i])
  })

  # ------------------------------ #
  #       Build the asymptotic
  #     covariance of B_j - B_i
  #        matrix of (dim x dim)
  # ------------------------------ #
  Sigma <- matrix(0, nrow = dim, ncol = dim)

  for(k in seq_len(dim)) {
    for(l in seq_len(dim)) {
      Sigma[k, l] <- t[i + min(k, l)] - t[i]
    }
  }

  # ------------------------------ #
  #   Calculate integration limit
  #        for alpha(i,j)
  # ------------------------------ #
  # alpha(i,j) is the conditional probability of first cross efficacy bound
  # at analysis j given no efficacy/futility bound crossing between analysis i amd j
  # D_m = B_m - B_i, where m = i+1, i+2, ..., j
  # for D_{i+1},...,D_{j-1} use [a, b);
  # for D_j use [b_j, +Inf)
  # integration lower bound
  lower_alpha <- rep(0, j - i)
  for (m in 1:(j - i - 1)) {
    ## ?? here looks wrong: when m = 1, it is a[1]*sqrt(t[1]) - c*sqrt(t[i]) where i is always 1 -- which is wrong
    ##                      when m = 1, it is B2 - B1, which should be a[2]*sqrt(t[2]) - c*sqrt(t[i])
    lower_alpha[m] <- a[m] * sqrt(t[m]) - c * sqrt(t[i])
  }
  lower_alpha[j-i] <- b[j-i] * sqrt(t[j]) - c * sqrt(t[i])

  # integration upper bound
  upper_alpha <- rep(0, j - i)
  for(m in 1:(j - i - 1)){
    ## ?? same as above
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
    lower = lower_alpha_plus,
    upper = upper_alpha_plus,
    mean = mu,
    sigma = Sigma)[1]

  prob_beta <- mvtnorm::pmvnorm(
    lower = lower_beta,
    upper = upper_beta,
    mean = mu,
    sigma = Sigma)[1]


  return(prob = list(prob_alpha = prob_alpha,
                     prob_alpha_plus = prob_alpha_plus,
                     prob_beta = prob_beta))


}








