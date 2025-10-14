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
#' @param a A vector of length j-i, which specifies the futility bounds from analysis i+1 to analysis j.
#' @param b A vector of length j-i, which specifies the efficacy bounds from analysis i+1 to analysis j.
#' @param c Interim z-value at analysis i (scalar).
#' @return A list of conditional powers: prob_alpha is a vector of c(alpha_i,i+1, ..., alpha_i,j-1, alpha_i,j), where alpha_i,j = \eqn{P(\{Z_j \geq b_j\} \& \{\cap_{m=i+1}^{j-1} a_m \leq Z_m < b_m\} \mid Z_i = c)}.
#'                                       prob_alpha_plus is a vector of c(alpha^+_i,i+1, ..., alpha^+_i,j-1, alpha^+_i,j), where alpha^+_i,j = \eqn{P(\{Z_j \geq b_j\} \& \{\cap_{m=i+1}^{j-1} Z_m < b_m\} \mid Z_i = c)}.
#'                                       prob_beta is a vector of c(beta_i,i+1, ..., beta_i,j-1, beta_i,j) where beta_i,j = \eqn{P(\{Z_j \leq b_j\} \& \{\cap_{m=i+1}^{j-1} a_m \leq Z_m < b_m\} \mid Z_i = c)}.
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
#'                    info_frac = c(0.4, 0.6, 0.8, 1), analysis_time = 30,
#'                    binding = FALSE,
#'                    upper = "gs_spending_bound",
#'                    upar = list(sf = "sfLDOF", total_spend = 0.025, param = NULL),
#'                    lower = "gs_b",
#'                    lpar = c(-Inf, -Inf, -Inf),
#'                    h1_spending = TRUE,
#'                    test_lower = FALSE,
#'                    info_scale = "h0_h1_info") |> to_integer()
#'
#' # update design based on IA1 observed data ----
#' # assume at IA1, we observed 150 events (planned of 140) during the non-effect period, and 180 events (planned of 170) afterwards.
#' event_tbl <- data.frame(analysis = c(1, 1), event = c(150, 180))
#' ustime <- c(150+180, x$analysis$event[2:4]) / max(x$analysis$event)
#' xu <- gs_update_ahr(x, alpha = NULL, ustime = ustime, lstime = NULL, event_tbl = event_tbl)
#'
#' # calculate conditional power
#' # case 1: currently at IA1, compute conditional power at IA2
#' gs_cp_npe2(# IA1 and IA2's theta
#'            theta = x$analysis$theta[1:2],
#'            # IA1 and IA2's information fraction
#'            t = x$analysis$info_frac[1:2],
#'            # IA1 and IA2's statistical information
#'            info = x$analysis$info[1:2],
#'            # IA2's futility bound
#'            a = -Inf,
#'            # IA2's efficacy bound
#'            b = x$bound$z[x$bound$bound == "upper" & x$bound$analysis == 2],
#'            # IA1's Z-score
#'            c = -gsDesign::hrn2z(hr = 0.8, n = 150+180, ratio = 1))
#'
#' # case 2: currently at IA1, compute conditional power at IA2 and IA3
#' gs_cp_npe2(# IA1, IA2 and IA3's theta
#'            theta = x$analysis$theta[1:3],
#'            # IA1, IA2 and IA3's information fraction
#'            t = x$analysis$info_frac[1:3],
#'            # IA1, IA2, and IA3's statistical information
#'            info = x$analysis$info[1:3],
#'            # IA2 and IA3's futility bound
#'            a = c(-Inf, Inf),
#'            # IA2 and IA3's efficacy bound
#'            b = x$bound$z[x$bound$bound == "upper" & x$bound$analysis %in% c(2, 3)],
#'            # IA1's Z-score
#'            c = -gsDesign::hrn2z(hr = 0.8, n = 150+180, ratio = 1))
#'
#' # case 3: currently at IA1, compute conditional power at IA2, IA3 and FA
#' gs_cp_npe2(# IA1, IA2, IA3 and FA's theta
#'            theta = x$analysis$theta[1:3],
#'            # IA1, IA2, IA3 and FA's information fraction
#'            t = x$analysis$info_frac[1:3],
#'            # IA1, IA2, IA3 and FA's statistical information
#'            info = x$analysis$info[1:3],
#'            # IA2, IA3 and FA's futility bound
#'            a = c(-Inf, Inf),
#'            # IA2, IA3 and FA's efficacy bound
#'            b = x$bound$z[x$bound$bound == "upper" & x$bound$analysis %in% c(2, 3)],
#'            # IA1's Z-score
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

  if(length(theta) - length(b) != 1)
    stop("The length of theta should equal to length of b + 1. ")

  if(length(b) != length(a))
    stop("The length of b should equal to length of a. ")
  # ------------------------------ #
  #        Initialization
  # ------------------------------ #

  # the conditional power is calculated from analysis i to analysis j
  # the analysis j is decided by the length of b (efficacy bound)
  # let D_m = B_m - B_i, where m = i+1, i+2, ..., j
  dim <- length(b)  # = j-i
  prob_alpha <- rep(0, dim)
  prob_alpha_plus <- rep(0, dim)
  prob_beta <- rep(0, dim)

  for(x in 1:dim){
    # x ranges from 1 to j-i, represents cases for alpha_{i,i+1}, ..., {alpha_i,j-1}, alpha_{i,j}
    # x is the increment from i

    # ------------------------------ #
    #       Build the asymptotic
    #         mean of B_x - B_i
    #       vector of length x
    # ------------------------------ #

    mu <- sapply(seq_len(x), function(k){
      idx <- k + 1 # i.e., start from i+1
      theta[idx] * sqrt(t[idx] * info[idx]) - theta[1] * sqrt(t[1] * info[1]) #first element of `theta` is the treatment effect of IA i.
    })

    # ------------------------------ #
    #       Build the asymptotic
    #     covariance of B_x - B_i
    #        matrix of (x x x)
    # ------------------------------ #
    Sigma <- matrix(0, nrow = x, ncol = x)

    for(k in seq_len(x)) {
      for(l in seq_len(x)) {
        Sigma[k, l] <- t[1 + min(k, l)] - t[1]
      }
    }

    # ----------------------------------------------------------------------------------------- #
    #             Calculate Lower/upper bounds for alpha
    # first cross efficacy bound at analysis x given no efficacy/futility bound crossing before
    # ----------------------------------------------------------------------------------------- #
    # D_m = B_m - B_i
    # for D_{i+1},...,D_{j-1} use [a, b); for D_j use [b_j, +Inf)
    # integration lower bound
    lower_alpha <- rep(0, x)
    if(x == 1){
      lower_alpha[x] <- b[x] * sqrt(t[x + 1]) - c * sqrt(t[1])
    }else{
      for (m in 1:(x - 1)) {
        ## ?? here looks wrong: when m = 1, it is a[1]*sqrt(t[1]) - c*sqrt(t[i]) where i is always 1 -- which is wrong
        ##                      when m = 1, it is B2 - B1, which should be a[2]*sqrt(t[2]) - c*sqrt(t[i])
        ## corrected -  since 'a' is vector of length (j-i-1) that specifies futility bounds from analysis i+1 to analysis j-1.
        ## for example, j = 4 and i = 2, then 'a' contains futility bound at analysis #3, and t[1] here is the IF for analysis i
        ## 't' is a vector of length j-i+1, indicating the IF for analysis i to j.
        lower_alpha[m] <- a[m] * sqrt(t[m + 1]) - c * sqrt(t[1])
      }
      lower_alpha[x] <- b[x] * sqrt(t[x + 1]) - c * sqrt(t[1])
    }

    # integration upper bound
    # Note: should we consider the case where j = 4, i = 3, in this case, dim = 1, length of a is 0 --> we simply integrate from bj (the only element in b) to Inf
    upper_alpha <- rep(0, x)
    if(x == 1){
      upper_alpha[x] = Inf
    }else{
      for(m in 1:(x - 1)){
        ## ?? same as above
        upper_alpha[m] <- b[m] * sqrt(t[m + 1]) - c * sqrt(t[1])
      }
      upper_alpha[x] <- Inf
    }

    # Compute multivariate normal probability
    prob_alpha[x] <- mvtnorm::pmvnorm(
      lower = lower_alpha,
      upper = upper_alpha,
      mean = mu,
      sigma = Sigma)[1]

    # --------------------------------------------------------------------------------- #
    #             Calculate Lower/upper bounds for alpha_plus
    # first cross efficacy bound at analysis x given no efficacy bound crossing before
    # --------------------------------------------------------------------------------- #
    # Integration limits: D_m = B_m - B_i
    # for D_{i+1},...,D_{j-1} use (-Inf, bm); for D_j use [b_j, +Inf)
    # lower bound
    lower_alpha_plus <- rep(0, x)
    if(x == 1){
      lower_alpha_plus[x] = b[x] * sqrt(t[x + 1]) - c * sqrt(t[1])
    }else{
      lower_alpha_plus <- rep(-Inf, x - 1)
      lower_alpha_plus[x] <- b[x] * sqrt(t[x + 1]) - c * sqrt(t[1])
    }

    # upper bound
    upper_alpha_plus <- rep(0, x)
    if(x == 1){
      upper_alpha_plus[x] <- Inf
    }else{
      for(m in 1:(x - 1)){
        upper_alpha_plus[m] <- b[m] * sqrt(t[m + 1]) - c * sqrt(t[1])
      }
      upper_alpha_plus[x] <- Inf
    }


    prob_alpha_plus[x] <- mvtnorm::pmvnorm(
      lower = lower_alpha_plus,
      upper = upper_alpha_plus,
      mean = mu,
      sigma = Sigma)[1]

    # --------------------------------------------------------------------------------------- #
    #             Calculate Lower/upper bounds for beta
    # Not cross efficacy bound at analysis x given no efficacy/futility bound crossing before
    # --------------------------------------------------------------------------------------- #
    # Integration limits: D_m = B_m - B_i
    # for D_{i+1},...,D_{j-1} use [a, b); for D_j use [-Inf, b_j)
    # lower bound
    lower_beta <- rep(0, x)
    if(x == 1){
      lower_beta[x] <- -Inf
    }else{
      for(m in 1:(x - 1)){
        lower_beta[m] <- a[m] * sqrt(t[m + 1]) - c * sqrt(t[1])
      }
      lower_beta[x] <- -Inf
    }

    # upper bound
    upper_beta <- rep(0, x)
    if(x == 1){
      upper_beta[x] <- b[x] * sqrt(t[x + 1]) - c * sqrt(t[1])
    }else{
      for(m in 1:x){
        upper_beta[m] <- b[m] * sqrt(t[m + 1]) - c * sqrt(t[1])
      }
    }


    prob_beta[x] <- mvtnorm::pmvnorm(
      lower = lower_beta,
      upper = upper_beta,
      mean = mu,
      sigma = Sigma)[1]

  }


  return(prob = list(prob_alpha = prob_alpha,
                     prob_alpha_plus = prob_alpha_plus,
                     prob_beta = prob_beta))

}
