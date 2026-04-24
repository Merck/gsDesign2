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

#' Conditional power computation with non-constant effect size
#'
#' @details
#' Suppose there are \eqn{K} analyses. Let \eqn{Z_i} and \eqn{Z_j} be the Z-statistics at a current analysis \eqn{i} and a future analysis \eqn{j}, respectively.
#' Let's denote the statistical information at the \eqn{i}-th analysis as \eqn{I_i}.
#' We further assume \eqn{Z_i} and \eqn{Z_j} are bivariate normal with standard group sequential assumptions on independent increments, then
#' \deqn{E(Z_i) = \theta_i\sqrt{I_i}}
#' \deqn{Var(Z_i) = 1/I_i}
#' \deqn{Cov(Z_i, Z_j) = t \equiv I_i/I_j}
#' See https://merck.github.io/gsDesign2/articles/story-npe-background.html for assumption details.
#' Returned value is
#' \deqn{P(Z_j > b_j \mid Z_i = z_i) = 1 - \Phi\left(\frac{b_j - \sqrt{t}z_i - \sqrt{I_j}(\theta_j - \theta_i\sqrt{t})}{\sqrt{1 - t}}\right)}
#' where \eqn{b_j} is the efficacy bound at analysis \eqn{j}, \eqn{z_i} is the observed Z-value at analysis \eqn{i},
#' and \eqn{\theta_i} natural parameter for treatment effect at analysis \eqn{i}.
#'
#' @param x An object of type gsDesign2.
#' @param theta Optional numeric vector of natural parameter for treatment effects from analysis \eqn{i} through final analysis \eqn{K}.
#'              If `NULL`, values are taken from `x$analysis$theta`.
#' @param i Integer index for current analysis. Default is 1.
#' @param zi Numeric scalar z-value observed at analysis \eqn{i}.
#' @return A numeric vector with the conditional power
#'         \eqn{P(Z_j > b_j \mid Z_i = z_i)} for \eqn{j = i+1, ..., K}.
#' @export
#'
#' @examples
#' library(gsDesign2)
#' library(gsDesign)
#' # Calculate conditional power with optional user-input theta (if NULL, will come from the input design)
#' alpha <- 0.025
#' beta <- 0.1
#' ratio <- 1
#' enroll_rate <- define_enroll_rate(
#'   duration = c(2, 2, 10),
#'   rate = (1:3) / 3)
#' fail_rate <- define_fail_rate(
#'   duration = Inf, fail_rate = log(2) / 9,
#'   hr = 0.6, dropout_rate = .0001)
#' analysis_time <- c(12, 24, 36)
#' ratio <- 1
#' upper <- gs_spending_bound
#' lower <- gs_b
#' upar <- list(sf = sfLDOF, total_spend = alpha)
#' lpar <- rep(-Inf, 3)
#' x <- gs_design_ahr(
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   alpha = alpha, beta = beta, ratio = ratio,
#'   info_scale = "h0_h1_info",
#'   info_frac = NULL,
#'   analysis_time = c(12, 24, 36),
#'   upper = upper, upar = upar, test_upper = TRUE,
#'   lower = lower, lpar = lpar, test_lower = FALSE,
#' ) |>
#'   to_integer()
#'
#' # theta is NULL case
#' gs_cp_simple(x = x, theta = NULL, i = 1, zi = 1.5)
#'
#' # User-input theta case
#' gs_cp_simple(x = x, theta = c(0.1, 0.2, 0.3), i = 1, zi = 1.5)
#'
gs_cp_simple <- function(x = NULL, theta = NULL, i = 1, zi = NULL) {
  # ----------------------------------------- #
  #       input checking                      #
  # ----------------------------------------- #

  # check x
  if(is.null(x)){
    stop("Please provide a design object of type gsDesign2 to calculate conditional power.")
  }

  if (!is.list(x) || is.null(x$analysis) || is.null(x$bound)) {
    stop("`x` must contain `analysis` and `bound` components.")
  }

  analysis <- x$analysis
  bound <- x$bound
  k_max <- nrow(analysis)

  if (is.null(k_max) || k_max < 1) {
    stop("`x$analysis` must have at least one analysis row.")
  }

  # check theta
  if(!is.null(theta) && length(theta) != k_max - i + 1){
    stop("Please provide the vector of theta from current analysis i to final analysis.")
  }

  # check i
  if (length(i) != 1 || !is.finite(i) || i != as.integer(i)) {
    stop("`i` must be a single finite integer.")
  }

  if (i < 1 || i > k_max) {
    stop("`i` must be between 1 and the max number of analyses in `x$analysis`.")
  }

  # check zi
  if (is.null(zi) || length(zi) != 1 || !is.finite(zi)) {
    stop("Please provide a finite scalar Z-score `zi` for analysis `i`.")
  }

  # ----------------------------------------- #
  #  Simple conditional power calculation     #
  # ----------------------------------------- #
  n_analysis <- k_max - i + 1 # number of future analysis
  conditional_power <- rep(0, max(n_analysis - 1, 1)) # if i = FA
  info <- analysis$info
  info0 <- analysis$info0
  b <- bound$z[bound$bound == "upper"]

  if(is.null(theta)){theta <- analysis$theta}

  # cp function under H0 (use info0)
  cp_func_h0 <- function(k){

    t <- info0[i]/info0[i + k]
    numerator1 <- b[i + k] - zi * sqrt(t)

    if(is.null(theta)){
      theta <- analysis$theta
      numerator2 <- theta[i + k] * sqrt(info0[i + k]) - theta[i] * sqrt(t * info0[i])
    }else{
      numerator2 <- theta[1 + k] * sqrt(info0[i + k]) - theta[1] * sqrt(t * info0[i]) # starting index of theta is 1, which is different from the above
    }

    denominator <- sqrt(1 - t)
    cp <- pnorm((numerator1 - numerator2)  / denominator, lower.tail = FALSE)

    return(cp)
  }

  # cp function under H1 (use info)
  cp_func_h1 <- function(k){

    t <- info[i]/info[i + k]
    numerator1 <- b[i + k] - zi * sqrt(t)


    if(is.null(theta)){
      theta <- analysis$theta
      numerator2 <- theta[i + k] * sqrt(info[i + k]) - theta[i] * sqrt(t * info[i])
    }else{
      numerator2 <- theta[1 + k] * sqrt(info[i + k]) - theta[1] * sqrt(t * info[i]) # starting index of theta is different from the above
    }

    denominator <- sqrt(1 - t)
    cp <- pnorm((numerator1 - numerator2)  / denominator, lower.tail = FALSE)

    return(cp)
  }

  # ----------------------------------------- #
  #                Output                     #
  # ----------------------------------------- #

  if(n_analysis == 1){  # i.e., i = last analysis of x

    conditional_power <- cp_func_h1(0)  # no difference of using cp_func_h1 or cp_func_h0

    }else{
      if(sum(theta) == 0){
        conditional_power <- vapply(1:(n_analysis - 1), cp_func_h0, numeric(1))  # cp under H0
      }else{
        conditional_power <- vapply(1:(n_analysis - 1), cp_func_h1, numeric(1))  # cp under H1
      }
    }

  return(conditional_power)

 }



