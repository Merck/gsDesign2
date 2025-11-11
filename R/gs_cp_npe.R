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
#' We assume \eqn{Z_i, i = 1, ..., K} are the z-values at an interim analysis i, respectively.
#' We assume further \eqn{Z_i, i = 1, ..., K} follows multivariate normal distribution
#' \deqn{E(Z_i) = \theta_i\sqrt{I_i}}
#' \deqn{Cov(Z_i, Z_j) = \sqrt{t_i/t_j}}
#' See https://merck.github.io/gsDesign2/articles/story-npe-background.html for assumption details.
#' For simple conditional power, the returned value is
#' \deqn{P(Z_j > b \mid Z_i = c) = 1 - \Phi\left(\frac{b - \sqrt{t}c - \sqrt{I_j}(\theta_j - \theta_i\sqrt{t})}{\sqrt{1 - t}}\right)}
#' For non-simple conditional power, the returned value is the list of
#' \deqn{P(\{Z_j \geq b_j\} \& \{\cap_{m=i+1}^{j-1} a_m \leq Z_m < b_m\} \mid Z_i = c)}.
#' \deqn{P(\{Z_j \geq b_j\} \& \{\cap_{m=i+1}^{j-1} Z_m < b_m\} \mid Z_i = c)}.
#' \deqn{P(\{Z_j \leq b_j\} \& \{\cap_{m=i+1}^{j-1} a_m \leq Z_m < b_m\} \mid Z_i = c)}.
#'
#' @param theta For simple conditional power, `theta` is a vector of length two, which specifies the natural parameter for treatment effect.
#'              The first element of `theta` is the treatment effect of an interim analysis i.
#'              The second element of `theta` is the treatment effect of a future analysis j.
#'
#'              For non-simple conditional power, `theta` is a vector of j-i+1, which specifies the natural parameter for treatment effect.
#'              The first element of `theta` is the treatment effect of an interim analysis i.
#'              The second element of `theta` is the treatment effect of an interim analysis i+1.
#'              ...
#'              The last element of `theta` is the treatment effect of a future analysis j.
#' @param t For non-simple conditional power, `t` is a vector of j-i+1, which specifies the information fraction under the treatment effect `theta`.
#' @param info For simple conditional power, `info` is a vector of length two, which specifies the statistical information under the treatment effect `theta`.
#'             For non-simple conditional power, `info` is a vector of j-i+1, which specifies the statistical information under the treatment effect `theta`.
#' @param a For non-simple conditional power, `a` is a vector of length j-i, which specifies the futility bounds from analysis i+1 to analysis j.
#' @param b For simple conditional power, `b` is a scalar which specifies the future target z-value at analysis j.
#'          For non-simple conditional power, `b` is a vector of length j-i, which specifies the efficacy bounds from analysis i+1 to analysis j.
#' @param c For both simple and non-simple conditional power, `c` is the interim z-value at analysis i (scalar).
#' @return For simple conditional power, the function returns a scalar with the conditional power \eqn{P(Z_2 > b\mid Z_1 = c)}.
#'         For non-simple conditional power, the function returns a list of conditional powers:
#'         prob_alpha is a vector of c(alpha_i,i+1, ..., alpha_i,j-1, alpha_i,j), where alpha_i,j = \eqn{P(\{Z_j \geq b_j\} \& \{\cap_{m=i+1}^{j-1} a_m \leq Z_m < b_m\} \mid Z_i = c)}.
#'                                       prob_alpha_plus is a vector of c(alpha^+_i,i+1, ..., alpha^+_i,j-1, alpha^+_i,j), where alpha^+_i,j = \eqn{P(\{Z_j \geq b_j\} \& \{\cap_{m=i+1}^{j-1} Z_m < b_m\} \mid Z_i = c)}.
#'                                       prob_beta is a vector of c(beta_i,i+1, ..., beta_i,j-1, beta_i,j) where beta_i,j = \eqn{P(\{Z_j \leq b_j\} \& \{\cap_{m=i+1}^{j-1} a_m \leq Z_m < b_m\} \mid Z_i = c)}.
#' @examples
#' library(gsDesign2)
#' library(dplyr)
#' library(mvtnorm)
#' # Example 1 ----
#' # Calculate conditional power under arbitrary theta, info and lower/upper bound
#' # In practice, the value of theta and info commonly comes from a design.
#' # More examples are available at the pkgdown vignettes.
#' gs_cp_npe(theta = c(0.1, 0.2, 0.3),
#'            t = c(0.15, 0.35, 0.6),
#'            info = c(15, 35, 60),
#'            a = c(-0.5, -0.5),
#'            b = c(1.8, 2.1),
#'            c = 1.5,
#'            simple_cp = TRUE)
gs_cp_npe <- function(theta = NULL,
                      t = NULL,
                      info = NULL,
                      a = NULL,
                      b = NULL,
                      c = NULL,
                      simple_cp = FALSE){

  # ----------------------------------------- #
  #       input checking                      #
  # ----------------------------------------- #
  if(simple_cp){

    # check theta
    if(is.null(theta) || any(is.na(theta))){
      stop("Please provide theta (arbitrary treatment effect) to calculate conditional power.")
    }else if(length(theta) == 1){
      theta <- rep(theta, 2)
    }else if(length(theta > 2)){
      theta <- theta[1:2]
      message("The first two elements of theta are used.")
    }
    # check info
    if(is.null(info) || any(is.na(info))){
      stop("Please provide info (statistical information given the treatment effect theta) to calculate conditional power.")
    }
    # check b
    if (is.null(b) || !is.numeric(b) || length(b) != 1 || any(is.na(b))){
      stop("Argument 'b' must be a numeric scalar and not NA.")
    }
    # Check c
    if (is.null(c) || !is.numeric(c) || length(c) != 1 || any(is.na(c))){
      stop("Argument 'c' must be a numeric scalar and not NA.")
    }
  }

  if(!simple_cp){

    # check theta
    if(is.null(theta) || any(is.na(theta))){
      stop("Please provide theta (arbitrary treatment effect) to calculate conditional power.")
    }
    # check t
    if(is.null(t) || any(is.na(t))){
      stop("Please provide t (information fraction) to calculate conditional power.")
    }
    # check info
    if(is.null(info) || any(is.na(info))){
      stop("Please provide info (statistical information given the treatment effect theta) to calculate conditional power.")
    }
    # check a
    if (is.null(a) || !is.numeric(a) || any(is.na(c))){
      stop("Argument 'a' must be numeric and not NA.")
    }
    # Check b
    if (is.null(b) || !is.numeric(b) || any(is.na(b))){
      stop("Argument 'b' must be numeric and not NA.")
    }
    # Check c
    if (is.null(c) || !is.numeric(c) || length(c) != 1 || any(is.na(c))){
      stop("Argument 'c' must be a numeric scalar and not NA.")
    }

  }


  # --------------------- #
  #  Call the cp function #
  # --------------------- #
  if(simple_cp){
    cp = gs_cp_npe1(theta = theta, info = info, b = b, c = c)
  }else{
    cp = gs_cp_npe2(theta = theta, t = t, info = info, a = a, b = b, c = c)
  }

  return(cp)

}




