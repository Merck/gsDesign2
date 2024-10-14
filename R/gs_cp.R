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

#' Conditional probability under non-proportional hazards for
#' group sequential design
#'
#' Computes conditional boundary crossing probabilities at future planned
#' analyses for a given group sequential design assuming an interim
#' z-statistic at a specified interim analysis.
#'
#' @param x An object of type `gs_design`.
#' @param i Analysis at which interim z-value is given; must be from 1 to
#'   `max(x$analysis$analysis)`.
#' @param zi Interim z-value at analysis `i` (scalar).
#'
#' @return A list containing:
#' - `theta` - A list containing `theta0` (`theta` under H0, i.e., 0)
#'   and `theta1` (a vector of `theta` under H1).
#' - `upper_bound` - The upper bound value return by any
#'   `gs_design_ahr()` functions.
#' - `upper_prob` - A list containing the conditional probability given
#'   `zi` under H0 and H1, respectively.
#'
#' @export
#'
#' @examples
#' x <- gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = 36)
#' gs_cp(x = x, i = 1, zi = 0, j = 2)
gs_cp <- function(x, i, zi, j){
  # input check
  # Check if 'x' has an 'analysis' element and it's a matrix or data frame
  if (!is.list(x) || !"analysis" %in% names(x) || (!is.matrix(x$analysis) && !is.data.frame(x$analysis))) {
    stop("'x' should be a list containing an 'analysis' matrix or data frame.")
  }
  # Check if 'i' and 'j' are numeric and within the appropriate range
  if (!is.numeric(i) || !is.numeric(j)) {
    stop("'i' and 'j' should be numeric.")
  }

  if (!(1 <= i && i < j && j <= dim(x$analysis)[1])) {
    stop("Please ensure that 1 <= i < j and j <= dim(x$analysis)[1].")
  }

  # Check the # of upper bound is equal to # of analysis
  if(length(which(x$bound$bound == "upper")) != dim(x$analysis)[1]){
    stop("'x' should contains the same number of upper bounds as the number of analysis")
  }

  # obtain necessary inforation from x
  t_frac <- x$analysis$info_frac
  h0 <- x$analysis$info0 # under local asymptotic, assume H0 ~= H1


  # default theta: under H0 and under H1
  theta0 <- c(0, 0, 0)
  theta <- x$analysis$theta


  # compute the conditional probability under H0
  eff_bound <- as.data.frame(x$bound) %>%
    filter(bound == "upper") %>%
    select(z)

  # compute conditional power under H0
  # prob0 <- 1 - pnorm((eff_bound[i+1, ] - sqrt(t_frac[i]/t_frac[i+1]) * zi)/sqrt((t_frac[i+1]-t_frac[i])/t_frac[i+1]))
  prob0 <- 1 - pnorm((sqrt(t_frac[j])*eff_bound[j, ] - sqrt(t_frac[i])*zi)/sqrt(t_frac[j] - t_frac[i]))


  # compute conditional power under H1
  #mu_star <- sqrt(H0[i+1])*theta[i+1] + sqrt(t_frac[i]/t_frac[i+1])*(zi - sqrt(H0[i])*theta[i])
  #sigma2_star <- H0[i+1]/H1[i+1] - (t_frac[i]/t_frac[i+1])*(H0[i]/H1[i])

  mu_star <- sqrt(t_frac[j])*sqrt(h0[j])*theta[j] - sqrt(t_frac[i])*sqrt(h0[i])*theta[i]
  sigma2_star <- t_frac[j] - t_frac[i]

  prob1 <- 1 - pnorm((sqrt(t_frac[j])*eff_bound[j, ] - sqrt(t_frac[i])*zi - mu_star)/sqrt(sigma2_star))

  # return list of results
  output <- list(theta = list(theta0 = 0, theta1 = theta),
                 upper_bound = eff_bound,
                 upper_prob = list(prob0 = prob0, prob1 = prob1)
  )

  return(output)
}
