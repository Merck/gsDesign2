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

#' @title Conditional Probability under NPH setting
#' @description  \code{gs_cp()} computes conditional boundary crossing probabilities at future
#' planned analyses for a given group sequential design assuming an interim
#' z-statistic at a specified interim analysis.
#' @param x An object of type \code{gsDesign} or \code{gsProbability}
#' @param theta a vector with \eqn{\theta}{theta} value(s) at which conditional
#' power is to be computed; for \code{gsCP()} if \code{NULL}, an estimated
#' value of \eqn{\theta}{theta} based on the interim test statistic
#' (\code{zi/sqrt(x$n.I[i])}) as well as at \code{x$theta} is computed. For
#' \code{gsPosterior}, this may be a scalar or an interval; for \code{gsPP} and
#' \code{gsCP}, this must be a scalar.
#' @param i analysis at which interim z-value is given; must be from 1 to \code{x$k-1}
#' @param zi interim z-value at analysis i (scalar)


#library(dplyr)
#x <- gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = 36)

# current gs_cp() only consider conditioning on one previous stage

# ------------------
#   gs_cp()
# ------------------

gs_cp <- function(x, i = 1, zi = 0){

  t_frac <- x$analysis$info_frac
  H0 <- x$analysis$info0
  H1 <- x$analysis$info


  # default theta: under H0 and under H1
  theta0 <- c(0, 0, 0)
  theta <- x$analysis$theta


  # compute the conditional probability under H0
  eff_bound <- as.data.frame(x$bound) %>%
    filter(bound == "upper") %>%
    select(z)

  # compute conditional power under H0
  prob0 <- 1 - pnorm((eff_bound[i+1, ] - sqrt(t_frac[i]/t_frac[i+1]) * zi)/sqrt((t_frac[i+1]-t_frac[i])/t_frac[i+1]))


  # compute conditional power under H1
  mu_star <- sqrt(H0[i+1])*theta[i+1] + sqrt(t_frac[i]/t_frac[i+1])*(zi - sqrt(H0[i])*theta[i])
  sigma2_star <- H0[i+1]/H1[i+1] - (t_frac[i]/t_frac[i+1])*(H0[i]/H1[i])

  prob1 <- 1 - pnorm((eff_bound[i+1, ])/sqrt(sigma2_star))

  # return list of results
  output <- list(theta = list(theta0 = 0, theta1 = theta),
                 upper_bound = cbind(eff_bound0, eff_bound),
                 upper_prob =
  )
}