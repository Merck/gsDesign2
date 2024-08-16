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

#' Derive spending bound for MaxCombo group sequential boundary
#'
#' @inheritParams gs_spending_bound
#'
#' @return A vector of the alpha spending per analysis.
#'
#' @export
#'
#' @examples
#' # alpha-spending
#' par <- list(sf = gsDesign::sfLDOF, total_spend = 0.025)
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' par <- list(sf = gsDesign::sfLDPocock, total_spend = 0.025)
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' par <- list(sf = gsDesign::sfHSD, total_spend = 0.025, param = -40)
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' # Kim-DeMets (power) Spending Function
#' par <- list(sf = gsDesign::sfPower, total_spend = 0.025, param = 1.5)
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' # Exponential Spending Function
#' par <- list(sf = gsDesign::sfExponential, total_spend = 0.025, param = 1)
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' # Two-parameter Spending Function Families
#' par <- list(sf = gsDesign::sfLogistic, total_spend = 0.025, param = c(.1, .4, .01, .1))
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' par <- list(sf = gsDesign::sfBetaDist, total_spend = 0.025, param = c(.1, .4, .01, .1))
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' par <- list(sf = gsDesign::sfCauchy, total_spend = 0.025, param = c(.1, .4, .01, .1))
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' par <- list(sf = gsDesign::sfExtremeValue, total_spend = 0.025, param = c(.1, .4, .01, .1))
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' par <- list(sf = gsDesign::sfExtremeValue2, total_spend = 0.025, param = c(.1, .4, .01, .1))
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' par <- list(sf = gsDesign::sfNormal, total_spend = 0.025, param = c(.1, .4, .01, .1))
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' # t-distribution Spending Function
#' par <- list(sf = gsDesign::sfTDist, total_spend = 0.025, param = c(-1, 1.5, 4))
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' # Piecewise Linear and Step Function Spending Functions
#' par <- list(sf = gsDesign::sfLinear, total_spend = 0.025, param = c(.2, .4, .05, .2))
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' par <- list(sf = gsDesign::sfStep, total_spend = 0.025, param = c(1 / 3, 2 / 3, .1, .1))
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' # Pointwise Spending Function
#' par <- list(sf = gsDesign::sfPoints, total_spend = 0.025, param = c(.25, .25))
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' # Truncated, trimmed and gapped spending functions
#' par <- list(sf = gsDesign::sfTruncated, total_spend = 0.025,
#'   param = list(trange = c(.2, .8), sf = gsDesign::sfHSD, param = 1))
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' par <- list(sf = gsDesign::sfTrimmed, total_spend = 0.025,
#'   param = list(trange = c(.2, .8), sf = gsDesign::sfHSD, param = 1))
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' par <- list(sf = gsDesign::sfGapped, total_spend = 0.025,
#'   param = list(trange = c(.2, .8), sf = gsDesign::sfHSD, param = 1))
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' # Xi and Gallo conditional error spending functions
#' par <- list(sf = gsDesign::sfXG1, total_spend = 0.025, param = 0.5)
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' par <- list(sf = gsDesign::sfXG2, total_spend = 0.025, param = 0.14)
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' par <- list(sf = gsDesign::sfXG3, total_spend = 0.025, param = 0.013)
#' gs_spending_combo(par, info = 1:3 / 3)
#'
#' # beta-spending
#' par <- list(sf = gsDesign::sfLDOF, total_spend = 0.2)
#' gs_spending_combo(par, info = 1:3 / 3)
gs_spending_combo <- function(par = NULL, info = NULL) {
  par$sf(alpha = par$total_spend, t = info, param = par$param)$spend
}
