#  Copyright (c) 2021 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co., Inc., Kenilworth, NJ, USA.
#
#  This file is part of the gsdmvn program.
#
#  gsdmvn is free software: you can redistribute it and/or modify
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
#' @param ... additional parameters transfered to `par$sf`.
#'
#' @examples
#'
#' # alpha-spending
#' par <- list(sf = gsDesign::sfLDOF, total_spend = 0.025)
#' gs_spending_combo(par, info = 1:3/3)
#'
#' # beta-spending
#' par <- list(sf = gsDesign::sfLDOF, total_spend = 0.2)
#' gs_spending_combo(par, info = 1:3/3)
#'
#' @export
gs_spending_combo <- function(par = NULL, info = NULL, ...){
  par$sf(par$total_spend, info, ...)$spend
}