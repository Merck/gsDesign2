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

#' Weight Function of Weighted Log-rank Test
#'
#' * `wlr_weight_fh` is Fleming-Harriongton, FH(rho, gamma) weight function.
#' * `wlr_weight_1`  is constant for log rank test
#' * `wlr_weight_power` is Gehan-Breslow and Tarone-Ware weight function.
#'
#' @param x analysis time
#' @param arm0 an "arm" object defined in `npsurvSS` package
#' @param arm1 an "arm" object defined in `npsurvSS` package
#' @param rho A scalar parameter that controls the type of test
#' @param gamma A scalar parameter that controls the type of test
#' @param tau A scalar parameter of the cut-off time for modest weighted log rank test
#' @param power A scalar parameter that controls the power of the weight function
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Compute the sample size via the sum of arm sizes.
#'    \item Compute the proportion of size in the two arms.
#'    \item If the input tau is specified, define time up to the cut off time tau.
#'    \item Compute the CDF using the proportion of the size in the two arms and \code{npsruvSS::psurv()}.
#'    \item Return the Fleming-Harriongton weights for weighted Log-rank test.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @name wlr_weight

#' @rdname wlr_weight
#' @export
wlr_weight_fh <- function(x, arm0, arm1, rho = 0, gamma = 0, tau = NULL) {
  
  n   <- arm0$size + arm1$size
  p1 <- arm1$size / n
  p0 <- 1 - p1
  
  if(! is.null(tau)){
    # Define time up to cut-off time tau
    if(tau > 0){x <- pmin(x, tau)}
  }
  
  # CDF
  esurv <- p0 * npsurvSS::psurv(x, arm0) + p1 * npsurvSS::psurv(x, arm1)
  (1-esurv)^rho * esurv^gamma
  
}

#' @rdname wlr_weight
#' @export
wlr_weight_1 <- function(x, arm0, arm1){
  1
}

#' @rdname wlr_weight
#' @export
wlr_weight_n <- function(x, arm0, arm1, power = 1){
  
  n   <- arm0$size + arm1$size
  p1 <- arm1$size / n
  p0 <- 1 - p1
  tmax <- arm0$total_time
  
  (n * (p0 * prob_risk(arm0, x, tmax) + p1 * prob_risk(arm1, x,tmax)))^power
}
