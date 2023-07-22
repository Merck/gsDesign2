#  Copyright (c) 2022 Merck & Co., Inc., Rahway, NJ, USA and its affiliates. All rights reserved.
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

#' @importFrom dplyr last lag lead
#' @importFrom tibble tibble
#' @importFrom stats stepfun
NULL

#' Piecewise constant expected accrual
#'
#' \code{eAccrual()} computes the expected cumulative enrollment (accrual)
#' given a set of piecewise constant enrollment rates and times.
#' @param x times at which enrollment is to be computed.
#' @param enrollRates Piecewise constant enrollment rates expressed as a `tibble` with
#' `duration` for each piecewise constant period and the `rate` of enrollment for that period.
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input x is a vector of strictly increasing non-negative numeric elements.
#'    \item Validate if input enrollment rate is of type data.frame.
#'    \item Validate if input enrollment rate contains duration column.
#'    \item Validate if input enrollment rate contains rate column.
#'    \item Validate if rate in input enrollment rate is non-negative with at least one positive rate.
#'    \item Convert rates to step function.
#'    \item Add times where rates change to enrollment rates.
#'    \item Make a tibble of the input time points x, duration, enrollment rates at points, and
#'    expected accrual.
#'    \item Extract the expected cumulative or survival enrollment.
#'    \item Return \code{eAccrual}
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @return A vector with expected cumulative enrollment for the specified `times`.
#' @examples
#' # Example: default
#' eAccrual_()
#' @export
#'
eAccrual_ <- function(x = 0:24,
                     enrollRates=tibble::tibble(duration=c(3,3,18),
                                                rate=c(5,10,20)
                     )){
  # check input value
  # check input enrollment rate assumptions
  if(!is.numeric(x)){stop("gsDesign2: x in `eAccrual()` must be a strictly increasing non-negative numeric vector")}
  if(!min(x) >= 0){stop("gsDesign2: x in `eAccrual()` must be a strictly increasing non-negative numeric vector")}
  if(!min(lead(x,default=max(x)+1) - x) > 0){stop("gsDesign2: x in `eAccrual()` must be a strictly increasing non-negative numeric vector")}
  
  # check enrollment rate assumptions
  if(!is.data.frame(enrollRates)){stop("gsDesign2: enrollRates in `eAccrual()` must be a data frame")}
  if(!max(names(enrollRates)=="duration") == 1){stop("gsDesign2: enrollRates in `eAccrual()` column names must contain duration")}
  if(!max(names(enrollRates)=="rate") == 1){stop("gsDesign2: enrollRates in `eAccrual()` column names must contain rate")}
  
  # test that enrollment rates are non-negative with at least one positive
  if(!min(enrollRates$rate) >= 0){stop("gsDesign2: enrollRates in `eAccrual()` must be non-negative with at least one positive rate")}
  if(!max(enrollRates$rate) > 0){stop("gsDesign2: enrollRates in `eAccrual()` must be non-negative with at least one positive rate")}
  
  
  # convert rates to step function
  ratefn <- stepfun(x=cumsum(enrollRates$duration),
                    y=c(enrollRates$rate,0),
                    right=TRUE)
  # add times where rates change to enrollRates
  xvals <- sort(unique(c(x,cumsum(enrollRates$duration))))
  # make a tibble
  xx <- tibble::tibble(x=xvals,
                       duration= xvals - lag(xvals,default = 0),
                       rate=ratefn(xvals), # enrollment rates at points (right continuous)
                       eAccrual=cumsum(rate*duration) # expected accrual
  )
  # return survival or CDF
  ind <- !is.na(match(xx$x,x))
  return(as.numeric(xx$eAccrual[ind]))
}
