#  Copyright (c) 2021 Merck Sharp & Dohme Corp., a subsidiary of
#  Merck & Co., Inc., Kenilworth, NJ, USA.
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
#' 
#' @param x times at which enrollment is to be computed.
#' @param enrollRates Piecewise constant enrollment rates expressed as a `tibble` with
#' `duration` for each piecewise constant period and the `rate` of enrollment for that period.
#' 
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
#' 
#' @examples
#' library(tibble)
#' 
#' # Example 1: default
#' eAccrual()
#' 
#' # Example 2: unstratified design
#' eAccrual(x = c(5, 10, 20), 
#'          enrollRates = tibble(duration = c(3, 3, 18), rate = c(5, 10, 20)))
#' 
#' eAccrual(x = c(5, 10, 20), 
#'          enrollRates = tibble(duration = c(3, 3, 18), rate = c(5, 10, 20), 
#'          Stratum = "All"))
#'          
#' # Example 3: stratified design
#' eAccrual(x = c(24, 30, 40), 
#'          enrollRates = tibble(Stratum=c("subgroup", "complement"), 
#'                               duration = 33, 
#'                               rate = c(30, 30)))
#' 
#' @export
#'
eAccrual <- function(x = 0:24,
                     enrollRates = tibble(duration = c(3, 3, 18), rate = c(5, 10, 20))){
  # check input value
  # check input enrollment rate assumptions
  if(!is.numeric(x)){stop("gsDesign2: x in `eAccrual()` must be a strictly increasing non-negative numeric vector!")}
  if(!min(x) >= 0){stop("gsDesign2: x in `eAccrual()` must be a strictly increasing non-negative numeric vector!")}
  if(!min(lead(x, default = max(x) + 1) - x) > 0){stop("gsDesign2: x in `eAccrual()` must be a strictly increasing non-negative numeric vector!")}
  
  # check enrollment rate assumptions
  check_enrollRates(enrollRates)
  
  # check if it is stratified design
  if("Stratum" %in% names(enrollRates)){
    n_strata <- length(unique(enrollRates$Stratum))
  }else{
    n_strata <- 1
  }
  
  # convert rates to step function
  if(n_strata == 1){
    ratefn <- stepfun(x = cumsum(enrollRates$duration),
                      y = c(enrollRates$rate, 0),
                      right = TRUE)
  }else{
    ratefn <- lapply(unique(enrollRates$Stratum), 
                     FUN = function(s){
                       stepfun(x = cumsum((enrollRates %>% filter(Stratum == s))$duration),
                               y = c((enrollRates %>% filter(Stratum == s))$rate, 0),
                               right = TRUE)
                     })
  }

  # add times where rates change to enrollRates
  if(n_strata == 1){
    xvals <- sort(unique(c(x, cumsum(enrollRates$duration))))
  }else{
    xvals <- lapply(unique(enrollRates$Stratum), 
                    FUN = function(s){
                      sort(unique(c(x, cumsum((enrollRates %>% filter(Stratum == s))$duration))))
                    })
  }
  
  # make a tibble
  if(n_strata == 1){
    xx <- tibble(x = xvals,
                 duration = xvals - lag(xvals, default = 0),
                 rate = ratefn(xvals),                        # enrollment rates at points (right continuous)
                 eAccrual = cumsum(rate * duration)           # expected accrual
    )
  }else{
    xx <- lapply(1:n_strata, 
               FUN = function(i){
                 tibble(x = xvals[[i]],
                        duration = xvals[[i]] - lag(xvals[[i]], default = 0),
                        rate = ratefn[[i]](xvals[[i]]),       # enrollment rates at points (right continuous)
                        eAccrual = cumsum(rate * duration)    # expected accrual
                 )
               })
  }
  
  

  # return survival or cdf
  if(n_strata == 1){
    ind <- !is.na(match(xx$x, x))
    ans <- as.numeric(xx$eAccrual[ind])
  }else{
    ind <- lapply(1:n_strata, 
                  FUN = function(i){
                    !is.na(match(xx[[i]]$x, x))
                  })
    ans <- lapply(1:n_strata, 
                  FUN = function(i){
                    as.numeric(xx[[i]]$eAccrual[ind[[i]]])
                  })
    ans <- do.call("+", ans)
  }
  
  return(ans)
}