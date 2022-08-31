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

#' @importFrom dplyr lag select "%>%"
#' @importFrom  tibble tibble
NULL

#' Approximate survival distribution with piecewise exponential distribution
#'
#' \code{s2pwe} converts a discrete set of points from an arbitrary survival distribution
#' to a piecewise exponential approximation
#' @param times Positive increasing times at which survival distribution is provided.
#' @param survival Survival (1 - cumulative distribution function) at specified `times`
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input times is increasing positive finite numbers.
#'    \item Validate if input survival is numeric and same length as input times.
#'    \item Validate if input survival is positive, non-increasing, less than or equal to 1 and greater than 0.
#'    \item Create a tibble of inputs times and survival.
#'    \item Calculate the duration, hazard and the rate.
#'    \item Return the duration and rate by \code{s2pwe}
#'  }
#'  }
#' \if{html}{The contents of this section are shown in PDF user manual only.}#' @return A `tibble` with `duration` and 'rate'
#' @return A tibble containing the duration and rate.
#' @examples
#' # Example: arbitrary numbers
#' s2pwe(1:9,(9:1)/10)
#' # Example: lognormal
#' s2pwe(c(1:6,9),plnorm(c(1:6,9),meanlog=0,sdlog=2,lower.tail=FALSE))
#' @export
s2pwe <- function(times, survival){
# check input values
  # check that times are positive, ordered, unique and finite numbers
  if(!is.numeric(times)){stop("gsDesign2: times in `s2pwe()` must be increasing positive finite numbers")}
  if(!min(times) > 0){stop("gsDesign2: times in `s2pwe()` must be increasing positive finite numbers")}
  if(!max(times) < Inf){stop("gsDesign2: times in `s2pwe()` must be increasing positive finite numbers")}
  len <- length(times)
  if(!if(len>1){min(times[2:len]-times[1:(len-1)]) > 0}){stop("gsDesign2: times in `s2pwe()`must be increasing positive finite numbers")}

  # check that survival is numeric and same length as times
  if(!is.numeric(survival)){stop("gsDesign2: survival in `s2pwe()` must be numeric and of same length as times")}
  if(!length(survival) == len){stop("gsDesign2: survival in `s2pwe()` must be numeric and of same length as times")}

  # check that survival is positive, non-increasing, less than or equal to 1 and gt 0
  if(!min(survival) > 0){stop("gsDesign2: survival in `s2pwe()` must be non-increasing positive finite numbers less than or equal to 1 with at least 1 value < 1")}
  if(!max(survival) <= 1){stop("gsDesign2: survival in `s2pwe()` must be non-increasing positive finite numbers less than or equal to 1 with at least 1 value < 1")}
  if(!min(survival) < 1){stop("gsDesign2: survival in `s2pwe()` must be non-increasing positive finite numbers less than or equal to 1 with at least 1 value < 1")}
  if(len>1){
  if(!min(survival[2:len]-survival[1:(len-1)]) <= 0 ){stop("gsDesign2: survival in `s2pwe()` must be non-increasing positive finite numbers less than or equal to 1 with at least 1 value < 1")}
  }

  xx <- tibble::tibble(Times=times, Survival = survival) %>%
          mutate(duration = Times - lag(Times, default = 0),
                 H = -log(Survival),
                 rate = (H-lag(H,default=0)) / duration
                 ) %>%
          select(duration,rate)
  return(xx)
}
