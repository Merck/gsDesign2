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

#' @importFrom stats uniroot
NULL

#' Predict time at which a targeted event count is achieved
#'
#' \code{tEvents()} is made to match input format with \code{AHR()} and to solve for the
#' time at which the expected accumulated events is equal to an input target.
#' Enrollment and failure rate distributions are specified as follows.
#' The piecewise exponential distribution allows a simple method to specify a distribtuion
#' and enrollment pattern
#' where the enrollment, failure and dropout rates changes over time.
#' @param enrollRates Piecewise constant enrollment rates by stratum and time period.
#' @param failRates Piecewise constant control group failure rates, duration for each piecewise constant period,
#' hazard ratio for experimental vs control, and dropout rates by stratum and time period.
#' @param targetEvents The targeted number of events to be achieved.
#' @param ratio Experimental:Control randomization ratio.
#' @param interval An interval that is presumed to include the time at which
#' expected event count is equal to `targetEvents`.
#' 
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Use root-finding routine with `AHR()` to find time at which targeted events accrue.
#'    \item Return a tibble with a single row with the output from `AHR()` got the specified output.
#'    }
#'  }
#'  
#' @return A `tibble` with `Time` (computed to match events in `targetEvents`), `AHR` (average hazard ratio),
#' `Events` (`targetEvents` input), info (information under given scenarios),
#' and info0 (information under related null hypothesis) for each value of `totalDuration` input;
#' 
#' @examples
#' # ------------------------# 
#' #      Example 1          #
#' # ------------------------#
#' # default
#' tEvents()
#' 
#' # ------------------------# 
#' #      Example 2          #
#' # ------------------------#
#' # check that result matches a finding using AHR()
#' # Start by deriving an expected event count
#' enrollRates <- tibble::tibble(Stratum = "All", duration = c(2, 2, 10), rate = c(3, 6, 9) * 5)
#' failRates <- tibble::tibble(Stratum = "All", duration = c(3, 100), failRate = log(2) / c(9, 18), 
#'                             hr = c(.9,.6), dropoutRate = rep(.001, 2))
#' totalDuration <- 20
#' xx <- AHR(enrollRates, failRates, totalDuration)
#' xx
#' 
#' # Next we check that the function confirms the timing of the final analysis.
#' tEvents(enrollRates, failRates, 
#'         targetEvents = xx$Events, interval = c(.5, 1.5) * xx$Time)
#' 
#' @export
#'
tEvents <- function(enrollRates = tibble::tibble(Stratum = "All",
                                                 duration = c(2, 2, 10),
                                                 rate = c(3, 6, 9) * 5),
                    failRates = tibble::tibble(Stratum = "All",
                                               duration = c(3, 100),
                                               failRate = log(2) / c(9, 18),
                                               hr = c(.9, .6),
                                               dropoutRate = rep(.001, 2)),
                    targetEvents = 150,
                    ratio = 1,
                    interval = c(.01, 100)
){
  # ----------------------------#
  #    check inputs             #
  # ----------------------------#
  check_ratio(ratio)
  if(length(targetEvents) > 1){
    stop("tEvents(): the input targetEvents` should be a positive numer, rather than a vector!")
  }
  
  # ----------------------------#
  #    build a help function    #
  # ----------------------------#
  # find the difference between  `AHR()` and different values of totalDuration
  foo <- function(x){
    ans <- AHR(enrollRates = enrollRates, failRates = failRates, 
               totalDuration = x, ratio = ratio)$Events - targetEvents
    return(ans)
  }
  
  # ----------------------------#
  #       uniroot AHR()         #
  #    over totalDuration       #
  # ----------------------------#
  res <- try(uniroot(foo, interval))
  
  if(inherits(res,"try-error")){
    stop("tEvents(): solution not found!")
  }else{
    ans <- AHR(enrollRates = enrollRates, failRates = failRates, 
               totalDuration = res$root, ratio = ratio)
    return(ans)
  }
  
}