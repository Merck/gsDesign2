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

#' Blinded estimation of average hazard ratio
#'
#' Based on blinded data and assumed hazard ratios in different intervals, compute
#' a blinded estimate of average hazard ratio (AHR) and corresponding estimate of statistical information.
#' This function is intended for use in computing futility bounds based on spending assuming
#' the input hazard ratio (hr) values for intervals specified here.
#' @importFrom tibble tibble
#' @importFrom survival Surv
#' @param Srv input survival object (see \code{Surv}); note that only 0=censored, 1=event for \code{Surv}
#' @param intervals Vector containing positive values indicating interval lengths where the
#' exponential rates are assumed.
#' Note that a final infinite interval is added if any events occur after the final interval
#' specified.
#' @param hr vector of hazard ratios assumed for each interval
#' @param ratio ratio of experimental to control randomization.
#' 
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input hr is a numeric vector.
#'    \item Validate if input hr is non-negative.
#'    \item Simulate piece-wise exponential survival estimation with the inputs survival object Srv
#'    and intervals.
#'    \item Save the length of  hr and events to an object, and if the length of hr is shorter than
#'    the intervals, add replicates of the last element of hr and the corresponding numbers of events
#'    to hr.
#'    \item Compute the blinded estimation of average hazard ratio.
#'    \item Compute adjustment for information.
#'    \item Return a tibble of the sum of events, average hazard raito, blinded average hazard
#'    ratio, and the information.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @return A \code{tibble} with one row containing
#' `AHR` blinded average hazard ratio based on assumed period-specific hazard ratios input in `failRates`
#' and observed events in the corresponding intervals
#' `Events` total observed number of events, `info` statistical information based on Schoenfeld approximation,
#' and info0 (information under related null hypothesis) for each value of `totalDuration` input;
#' if `simple=FALSE`, `Stratum` and `t` (beginning of each constant HR period) are also returned
#' and `HR` is returned instead of `AHR`
#'
#' @examples
#' \dontrun{
#' library(simtrial)
#' library(survival)
#' ahr_blinded(Srv = Surv(time = simtrial::Ex2delayedEffect$month,
#'                        event = simtrial::Ex2delayedEffect$evntd),
#'             intervals = c(4, 100),
#'             hr = c(1, .55),
#'             ratio = 1)
#' }
#'
#' @export
ahr_blinded <- function (Srv = Surv(time = simtrial::Ex1delayedEffect$month,
                                    event = simtrial::Ex1delayedEffect$evntd),
                         intervals = array(3, 3),
                         hr = c(1, .6),
                         ratio = 1){   
  
  msg <- "hr must be a vector of positive numbers"
  if (!is.vector(hr, mode = "numeric")) stop(msg)
  if (min(hr) <= 0) stop(msg)
  
  events <- simtrial::pwexpfit(Srv, intervals)[ , 3]
  nhr <- length(hr)
  nx <- length(events)
  # Add to hr if length shorter than intervals
  if (length(hr) < length(events)) hr <- c(hr, rep(hr[nhr], nx - nhr))
  
  # Compute blinded AHR
  theta <- sum(log(hr[1 : nx]) * events) / sum(events)
  
  # Compute adjustment for information
  Qe <- ratio / (1 + ratio)
  
  ans <- tibble(Events = sum(events), AHR = exp(theta), 
                theta = theta, info0 = sum(events) * (1 - Qe) * Qe)
  return(ans)
}

#' @importFrom dplyr last
#' @importFrom tibble tibble
#' @importFrom stats stepfun
NULL

#' Piecewise exponential cumulative distribution function
#'
#' \code{ppwe} computes the cumulative distribution function (CDF) or survival rate
#' for a piecewise exponential distribution.
#' @param x times at which distribution is to be computed.
#' @param failRates Piecewise constant failure rates in `rate`,
#' `duration` for each piecewise constant failure rate period.
#' @param lower.tail Indicator of whether lower (TRUE) or upper tail (FALSE; default)
#' of CDF is to be computed.
#' @details
#' Suppose \eqn{\lambda_i} is the failure rate in the interval \eqn{(t_{i-1},t_i], i=1,2,\ldots,M} where
#' \eqn{0=t_0<t_i\ldots,t_M=\infty}. The cumulative hazard function at an arbitrary time \eqn{t>0} is then:
#'
#' \deqn{\Lambda(t)=\sum_{i=1}^M \delta(t\le t_i)(\min(t,t_i)-t_{i-1})\lambda_i.}
#' The survival at time \eqn{t} is then
#' \deqn{S(t)=\exp(-\Lambda(t)).}
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input enrollment rate is a strictly increasing non-negative numeric vector.
#'    \item Validate if input failure rate is of type data.frame.
#'    \item Validate if input failure rate contains duration column.
#'    \item Validate if input failure rate contains rate column.
#'    \item Validate if input lower.tail is logical.
#'    \item Convert rates to step function.
#'    \item Add times where rates change to enrollment rates.
#'    \item Make a tibble of the input time points x, duration, hazard rates at points,
#'    cumulative hazard and survival.
#'    \item Extract the expected cumulative or survival of piecewise exponential distribution.
#'    \item If input lower.tail is true, return the cdf, else return the survival for \code{ppwe}
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#' @return A vector with cumulative distribution function or survival values
#' @examples
#' # Example: default
#' ppwe(seq(0:10))
#' # Example: plot a survival function with 2 different sets of time values
#' # to demonstrate plot precision corresponding to input parameters.
#' fr <- tibble::tibble(duration=c(3,3,1),rate=c(.2,.1,.005))
#' Time <- seq(0,10,10/pi)
#' Survival <-  ppwe(Time,fr)
#' plot(Time,Survival,type="l",ylim=c(0,1))
#' Time <- seq(0,10,.25)
#' Survival <-  ppwe(Time,fr)
#' lines(Time,Survival,col=2)
#' @export
ppwe <- function(x = 0:20,
                 failRates = tibble::tibble(duration = c(3, 100), rate = log(2) / c(9, 18)),
                 lower.tail = FALSE
){
  # check input values
  # check input enrollment rate assumptions
  if(!is.numeric(x)){stop("gsDesign2: x in `ppwe()` must be a strictly increasing non-negative numeric vector")}
  if(!min(x) >= 0){stop("gsDesign2: x in `ppwe()` must be a strictly increasing non-negative numeric vector")}
  if(!min(x[x>0] - lag(x[x > 0], default = 0)) > 0){stop("gsDesign2: x in `ppwe()` must be a strictly increasing non-negative numeric vector")}
  
  # check input failure rate assumptions
  if(!is.data.frame(failRates)){stop("gsDesign2: failRates in `ppwe()` must be a data.frame")}
  if(!max(names(failRates) == "duration") == 1){stop("gsDesign2: failRates in `ppwe()` column names must contain duration")}
  if(!max(names(failRates) == "rate") == 1){stop("gsDesign2: failRates in `ppwe()` column names must contain rate")}
  
  # check lower.tail
  if(!is.logical(lower.tail)){stop("gsDesign2: lower.tail in `ppwe()` must be logical")}
  
  # convert rates to step function
  ratefn <- stepfun(x = cumsum(failRates$duration),
                    y = c(failRates$rate, last(failRates$rate)),
                    right = TRUE)
  # add times where rates change to failRates
  xvals <- sort(unique(c(x, cumsum(failRates$duration))))
  # make a tibble
  xx <- tibble::tibble(x = xvals,
                       duration = xvals - lag(xvals, default = 0),
                       h = ratefn(xvals), # hazard rates at points (right continuous)
                       H = cumsum(h * duration), # cumulative hazard
                       survival = exp(-H) # survival
  )
  # return survival or cdf
  ind <- !is.na(match(xx$x, x))
  survival <- as.numeric(xx$survival[ind])
  if(lower.tail){
    return(1 - survival)}else{
      return(survival)}
}

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
#' s2pwe(1:9, (9:1)/10)
#' # Example: lognormal
#' s2pwe(c(1:6,9), plnorm(c(1:6,9),meanlog = 0, sdlog = 2,lower.tail = FALSE))
#' @export
s2pwe <- function(times, survival){
  # check input values
  # check that times are positive, ordered, unique and finite numbers
  if(!is.numeric(times)){stop("gsDesign2: times in `s2pwe()` must be increasing positive finite numbers")}
  if(!min(times) > 0){stop("gsDesign2: times in `s2pwe()` must be increasing positive finite numbers")}
  if(!max(times) < Inf){stop("gsDesign2: times in `s2pwe()` must be increasing positive finite numbers")}
  len <- length(times)
  if(!if(len>1){min(times[2 : len] - times[1 : (len - 1)]) > 0}){stop("gsDesign2: times in `s2pwe()`must be increasing positive finite numbers")}
  
  # check that survival is numeric and same length as times
  if(!is.numeric(survival)){stop("gsDesign2: survival in `s2pwe()` must be numeric and of same length as times")}
  if(!length(survival) == len){stop("gsDesign2: survival in `s2pwe()` must be numeric and of same length as times")}
  
  # check that survival is positive, non-increasing, less than or equal to 1 and gt 0
  if(!min(survival) > 0){stop("gsDesign2: survival in `s2pwe()` must be non-increasing positive finite numbers less than or equal to 1 with at least 1 value < 1")}
  if(!max(survival) <= 1){stop("gsDesign2: survival in `s2pwe()` must be non-increasing positive finite numbers less than or equal to 1 with at least 1 value < 1")}
  if(!min(survival) < 1){stop("gsDesign2: survival in `s2pwe()` must be non-increasing positive finite numbers less than or equal to 1 with at least 1 value < 1")}
  if(len > 1){
    if(!min(survival[2 : len] - survival[1 : (len - 1)]) <= 0 ){
      stop("gsDesign2: survival in `s2pwe()` must be non-increasing positive finite numbers less than or equal to 1 with at least 1 value < 1")
    }
  }
  
  ans <- tibble::tibble(Times = times, Survival = survival) %>%
    mutate(duration = Times - lag(Times, default = 0),
           H = -log(Survival),
           rate = (H - lag(H,default = 0)) / duration) %>%
    select(duration,rate)
  return(ans)
}
