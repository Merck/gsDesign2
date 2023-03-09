#  Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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

#' Piecewise exponential cumulative distribution function
#'
#' Computes the cumulative distribution function (CDF) or survival rate
#' for a piecewise exponential distribution.
#'
#' @param x Times at which distribution is to be computed.
#' @param fail_rate Piecewise constant failure rates in `rate`,
#'   `duration` for each piecewise constant failure rate period.
#' @param lower_tail Indicator of whether lower (`TRUE`) or upper tail
#'   (`FALSE`; default) of CDF is to be computed.
#'
#' @return A vector with cumulative distribution function or survival values.
#'
#' @details
#' Suppose \eqn{\lambda_i} is the failure rate in the interval
#' \eqn{(t_{i-1},t_i], i=1,2,\ldots,M} where
#' \eqn{0=t_0<t_i\ldots,t_M=\infty}.
#' The cumulative hazard function at an arbitrary time \eqn{t>0} is then:
#'
#' \deqn{\Lambda(t)=\sum_{i=1}^M \delta(t\leq t_i)(\min(t,t_i)-t_{i-1})\lambda_i.}
#' The survival at time \eqn{t} is then
#' \deqn{S(t)=\exp(-\Lambda(t)).}
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input enrollment rate is a strictly increasing non-negative numeric vector.
#'    \item Validate if input failure rate is of type data.frame.
#'    \item Validate if input failure rate contains duration column.
#'    \item Validate if input failure rate contains rate column.
#'    \item Validate if input lower_tail is logical.
#'    \item Convert rates to step function.
#'    \item Add times where rates change to enrollment rates.
#'    \item Make a tibble of the input time points x, duration, hazard rates at points,
#'    cumulative hazard and survival.
#'    \item Extract the expected cumulative or survival of piecewise exponential distribution.
#'    \item If input lower_tail is true, return the CDF, else return the survival for \code{ppwe}
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @importFrom dplyr last
#' @importFrom tibble tibble
#' @importFrom stats stepfun
#'
#' @export
#'
#' @examples
#' # Example: default
#' ppwe(seq(0:10))
#'
#' # Example: plot a survival function with 2 different sets of time values
#' # to demonstrate plot precision corresponding to input parameters.
#' fr <- tibble::tibble(duration = c(3, 3, 1), rate = c(.2, .1, .005))
#' Time <- seq(0, 10, 10 / pi)
#' Survival <- ppwe(Time, fr)
#' plot(Time, Survival, type = "l", ylim = c(0, 1))
#' Time <- seq(0, 10, .25)
#' Survival <- ppwe(Time, fr)
#' lines(Time, Survival, col = 2)
ppwe <- function(x = 0:20,
                 fail_rate = tibble::tibble(duration = c(3, 100), rate = log(2) / c(9, 18)),
                 lower_tail = FALSE) {
  # Check input values
  # Check input enrollment rate assumptions
  if (!is.numeric(x)) {
    stop("gsDesign2: x in `ppwe()` must be a strictly increasing non-negative numeric vector")
  }
  if (!min(x) >= 0) {
    stop("gsDesign2: x in `ppwe()` must be a strictly increasing non-negative numeric vector")
  }
  if (!min(x[x > 0] - lag(x[x > 0], default = 0)) > 0) {
    stop("gsDesign2: x in `ppwe()` must be a strictly increasing non-negative numeric vector")
  }

  # check input failure rate assumptions
  if (!is.data.frame(fail_rate)) {
    stop("gsDesign2: fail_rate in `ppwe()` must be a data.frame")
  }
  if (!max(names(fail_rate) == "duration") == 1) {
    stop("gsDesign2: fail_rate in `ppwe()` column names must contain duration")
  }
  if (!max(names(fail_rate) == "rate") == 1) {
    stop("gsDesign2: fail_rate in `ppwe()` column names must contain rate")
  }

  # Check lower_tail
  if (!is.logical(lower_tail)) {
    stop("gsDesign2: lower_tail in `ppwe()` must be logical")
  }

  # Convert rates to step function
  ratefn <- stepfun(
    x = cumsum(fail_rate$duration),
    y = c(fail_rate$rate, last(fail_rate$rate)),
    right = TRUE
  )
  # Add times where rates change to fail_rate
  xvals <- sort(unique(c(x, cumsum(fail_rate$duration))))
  # Make a tibble
  xx <- tibble::tibble(
    x = xvals,
    duration = xvals - lag(xvals, default = 0),
    h = ratefn(xvals), # hazard rates at points (right continuous)
    H = cumsum(h * duration), # cumulative hazard
    survival = exp(-H) # survival
  )
  # Return survival or CDF
  ind <- !is.na(match(xx$x, x))
  survival <- as.numeric(xx$survival[ind])
  if (lower_tail) {
    return(1 - survival)
  } else {
    return(survival)
  }
}

#' Approximate survival distribution with piecewise exponential distribution
#'
#' Converts a discrete set of points from an arbitrary survival distribution
#' to a piecewise exponential approximation.
#'
#' @param times Positive increasing times at which survival distribution is provided.
#' @param survival Survival (1 - cumulative distribution function) at specified `times`.
#'
#' @return A tibble containing the duration and rate.
#'
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
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @importFrom dplyr lag select "%>%"
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' # Example: arbitrary numbers
#' s2pwe(1:9, (9:1) / 10)
#' # Example: lognormal
#' s2pwe(c(1:6, 9), plnorm(c(1:6, 9), meanlog = 0, sdlog = 2, lower.tail = FALSE))
s2pwe <- function(times, survival) {
  # Check input values
  # Check that times are positive, ordered, unique and finite numbers
  if (!is.numeric(times)) {
    stop("gsDesign2: times in `s2pwe()` must be increasing positive finite numbers")
  }
  if (!min(times) > 0) {
    stop("gsDesign2: times in `s2pwe()` must be increasing positive finite numbers")
  }
  if (!max(times) < Inf) {
    stop("gsDesign2: times in `s2pwe()` must be increasing positive finite numbers")
  }
  len <- length(times)
  if (!if (len > 1) {
    min(times[2:len] - times[1:(len - 1)]) > 0
  }) {
    stop("gsDesign2: times in `s2pwe()`must be increasing positive finite numbers")
  }

  # Check that survival is numeric and same length as times
  if (!is.numeric(survival)) {
    stop("gsDesign2: survival in `s2pwe()` must be numeric and of same length as times")
  }
  if (!length(survival) == len) {
    stop("gsDesign2: survival in `s2pwe()` must be numeric and of same length as times")
  }

  # Check that survival is positive, non-increasing, less than or equal to 1 and gt 0
  if (!min(survival) > 0) {
    stop("gsDesign2: survival in `s2pwe()` must be non-increasing positive
         finite numbers less than or equal to 1 with at least 1 value < 1")
  }
  if (!max(survival) <= 1) {
    stop("gsDesign2: survival in `s2pwe()` must be non-increasing positive
         finite numbers less than or equal to 1 with at least 1 value < 1")
  }
  if (!min(survival) < 1) {
    stop("gsDesign2: survival in `s2pwe()` must be non-increasing positive
         finite numbers less than or equal to 1 with at least 1 value < 1")
  }
  if (len > 1) {
    if (!min(survival[2:len] - survival[1:(len - 1)]) <= 0) {
      stop("gsDesign2: survival in `s2pwe()` must be non-increasing positive
           finite numbers less than or equal to 1 with at least 1 value < 1")
    }
  }

  ans <- tibble::tibble(Times = times, Survival = survival) %>%
    mutate(
      duration = Times - lag(Times, default = 0),
      H = -log(Survival),
      rate = (H - lag(H, default = 0)) / duration
    ) %>%
    select(duration, rate)
  return(ans)
}
