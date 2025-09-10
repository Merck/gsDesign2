#  Copyright (c) 2025 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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
#' @param duration A numeric vector of time duration.
#' @param rate A numeric vector of event rate.
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
#' @export
#'
#' @examples
#'
#' # Plot a survival function with 2 different sets of time values
#' # to demonstrate plot precision corresponding to input parameters.
#'
#' x1 <- seq(0, 10, 10 / pi)
#' duration <- c(3, 3, 1)
#' rate <- c(.2, .1, .005)
#'
#' survival <- ppwe(
#'   x = x1,
#'   duration = duration,
#'   rate = rate
#' )
#' plot(x1, survival, type = "l", ylim = c(0, 1))
#'
#' x2 <- seq(0, 10, .25)
#' survival <- ppwe(
#'   x = x2,
#'   duration = duration,
#'   rate = rate
#' )
#' lines(x2, survival, col = 2)
ppwe <- function(x, duration, rate, lower_tail = FALSE) {
  # Check input enrollment rate assumptions
  check_non_negative(x)
  check_increasing(x, first = FALSE)

  H <- cumulative_rate(x, duration, rate, last_(rate)) # cumulative hazard
  survival <- exp(-H) # survival

  # return survival or CDF
  if (lower_tail) 1 - survival else survival
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
#' @export
#'
#' @examples
#' # Example: arbitrary numbers
#' s2pwe(1:9, (9:1) / 10)
#' # Example: lognormal
#' s2pwe(c(1:6, 9), plnorm(c(1:6, 9), meanlog = 0, sdlog = 2, lower.tail = FALSE))
s2pwe <- function(times, survival) {
  # Check input values
  check_positive(times)
  check_increasing(times)

  # Check that survival has same length as times
  if (length(survival) != length(times)) stop("`survival` must be of same length as `times`")

  # Check that survival is positive, non-increasing, less than or equal to 1 and gt 0
  check_positive(survival)
  if (any(diff(survival) > 0)) stop("`survival` must be non-increasing")
  if (survival[1] > 1) stop("`survival` must not be greater than 1")
  if (last_(survival) >= 1) stop("`survival` must have at least one value < 1")

  H <- -log(survival)
  tibble(duration = diff_one(times), rate = diff_one(H) / duration)
}
