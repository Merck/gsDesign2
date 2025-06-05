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

#' Blinded estimation of average hazard ratio
#'
#' Based on blinded data and assumed hazard ratios in different intervals,
#' compute a blinded estimate of average hazard ratio (AHR) and corresponding
#' estimate of statistical information.
#' This function is intended for use in computing futility bounds based on
#' spending assuming the input hazard ratio (hr) values for intervals
#' specified here.
#'
#' @param surv Input survival object (see [survival::Surv()]);
#'   note that only 0 = censored, 1 = event for [survival::Surv()].
#' @param intervals Vector containing positive values indicating
#'   interval lengths where the exponential rates are assumed.
#'   Note that a final infinite interval is added if any events occur
#'   after the final interval specified.
#' @param hr Vector of hazard ratios assumed for each interval.
#' @param ratio Ratio of experimental to control randomization.
#'
#' @return A `tibble` with one row containing
#' - `ahr` - Blinded average hazard ratio based on assumed period-specific
#'   hazard ratios input in `fail_rate` and observed events in the
#'   corresponding intervals.
#' - `event` - Total observed number of events.
#' - `info0` - Information under related null hypothesis.
#' - `theta` - Natural parameter for group sequential design representing
#'   expected incremental drift at all analyses.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate input hr is a numeric vector.
#'    \item Validate input hr is non-negative.
#'    \item Validate input intervals is a numeric vector > 0.
#'    \item Set final value in intervals to Inf
#'    \item Validate that hr and intervals have same length.
#'    \item For input time-to-event data, count number of events in each input interval by stratum.
#'    \item Compute the blinded estimate of average hazard ratio.
#'    \item Compute adjustment for information.
#'    \item Return a tibble of the sum of events, average hazard ratio,
#'    blinded average hazard ratio, and the information.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @export
#'
#' @examples
#' ahr_blinded(
#'   surv = survival::Surv(
#'     time = simtrial::ex2_delayed_effect$month,
#'     event = simtrial::ex2_delayed_effect$evntd
#'   ),
#'   intervals = c(4, 100),
#'   hr = c(1, .55),
#'   ratio = 1
#' )
ahr_blinded <- function(
    surv = survival::Surv(
      time = simtrial::ex1_delayed_effect$month,
      event = simtrial::ex1_delayed_effect$evntd
    ),
    intervals = c(3, Inf),
    hr = c(1, .6),
    ratio = 1) {
  # Input checking
  if (!is.numeric(hr) || min(hr) <= 0) {
    stop("'hr' must be a vector of positive numbers.")
  }
  if (!is.numeric(intervals) || min(intervals) <= 0) {
    stop("'intervals' must be a vector of positive numbers.")
  }
  if (length(intervals) != length(hr)) {
    stop("The piecewise model specified 'hr' and 'intervals' differ in length.")
  }

  # Set final element of "intervals" to Inf
  intervals[length(intervals)] <- Inf

  # Fit the survival data into piecewise exponential model
  event <- simtrial::fit_pwexp(surv, intervals)[, 3]
  nhr <- length(hr)
  nx <- length(event)

  # Add to hr if length shorter than intervals
  if (length(hr) < length(event)) hr <- c(hr, rep(hr[nhr], nx - nhr))

  # Compute blinded AHR
  theta <- -sum(log(hr[1:nx]) * event) / sum(event)

  # Compute adjustment for information
  q_e <- ratio / (1 + ratio)

  ans <- tibble(
    event = sum(event),
    ahr = exp(-theta),
    theta = theta,
    info0 = sum(event) * (1 - q_e) * q_e
  )
  return(ans)
}
