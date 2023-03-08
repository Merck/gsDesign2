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
#' \donttest{
#' library(simtrial)
#' library(survival)
#' ahr_blinded(
#'   Srv = Surv(
#'     time = simtrial::Ex2delayedEffect$month,
#'     event = simtrial::Ex2delayedEffect$evntd
#'   ),
#'   intervals = c(4, 100),
#'   hr = c(1, .55),
#'   ratio = 1
#' )
#' }
#'
#' @export
ahr_blinded <- function(Srv = Surv(
                          time = simtrial::Ex1delayedEffect$month,
                          event = simtrial::Ex1delayedEffect$evntd
                        ),
                        intervals = array(3, 3),
                        hr = c(1, .6),
                        ratio = 1) {
  msg <- "hr must be a vector of positive numbers"
  if (!is.vector(hr, mode = "numeric")) stop(msg)
  if (min(hr) <= 0) stop(msg)

  events <- simtrial::pwexpfit(Srv, intervals)[, 3]
  nhr <- length(hr)
  nx <- length(events)
  # Add to hr if length shorter than intervals
  if (length(hr) < length(events)) hr <- c(hr, rep(hr[nhr], nx - nhr))

  # Compute blinded AHR
  theta <- sum(log(hr[1:nx]) * events) / sum(events)

  # Compute adjustment for information
  Qe <- ratio / (1 + ratio)

  ans <- tibble(
    Events = sum(events), AHR = exp(theta),
    theta = theta, info0 = sum(events) * (1 - Qe) * Qe
  )
  return(ans)
}
