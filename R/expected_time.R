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

#' Predict time at which a targeted event count is achieved
#'
#' `expected_time()` is made to match input format with [ahr()] and to solve for the
#' time at which the expected accumulated events is equal to an input target.
#' Enrollment and failure rate distributions are specified as follows.
#' The piecewise exponential distribution allows a simple method to specify a distribution
#' and enrollment pattern
#' where the enrollment, failure and dropout rates changes over time.
#'
#' @param enroll_rate Piecewise constant enrollment rates by stratum and time period.
#' @param fail_rate Piecewise constant control group failure rates,
#'   duration for each piecewise constant period,
#'   hazard ratio for experimental vs control,
#'   and dropout rates by stratum and time period.
#' @param target_event The targeted number of events to be achieved.
#' @param ratio Experimental:Control randomization ratio.
#' @param interval An interval that is presumed to include the time at which
#'   expected event count is equal to `target_event`.
#'
#' @return A tibble with `Time` (computed to match events in `target_event`),
#'   `AHR` (average hazard ratio), `Events` (`target_event` input),
#'   `info` (information under given scenarios), and `info0`
#'   (information under related null hypothesis) for each value of
#'   `total_duration` input.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Use root-finding routine with `AHR()` to find time at which targeted events accrue.
#'    \item Return a tibble with a single row with the output from `AHR()` got the specified output.
#'    }
#'  }
#'
#' @importFrom stats uniroot
#'
#' @export
#'
#' @examples
#' # ------------------------#
#' #      Example 1          #
#' # ------------------------#
#' # default
#' \donttest{
#' expected_time()
#' }
#'
#' # ------------------------#
#' #      Example 2          #
#' # ------------------------#
#' # check that result matches a finding using AHR()
#' # Start by deriving an expected event count
#' enroll_rate <- tibble::tibble(stratum = "All", duration = c(2, 2, 10), rate = c(3, 6, 9) * 5)
#' fail_rate <- tibble::tibble(
#'   stratum = "All", duration = c(3, 100), fail_rate = log(2) / c(9, 18),
#'   hr = c(.9, .6), dropout_rate = rep(.001, 2)
#' )
#' total_duration <- 20
#' xx <- ahr(enroll_rate, fail_rate, total_duration)
#' xx
#'
#' # Next we check that the function confirms the timing of the final analysis.
#' \donttest{
#' expected_time(enroll_rate, fail_rate,
#'   target_event = xx$event, interval = c(.5, 1.5) * xx$time
#' )
#' }
expected_time <- function(enroll_rate = tibble::tibble(
                            stratum = "All",
                            duration = c(2, 2, 10),
                            rate = c(3, 6, 9) * 5
                          ),
                          fail_rate = tibble::tibble(
                            stratum = "All",
                            duration = c(3, 100),
                            fail_rate = log(2) / c(9, 18),
                            hr = c(.9, .6),
                            dropout_rate = rep(.001, 2)
                          ),
                          target_event = 150,
                          ratio = 1,
                          interval = c(.01, 100)) {
  # ----------------------------#
  #    check inputs             #
  # ----------------------------#
  check_ratio(ratio)
  if (length(target_event) > 1) {
    stop("expected_time(): the input target_event` should be a positive numer, rather than a vector!")
  }

  # ----------------------------#
  #    build a help function    #
  # ----------------------------#
  # find the difference between  `AHR()` and different values of total_duration
  foo <- function(x) {
    ans <- ahr(
      enroll_rate = enroll_rate, fail_rate = fail_rate,
      total_duration = x, ratio = ratio
    )$event - target_event
    return(ans)
  }

  # ----------------------------#
  #       uniroot AHR()         #
  #    over total_duration      #
  # ----------------------------#
  res <- try(uniroot(foo, interval))

  if (inherits(res, "try-error")) {
    stop("expected_time(): solution not found!")
  } else {
    ans <- ahr(
      enroll_rate = enroll_rate, fail_rate = fail_rate,
      total_duration = res$root, ratio = ratio
    )
    return(ans)
  }
}
