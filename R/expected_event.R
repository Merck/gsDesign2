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

#' Expected events observed under piecewise exponential model
#'
#' Computes expected events over time and by strata
#' under the assumption of piecewise constant enrollment rates and piecewise
#' exponential failure and censoring rates.
#' The piecewise exponential distribution allows a simple method to specify a distribution
#' and enrollment pattern
#' where the enrollment, failure and dropout rates changes over time.
#' While the main purpose may be to generate a trial that can be analyzed at a single point in time or
#' using group sequential methods, the routine can also be used to simulate an adaptive trial design.
#' The intent is to enable sample size calculations under non-proportional hazards assumptions
#' for stratified populations.
#'
#' @inheritParams ahr
#' @param total_duration Total follow-up from start of enrollment to data cutoff.
#' @param simple If default (`TRUE`), return numeric expected number of events,
#'   otherwise a data frame as described below.
#'
#' @return The default when `simple = TRUE` is to return the total expected
#'   number of events as a real number.
#'   Otherwise, when `simple = FALSE`, a data frame is returned with
#'   the following variables for each period specified in `fail_rate`:
#'   - `t`: start of period.
#'   - `fail_rate`: failure rate during the period.
#'   - `event`: expected events during the period.
#'
#'   The records in the returned data frame correspond to the input data frame `fail_rate`.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input enrollment rate contains total duration column.
#'    \item Validate if input enrollment rate contains rate column.
#'    \item Validate if input failure rate contains duration column.
#'    \item Validate if input failure rate contains failure rate column.
#'    \item Validate if input failure rate contains dropout rate column.
#'    \item Validate if input trial total follow-up (total duration) is a non-empty vector of positive integers.
#'    \item Validate if input simple is logical.
#'    \item Define a data frame with the start opening for enrollment at zero and cumulative duration.
#'    Add the event (or failure) time corresponding to the start of the enrollment.
#'    Finally, add the enrollment rate to the data frame
#'    corresponding to the start and end (failure) time.
#'    This will be recursively used to calculate the expected
#'    number of events later. For details, see vignette/eEventsTheory.Rmd
#'    \item Define a data frame including the cumulative duration of failure rates, the corresponding start time of
#'    the enrollment, failure rate and dropout rates.  For details, see vignette/eEventsTheory.Rmd
#'    \item Only consider the failure rates in the interval of the end failure rate and total duration.
#'    \item Compute the failure rates over time using \code{stepfun} which is used
#'     to group rows by periods defined by fail_rate.
#'    \item Compute the dropout rate over time using \code{stepfun}.
#'    \item Compute the enrollment rate over time using \code{stepfun}. Details are
#'    available in vignette/eEventsTheory.Rmd.
#'    \item Compute expected events by interval at risk using the notations and descriptions in
#'    vignette/eEventsTheory.Rmd.
#'    \item Return \code{expected_event}
#'  }
#' }
#'
#' @details
#' More periods will generally be supplied in output than those that are input.
#' The intent is to enable expected event calculations in a tidy format to
#' maximize flexibility for a variety of purposes.
#'
#' @export
#'
#' @examples
#' library(gsDesign2)
#'
#' # Default arguments, simple output (total event count only)
#' expected_event()
#'
#' # Event count by time period
#' expected_event(simple = FALSE)
#'
#' # Early cutoff
#' expected_event(total_duration = .5)
#'
#' # Single time period example
#' expected_event(
#'   enroll_rate = define_enroll_rate(duration = 10, rate = 10),
#'   fail_rate = define_fail_rate(duration = 100, fail_rate = log(2) / 6, dropout_rate = .01),
#'   total_duration = 22,
#'   simple = FALSE
#' )
#'
#' # Single time period example, multiple enrollment periods
#' expected_event(
#'   enroll_rate = define_enroll_rate(duration = c(5, 5), rate = c(10, 20)),
#'   fail_rate = define_fail_rate(duration = 100, fail_rate = log(2) / 6, dropout_rate = .01),
#'   total_duration = 22, simple = FALSE
#' )
#'
#' # Single time period example, multiple strata
#' enroll_rate <- define_enroll_rate(
#'   stratum = rep(c("Biomarker-positive", "Biomarker-negative"), each = 4),
#'   duration = c(2, 2, 2, 6, 2, 2, 2, 6),
#'   rate = c(1:4, 1:4))
#' failure_rate <- define_fail_rate(
#'   stratum = rep(c("Biomarker-positive", "Biomarker-negative"), each = 2),
#'   duration = c(3, 100, 3, 100),
#'   fail_rate = log(2) / c(8, 12, 8, 10),
#'   dropout_rate = 0.001)
#' # Number of expected events by stratum
#' sapply(c("Biomarker-positive", "Biomarker-negative"),
#'        function(ss){
#'          expected_event(enroll_rate = enroll_rate[enroll_rate$stratum == ss, ],
#'                         fail_rate = failure_rate[failure_rate$stratum == ss, ],
#'                         total_duration = 36,
#'                         simple = TRUE)})
expected_event <- function(
    enroll_rate = define_enroll_rate(
      duration = c(2, 2, 10),
      rate = c(3, 6, 9)
    ),
    fail_rate = define_fail_rate(
      duration = c(3, 100),
      fail_rate = log(2) / c(9, 18),
      dropout_rate = .001
    ),
    total_duration = 25,
    simple = TRUE) {

  # Report error where there is >=2 strata
  if(length(unique(fail_rate$stratum)) >= 2 || length(unique(enroll_rate$stratum)) >= 2) {stop("Please calculate the expected event by stratum, see examples. ")}

  # Compute breakpoints for the timeline ----
  # Enrollment breakpoints (time since start of enrollment)
  enroll_breaks <- c(0, cumsum(enroll_rate$duration))
  # Failure/dropout rate breakpoints (time on study)
  fail_breaks <- c(0, cumsum(fail_rate$duration))

  # Map enrollment start times to end_fail (time at risk)
  start_enroll_1 <- enroll_breaks
  end_fail_1 <- total_duration - start_enroll_1
  keep <- end_fail_1 > 0
  start_enroll_1 <- start_enroll_1[keep]
  end_fail_1 <- end_fail_1[keep]

  # Map failure breakpoints to start_enroll times
  end_fail_2 <- cumsum(fail_rate$duration)
  start_enroll_2 <- total_duration - end_fail_2
  temp_last <- end_fail_2[length(end_fail_2)]
  if (temp_last < total_duration) {
    end_fail_2 <- end_fail_2[-length(end_fail_2)]
    start_enroll_2 <- start_enroll_2[-length(start_enroll_2)]
  } else {
    keep2 <- start_enroll_2 > 0
    end_fail_2 <- end_fail_2[keep2]
    start_enroll_2 <- start_enroll_2[keep2]
  }

  # Union of all breakpoints (sorted by end_fail)
  all_end_fail <- sort(unique(c(end_fail_1, end_fail_2)))
  all_start_enroll <- total_duration - all_end_fail
  n_intervals <- length(all_end_fail)

  # Use fast step functions for rate lookups (right = FALSE means right-continuous)
  sf_enroll <- stepfun2(enroll_breaks, c(0, enroll_rate$rate, 0))
  sf_fail <- stepfun2(fail_breaks, c(0, fail_rate$fail_rate, fail_rate$fail_rate[nrow(fail_rate)]))
  sf_dropout <- stepfun2(fail_breaks, c(0, fail_rate$dropout_rate, fail_rate$dropout_rate[nrow(fail_rate)]))

  # Compute interval properties
  end_enroll <- c(total_duration, all_start_enroll[-n_intervals])
  start_fail <- c(0, all_end_fail[-n_intervals])
  duration <- end_enroll - all_start_enroll

  fail_rate_var <- sf_fail(start_fail)
  dropout_rate_var <- sf_dropout(start_fail)
  enroll_rate_var <- sf_enroll(all_start_enroll)

  # Compute q and big_q (survival through interval)
  q <- exp(-duration * (fail_rate_var + dropout_rate_var))
  big_q <- c(1, cumprod(q[-n_intervals]))

  # Compute g and big_g (accumulated enrollment, reversed order)
  g <- enroll_rate_var * duration
  # big_g needs cumulative sum in reverse order of start_fail
  rev_g <- rev(g)
  rev_big_g <- c(0, cumsum(rev_g[-n_intervals]))
  big_g <- rev(rev_big_g)

  # Compute expected events (nbar) per interval
  rate_sum <- fail_rate_var + dropout_rate_var
  nz <- fail_rate_var != 0  # non-zero failure rate mask
  d <- numeric(n_intervals)
  nbar <- numeric(n_intervals)
  d[nz] <- big_q[nz] * (1 - q[nz]) * fail_rate_var[nz] / rate_sum[nz]
  nbar[nz] <- big_g[nz] * d[nz] +
    (fail_rate_var[nz] * big_q[nz] * enroll_rate_var[nz]) / rate_sum[nz] *
    (duration[nz] - (1 - q[nz]) / rate_sum[nz])

  # Output results ----
  if (simple) {
    ans <- sum(nbar)
  } else {
    # Map each interval to its failure rate period start
    period_id <- fail_breaks[findInterval(start_fail, fail_breaks)]
    # Aggregate by period
    unique_periods <- unique(period_id)
    ans <- data.frame(
      t = unique_periods,
      fail_rate = sf_fail(unique_periods),
      event = vapply(unique_periods, function(p) sum(nbar[period_id == p]), numeric(1))
    )
    rownames(ans) <- NULL
  }
  return(ans)
}
