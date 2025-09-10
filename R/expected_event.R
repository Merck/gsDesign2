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

  # Divide the time line into sub-intervals ----

  ## by piecewise enrollment rates
  df_1 <- data.frame(start_enroll = c(0, cumsum(enroll_rate$duration)))
  df_1$end_fail <- total_duration - df_1$start_enroll
  df_1 <- df_1[df_1$end_fail > 0, ]

  ## by piecewise failure & dropout rates
  df_2 <- data.frame(
    end_fail = cumsum(fail_rate$duration),
    fail_rate_var = fail_rate$fail_rate,
    dropout_rate_var = fail_rate$dropout_rate
  )
  df_2$start_enroll <- total_duration - df_2$end_fail

  temp <- cumsum(fail_rate$duration)
  if (temp[length(temp)] < total_duration) {
    df_2 <- df_2[-nrow(df_2), ]
  } else {
    df_2 <- df_2[df_2$start_enroll > 0, ]
  }

  # Create 3 step functions (sf) ----
  # Step function to define enrollment rates over time
  sf_enroll_rate <- stats::stepfun(c(0, cumsum(enroll_rate$duration)),
    c(0, enroll_rate$rate, 0),
    right = FALSE
  )
  # step function to define failure rates over time
  start_fail <- c(0, cumsum(fail_rate$duration))
  fail_rate_last <- nrow(fail_rate)
  sf_fail_rate <- stats::stepfun(start_fail,
    c(0, fail_rate$fail_rate, fail_rate$fail_rate[fail_rate_last]),
    right = FALSE
  )
  # step function to define dropout rates over time
  sf_dropout_rate <- stats::stepfun(start_fail,
    c(0, fail_rate$dropout_rate, fail_rate$dropout_rate[fail_rate_last]),
    right = FALSE
  )

  # combine sub-intervals from enroll + failure + dropout #
  # impute the NA by step functions
  df <- merge(df_1, df_2, by = c("start_enroll", "end_fail"), all = TRUE, sort = FALSE)
  df <- df[order(df$end_fail), ]
  df$end_enroll <- fastlag(df$start_enroll, first = total_duration)
  df$start_fail <- fastlag(df$end_fail)
  df$duration <- df$end_enroll - df$start_enroll
  df$fail_rate_var <- sf_fail_rate(df$start_fail)
  df$dropout_rate_var <- sf_dropout_rate(df$start_fail)
  df$enroll_rate_var <- sf_enroll_rate(df$start_enroll)
  # create 2 auxiliary variable for failure & dropout rate
  # q: number of expected events in a sub-interval
  # big_q: cumulative product of q (pool all sub-intervals)
  df$q <- exp(-df$duration * (df$fail_rate_var + df$dropout_rate_var))
  df$big_q <- fastlag(cumprod(df$q), first = 1)
  df <- df[order(df$start_fail, decreasing = TRUE), ]
  # create another 2 auxiliary variable for enroll rate
  # g: number of expected subjects in a sub-interval
  # big_g: cumulative sum of g (pool all sub-intervals)
  df$g <- df$enroll_rate_var * df$duration
  df$big_g <- fastlag(cumsum(df$g))
  df <- df[order(df$start_fail), ]
  # compute expected events as nbar in a sub-interval
  df$d <- ifelse(
    df$fail_rate_var == 0,
    0,
    df$big_q * (1 - df$q) * df$fail_rate_var / (df$fail_rate_var + df$dropout_rate_var)
  )
  df$nbar <- ifelse(
    df$fail_rate_var == 0,
    0,
    df$big_g * df$d +
      (df$fail_rate_var * df$big_q * df$enroll_rate_var) /
        (df$fail_rate_var + df$dropout_rate_var) *
        (df$duration - (1 - df$q) / (df$fail_rate_var + df$dropout_rate_var))
  )

  # Output results ----
  if (simple) {
    ans <- sum(df$nbar)
  } else {
    sf_start_fail <- stats::stepfun(start_fail, c(0, start_fail), right = FALSE)
    ans <- data.frame(
      fail_rate = df$fail_rate_var,
      event = df$nbar,
      start_fail = sf_start_fail(df$start_fail)
    )
    ans <- lapply(split(ans, ~start_fail), function(s) {
      data.frame(t = s$start_fail[1], fail_rate = s$fail_rate[1], event = sum(s$event))
    })
    ans <- do.call(rbind, ans)
    row.names(ans) <- NULL
  }
  return(ans)
}
