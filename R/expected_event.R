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
#' @param enroll_rate Enrollment rates; see details and examples.
#' @param fail_rate Failure rates and dropout rates by period.
#' @param total_duration Total follow-up from start of enrollment to data cutoff.
#' @param simple If default (`TRUE`), return numeric expected number of events,
#'   otherwise a tibble as described below.
#'
#' @return The default when `simple = TRUE` is to return the total expected
#'   number of events as a real number.
#'   Otherwise, when `simple = FALSE`, a tibble is returned with
#'   the following variables for each period specified in `fail_rate`:
#'   - `t`: start of period.
#'   - `fail_rate`: failure rate during the period.
#'   - `Events`: expected events during the period.
#'
#'   The records in the returned tibble correspond to the input tibble `fail_rate`.
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
#'    \item Define a tibble with the start opening for enrollment at zero and cumulative duration.
#'    Add the event (or failure) time corresponding to the start of the enrollment.
#'    Finally, add the enrollment rate to the tibble
#'    corresponding to the start and end (failure) time.
#'    This will be recursively used to calculate the expected
#'    number of events later. For details, see vignette/eEventsTheory.Rmd
#'    \item Define a tibble including the cumulative duration of failure rates, the corresponding start time of
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
#' @importFrom dplyr select full_join mutate transmute
#' group_by summarize arrange desc lag last "%>%"
#' @importFrom tibble tibble
#' @importFrom stats stepfun
#'
#' @export
#'
#' @examples
#' library(tibble)
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
#'   enroll_rate = tibble(duration = 10, rate = 10),
#'   fail_rate = tibble(duration = 100, fail_rate = log(2) / 6, dropout_rate = .01),
#'   total_duration = 22,
#'   simple = FALSE
#' )
#'
#' # Single time period example, multiple enrollment periods
#' expected_event(
#'   enroll_rate = tibble(duration = c(5, 5), rate = c(10, 20)),
#'   fail_rate = tibble(duration = 100, fail_rate = log(2) / 6, dropout_rate = .01),
#'   total_duration = 22, simple = FALSE
#' )
expected_event <- function(enroll_rate = tibble::tibble(
                             duration = c(2, 2, 10),
                             rate = c(3, 6, 9)
                           ),
                           fail_rate = tibble::tibble(
                             duration = c(3, 100),
                             fail_rate = log(2) / c(9, 18),
                             dropout_rate = rep(.001, 2)
                           ),
                           total_duration = 25,
                           simple = TRUE) {
  # ----------------------------#
  #    check input values       #
  # ----------------------------#
  check_enroll_rate(enroll_rate)
  check_fail_rate(fail_rate)
  check_enroll_rate_fail_rate(enroll_rate, fail_rate)
  check_total_duration(total_duration)

  if (length(total_duration) > 1) {
    stop("gsDesign2: total_duration in `events_df()` must be a numeric number!")
  }
  if (!is.logical(simple)) {
    stop("gsDesign2: simple in `expected_event()` must be logical!")
  }

  # ----------------------------#
  #    divide the time line     #
  #     into sub-intervals      #
  # ----------------------------#
  ## by piecewise enrollment rates
  df_1 <- tibble::tibble(
    start_enroll = c(0, cumsum(enroll_rate$duration)),
    end_fail = total_duration - start_enroll
  ) %>%
    subset(end_fail > 0)

  ## by piecewise failure & dropout rates
  df_2 <- tibble::tibble(
    end_fail = cumsum(fail_rate$duration),
    start_enroll = total_duration - end_fail,
    fail_rate_var = fail_rate$fail_rate,
    dropout_rate_var = fail_rate$dropout_rate
  )

  temp <- cumsum(fail_rate$duration)
  if (temp[length(temp)] < total_duration) {
    df_2 <- df_2[-nrow(df_2), ]
  } else {
    df_2 <- df_2[df_2$start_enroll > 0, ]
  }

  # ----------------------------#
  # create 3 step functions (sf)#
  # ----------------------------#
  # Step function to define enrollment rates over time
  sf_enroll_rate <- stepfun(c(0, cumsum(enroll_rate$duration)),
    c(0, enroll_rate$rate, 0),
    right = FALSE
  )
  # step function to define failure rates over time
  start_fail <- c(0, cumsum(fail_rate$duration))
  sf_fail_rate <- stepfun(start_fail,
    c(0, fail_rate$fail_rate, last(fail_rate$fail_rate)),
    right = FALSE
  )
  # step function to define dropout rates over time
  sf_dropout_rate <- stepfun(start_fail,
    c(0, fail_rate$dropout_rate, last(fail_rate$dropout_rate)),
    right = FALSE
  )

  # combine sub-intervals from enroll + failure + dropout #
  # impute the NA by step functions
  df <- full_join(df_1, df_2, by = c("start_enroll", "end_fail")) %>%
    arrange(end_fail) %>%
    mutate(
      end_enroll = lag(start_enroll, default = as.numeric(total_duration)),
      start_fail = lag(end_fail, default = 0),
      duration = end_enroll - start_enroll,
      fail_rate_var = sf_fail_rate(start_fail),
      dropout_rate_var = sf_dropout_rate(start_fail),
      enroll_rate_var = sf_enroll_rate(start_enroll)
    ) %>%
    # create 2 auxiliary variable for failure & dropout rate
    # q: number of expected events in a sub-interval
    # big_q: cumulative product of q (pool all sub-intervals)
    mutate(
      q = exp(-duration * (fail_rate_var + dropout_rate_var)),
      big_q = lag(cumprod(q), default = 1)
    ) %>%
    arrange(desc(start_fail)) %>%
    # create another 2 auxiliary variable for enroll rate
    # g: number of expected subjects in a sub-interval
    # big_g: cumulative sum of g (pool all sub-intervals)
    mutate(
      g = enroll_rate_var * duration,
      big_g = lag(cumsum(g), default = 0)
    ) %>%
    arrange(start_fail) %>%
    # compute expected events as nbar in a sub-interval
    mutate(
      d = ifelse(fail_rate_var == 0, 0, big_q * (1 - q) * fail_rate_var / (fail_rate_var + dropout_rate_var)),
      nbar = ifelse(fail_rate_var == 0,
        0,
        big_g * d +
          (fail_rate_var * big_q * enroll_rate_var) /
            (fail_rate_var + dropout_rate_var) *
            (duration - (1 - q) /
              (fail_rate_var + dropout_rate_var))
      )
    )

  # ----------------------------#
  #       output results        #
  # ----------------------------#
  if (simple) {
    ans <- as.numeric(sum(df$nbar))
  } else {
    sf_start_fail <- stepfun(start_fail, c(0, start_fail), right = FALSE)
    ans <- df %>%
      transmute(
        t = end_fail,
        fail_rate = fail_rate_var,
        event = nbar,
        start_fail = sf_start_fail(start_fail)
      ) %>%
      group_by(start_fail) %>%
      summarize(
        fail_rate = first(fail_rate),
        event = sum(event)
      ) %>%
      mutate(t = start_fail) %>%
      select(t, fail_rate, event)
  }
  return(ans)
}
