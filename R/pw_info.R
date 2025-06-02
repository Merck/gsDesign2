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

#' Average hazard ratio under non-proportional hazards
#'
#' Provides a geometric average hazard ratio under
#' various non-proportional hazards assumptions for either single or multiple strata studies.
#' The piecewise exponential distribution allows a simple method to specify a distribution
#' and enrollment pattern where the enrollment, failure and dropout rates changes over time.
#'
#' @param enroll_rate An `enroll_rate` data frame with or without stratum
#'   created by [define_enroll_rate()].
#' @param fail_rate A `fail_rate` data frame with or without stratum
#'   created by [define_fail_rate()].
#' @param total_duration Total follow-up from start of enrollment to data
#'   cutoff; this can be a single value or a vector of positive numbers.
#' @param ratio Ratio of experimental to control randomization.
#'
#' @return A data frame with `time` (from `total_duration`), `stratum`, `t`,
#'   `hr` (hazard ratio), `event` (expected number of events), `info`
#'   (information under given scenarios), `info0` (information under related
#'   null hypothesis), and `n` (sample size) for each value of `total_duration`
#'   input
#'
#' @export
#'
#' @examples
#' # Example: default
#' pw_info()
#'
#' # Example: default with multiple analysis times (varying total_duration)
#' pw_info(total_duration = c(15, 30))
#'
#' # Stratified population
#' enroll_rate <- define_enroll_rate(
#'   stratum = c(rep("Low", 2), rep("High", 3)),
#'   duration = c(2, 10, 4, 4, 8),
#'   rate = c(5, 10, 0, 3, 6)
#' )
#' fail_rate <- define_fail_rate(
#'   stratum = c(rep("Low", 2), rep("High", 2)),
#'   duration = c(1, Inf, 1, Inf),
#'   fail_rate = c(.1, .2, .3, .4),
#'   dropout_rate = .001,
#'   hr = c(.9, .75, .8, .6)
#' )
#' # Give results by change-points in the piecewise model
#' ahr(enroll_rate = enroll_rate, fail_rate = fail_rate, total_duration = c(15, 30))
#'
#' # Same example, give results by strata and time period
#' pw_info(enroll_rate = enroll_rate, fail_rate = fail_rate, total_duration = c(15, 30))
pw_info <- function(
    enroll_rate = define_enroll_rate(
      duration = c(2, 2, 10),
      rate = c(3, 6, 9)
    ),
    fail_rate = define_fail_rate(
      duration = c(3, 100),
      fail_rate = log(2) / c(9, 18),
      hr = c(.9, .6),
      dropout_rate = .001
    ),
    total_duration = 30,
    ratio = 1) {

  # -------------------------------------- #
  #   compute expected accrual over time   #
  # -------------------------------------- #
  # calculate the sample size accrual during the interval
  tbl_n <- list()
  strata <- unique(enroll_rate$stratum)

  for (s in strata) {
    # get the stratum specific enroll rate and failure rate
    enroll_rate_s <- subset(enroll_rate, stratum == s)
    fail_rate_s <- subset(fail_rate, stratum == s)

    # get the change points of the piecewise exp for the failure rates
    fr_change_point <- fail_rate_s$duration
    # make the last change point into around 1e6
    i <- is.finite(fr_change_point)
    fr_change_point <- c(
      if (all(i)) head(fr_change_point, -1) else fr_change_point[i],
      max(total_duration) + 1e6
    )
    fr_cumsum <- cumsum(fr_change_point)

    for (td in total_duration) {
      fr_trunc <- fr_cumsum[fr_cumsum < td]

      # calculate the cumulative accrual before the td
      # cut by the change points from the pwexp dist of the failure rates
      cum_n <- expected_accrual(time = c(fr_trunc, td),
                                enroll_rate = enroll_rate_s)

      # get the accrual during the interval
      n <- diff_one(cum_n)

      tbl_n[[length(tbl_n) + 1]] <- data.frame(
        time = td, t = c(0, fr_trunc), stratum = s, n = n
      )
    }
  }
  # build the accrual table
  tbl_n <- rbindlist(tbl_n)

  # -------------------------------------- #
  #   compute expected events over time    #
  # -------------------------------------- #
  # compute proportion in each group
  q_e <- ratio / (1 + ratio)
  q_c <- 1 - q_e

  # compute expected events by treatment group, stratum
  # and time period listed in total_duration
  event_list <- list()
  for (s in strata) {
    # subset to stratum
    enroll <- subset(enroll_rate, stratum == s)

    # update enrollment rates
    enroll_c <- within(enroll, rate <- rate * q_c)
    enroll_e <- within(enroll, rate <- rate * q_e)

    # update failure rates
    fail_c <- subset(fail_rate, stratum == s)
    fail_e <- within(fail_c, fail_rate <- fail_rate * hr)

    for (td in total_duration) {
      # compute expected number of events
      event_c <- expected_event(
        enroll_rate = enroll_c,
        fail_rate = fail_c,
        total_duration = td,
        simple = FALSE)

      event_e <- expected_event(
        enroll_rate = enroll_e,
        fail_rate = fail_e,
        total_duration = td,
        simple = FALSE)

      # combine control and experimental
      event_c$treatment <- "control"
      event_e$treatment <- "experimental"
      event <- cbind(rbind(event_c, event_e), time = td, stratum = s)
      event_list[[length(event_list) + 1]] <- event
    }
  }
  tbl_event <- rbindlist(event_list)

  # recompute HR, events, info by time, stratum, and period
  tbl_event <- tbl_event[, .(
    info = 1/sum(1/event),
    event = sum(event),
    hr = last_(fail_rate) / fail_rate[1]
  ), by = .(time, stratum, t)]

  tbl_event[, info0 := event * q_c * q_e, by = .(time)]

  # pool strata together for each time period
  tbl_event <- tbl_event[, .(
    t = min(t),
    event = sum(event),
    info0 = sum(info0),
    info = sum(info)
  ), by = .(time, stratum, hr)]

  # -------------------------------------- #
  #    output the results                  #
  # -------------------------------------- #
  ans <- merge(tbl_event, tbl_n, by = c("time", "t", "stratum"))
  # filter out the rows with 0 events and unneeded columns
  ans <- ans[!almost_equal(event, 0L), .(time, stratum, t, hr, n, event, info, info0)]

  # row re-ordering
  setorderv(ans, cols = c("time", "stratum"))

  setDF(ans)
  return(ans)
}
