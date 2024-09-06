#  Copyright (c) 2024 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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
  #    Check input values                  #
  # -------------------------------------- #
  check_enroll_rate(enroll_rate)
  check_fail_rate(fail_rate)
  check_enroll_rate_fail_rate(enroll_rate, fail_rate)
  check_total_duration(total_duration)
  check_ratio(ratio)
  enroll_rate <- as.data.table(enroll_rate)
  class(enroll_rate) <- c("enroll_rate", class(enroll_rate))
  fail_rate <- as.data.table(fail_rate)
  class(fail_rate) <- c("fail_rate", class(enroll_rate))

  # -------------------------------------- #
  #   compute expected accrual over time   #
  # -------------------------------------- #
  # calculate the sample size accrual during the interval
  tbl_n <- NULL
  strata <- unique(enroll_rate$stratum)

  for (i in seq_along(total_duration)) {
    td <- total_duration[i]

    for (j in seq_along(strata)) {
      # get the stratum specific enroll rate and failure rate
      s <- strata[j]
      enroll_rate_s <- enroll_rate[stratum == s, ]
      fail_rate_s <- fail_rate[stratum == s, ]

      # get the change points of the piecewise exp for the failure rates
      fr_change_point <- fail_rate_s$duration
      # make the last change point into around 1e6
      if (is.infinite(max(fr_change_point))) {
        fr_change_point <- fr_change_point[is.finite(fr_change_point)]
        fr_change_point <- c(fr_change_point, max(total_duration) +  1e6)
      } else {
        fr_change_point <- fr_change_point[-length(fr_change_point)]
        fr_change_point <- c(fr_change_point, max(total_duration) + 1e6)
      }
      temp_fr_change_point <- fr_change_point[which(cumsum(fr_change_point) <= td)]

      # get the starting time of each pwexp interval
      start_time_fr <- c(0, cumsum(fr_change_point)[-length(fr_change_point)])

      # calculate the cumulative accrual before the td
      # cut by the change points from the pwexp dist of the failure rates
      cum_n <- expected_accrual(time = sort(unique(c(cumsum(temp_fr_change_point), td))),
                                enroll_rate = enroll_rate_s)

      # get the accrual during the interval
      n <- diff(c(0, cum_n))

      # build the accrual table
      tbl_n_new <- data.frame(time = rep(td, length(n)),
                              t = start_time_fr[start_time_fr < td],
                              stratum = rep(s, length(n)),
                              n = n)

      tbl_n <- rbind(tbl_n, tbl_n_new)
    }
  }


  # -------------------------------------- #
  #   compute expected events over time    #
  # -------------------------------------- #
  # compute proportion in each group
  q_e <- ratio / (1 + ratio)
  q_c <- 1 - q_e

  # compute expected events by treatment group, stratum
  # and time period listed in total_duration
  strata <- unique(enroll_rate$stratum)
  ans_list <- vector(mode = "list", length = length(total_duration) * length(strata))
  for (i in seq_along(total_duration)) {
    td <- total_duration[i]
    event_list <- vector(mode = "list", length = length(strata))
    for (j in seq_along(strata)) {
      s <- strata[j]
      # subset to stratum
      enroll <- enroll_rate[stratum == s, ]
      fail <- fail_rate[stratum == s, ]

      # update enrollment rates
      enroll_c <- copy(enroll)
      enroll_c[, rate := rate * q_c]
      enroll_e <- copy(enroll)
      enroll_e[, rate := rate * q_e]

      # update failure rates
      fail_c <- copy(fail)
      fail_e <- copy(fail)
      fail_e[, fail_rate := fail_rate * hr]

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

      # combine control and experimental; by period recompute HR, events, information
      setDT(event_c)
      event_c[, treatment := "control"]
      setDT(event_e)
      event_e[, treatment := "experimental"]
      event_tmp <- rbindlist(list(event_c, event_e))
      event_tmp <- event_tmp[order(t, treatment), ]

      # recompute HR, events, info by period
      event_tmp <- event_tmp[,
        .(
          stratum = s,
          info = (sum(1 / event))^(-1),
          event = sum(event),
          hr = last(fail_rate) / first(fail_rate)
        ),
        by = "t"
      ]
      event_list[[j]] <- event_tmp
    }
    # summarize events in one stratum
    event <- rbindlist(event_list)
    event[, `:=`(
      time = td,
      ln_hr = log(hr),
      info0 = event * q_c * q_e)]

    # pool strata together for each time period
    event <- event[,
      .(
        t = min(t),
        event = sum(event),
        info0 = sum(info0),
        info = sum(info)
      ),
      by = .(time, stratum, hr)
    ]
    ans_list[[i + j]] <- event
  }
  tbl_event <- rbindlist(ans_list)

  # -------------------------------------- #
  #    output the results                  #
  # -------------------------------------- #
  ans <- merge(tbl_event, tbl_n, by = c("time", "t", "stratum"))
  ans <- ans[, .(time, stratum, t, hr, n, event, info, info0)]

  # row re-ordering
  setorderv(ans, cols = c("time", "stratum"))
  ans <- ans[order(t), .SD, by = .(time, stratum)]

  # filter out the rows with 0 events
  ans <- ans[!almost_equal(event, 0L)]
  setcolorder(ans, neworder = c("time", "stratum", "t", "hr", "n", "event", "info", "info0"))

  setDF(ans)
  return(ans)
}
