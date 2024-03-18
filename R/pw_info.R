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

#' Average hazard ratio under non-proportional hazards (test version)
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
#' @return A tibble with `time` (from `total_duration`),
#'   `ahr` (average hazard ratio), `event` (expected number of events),
#'   `info` (information under given scenarios), `and` info0
#'   (information under related null hypothesis) for each value of
#'   `total_duration` input
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input enrollment rate contains stratum column.
#'    \item Validate if input enrollment rate contains total duration column.
#'    \item Validate if input enrollment rate contains rate column.
#'    \item Validate if input failure rate contains stratum column.
#'    \item Validate if input failure rate contains duration column.
#'    \item Validate if input failure rate contains failure rate column.
#'    \item Validate if input failure rate contains hazard ratio column.
#'    \item Validate if input failure rate contains dropout rate column.
#'    \item Validate if input trial total follow-up (total duration) is a non-empty vector of positive integers.
#'    \item Validate if strata is the same in enrollment rate and failure rate.
#'    \item Compute the proportion in each group.
#'    \item Compute the expected events by treatment groups, stratum and time period.
#'    \item Calculate the expected number of events for all time points in the total
#'     duration and for all stratification variables.
#'    \itemize{
#'      \item Compute the expected events in for each strata.
#'        \itemize{
#'          \item Combine the expected number of events of all stratification variables.
#'          \item Recompute events, hazard ratio and information under
#'          the given scenario of the combined data for each strata.
#'          }
#'        \item Combine the results for all time points by summarizing the results by adding up the number of events,
#'       information under the null and the given scenarios.
#'       }
#'    \item Return a tibble of overall event count, statistical information and average hazard ratio
#'    of each value in total_duration.
#'    \item Calculation of \code{ahr} for different design scenarios, and the comparison to the
#'    simulation studies are defined in vignette/AHRVignette.Rmd.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @importFrom data.table ":=" as.data.table copy first last rbindlist setDF
#'                        setorderv
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
#'   duration = 1,
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
  # Check input values ----
  check_enroll_rate(enroll_rate)
  check_fail_rate(fail_rate)
  check_enroll_rate_fail_rate(enroll_rate, fail_rate)
  check_total_duration(total_duration)
  check_ratio(ratio)
  enroll_rate <- as.data.table(enroll_rate)
  class(enroll_rate) <- c("enroll_rate", class(enroll_rate))
  fail_rate <- as.data.table(fail_rate)
  class(fail_rate) <- c("fail_rate", class(enroll_rate))
  # compute proportion in each group
  q_e <- ratio / (1 + ratio)
  q_c <- 1 - q_e
  # compute expected events by treatment group, stratum and time period
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
        simple = FALSE
      )
      event_e <- expected_event(
        enroll_rate = enroll_e,
        fail_rate = fail_e,
        total_duration = td,
        simple = FALSE
      )
      # Combine control and experimental; by period recompute HR, events, information
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
      info0 = event * q_c * q_e
    )]
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
  ans <- rbindlist(ans_list)
  # output the results
  ans <- ans[, .(time, stratum, t, hr, event, info, info0)]
  setorderv(ans, cols = c("time", "stratum"))
  ans <- ans[order(t), .SD, by = .(time, stratum)]
  # filter out the rows with 0 events
  ans <- ans[!is_almost_k(event, 0L)]
  setDF(ans)
  return(ans)
}
