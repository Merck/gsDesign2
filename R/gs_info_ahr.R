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

#' Information and effect size based on AHR approximation
#'
#' Based on piecewise enrollment rate, failure rate, and dropout rates computes
#' approximate information and effect size using an average hazard ratio model.
#'
#' @param enroll_rate Enrollment rates.
#' @param fail_rate Failure and dropout rates.
#' @param ratio Experimental:Control randomization ratio.
#' @param event Targeted minimum events at each analysis.
#' @param analysis_time Targeted minimum study duration at each analysis.
#' @param interval An interval that is presumed to include the time at which
#'   expected event count is equal to targeted event.
#'
#' @return A tibble with columns Analysis, Time, AHR, Events, theta, info, info0.
#'   `info`, and `info0` contain statistical information under H1, H0, respectively.
#'   For analysis `k`, `Time[k]` is the maximum of `analysis_time[k]` and the
#'   expected time required to accrue the targeted `event[k]`.
#'   `AHR` is the expected average hazard ratio at each analysis.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input event is a numeric value vector or a vector with increasing values.
#'    \item Validate if input analysis_time is a numeric value vector or a vector with increasing values.
#'    \item Validate if inputs event and analysis_time have the same length if they are both specified.
#'    \item Compute average hazard ratio:
#'    \itemize{
#'      \item If analysis_time is specified, calculate average hazard ratio using \code{AHR()}.
#'      \item If event is specified, calculate average hazard ratio using \code{expected_time()}.
#'    }
#'    \item Return a tibble of Analysis, Time, AHR, Events, theta, info, info0.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @details
#' The [ahr()] function computes statistical information at targeted
#' event times. The [expected_time()] function is used to get events and
#' average HR at targeted `analysis_time`.
#'
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' library(gsDesign)
#' library(gsDesign2)
#'
#' # ------------------------ #
#' #       Example 1          #
#' # ------------------------ #
#' \donttest{
#' # Only put in targeted events
#' gs_info_ahr(event = c(30, 40, 50))
#' }
#' # ------------------------ #
#' #       Example 2          #
#' # ------------------------ #
#'
#' # Only put in targeted analysis times
#' gs_info_ahr(analysis_time = c(18, 27, 36))
#'
#' # ------------------------ #
#' #       Example 3          #
#' # ------------------------ #
#' \donttest{
#' # Some analysis times after time at which targeted event accrue
#' # Check that both Time >= input analysis_time and event >= input event
#' gs_info_ahr(event = c(30, 40, 50), analysis_time = c(16, 19, 26))
#' gs_info_ahr(event = c(30, 40, 50), analysis_time = c(14, 20, 24))
#' }
gs_info_ahr <- function(enroll_rate = tibble::tibble(
                          stratum = "all",
                          duration = c(2, 2, 10),
                          rate = c(3, 6, 9)
                        ),
                        fail_rate = tibble::tibble(
                          stratum = "all",
                          duration = c(3, 100),
                          fail_rate = log(2) / c(9, 18),
                          hr = c(.9, .6),
                          dropout_rate = rep(.001, 2)
                        ),
                        ratio = 1, # experimental:Control randomization ratio
                        event = NULL, # event at analyses
                        analysis_time = NULL, # times of analyses
                        interval = c(.01, 100)) {
  # ----------------------------#
  #    check input values       #
  # ----------------------------#
  check_enroll_rate(enroll_rate)
  check_fail_rate(fail_rate)
  check_enroll_rate_fail_rate(enroll_rate, fail_rate)

  if (is.null(analysis_time) && is.null(event)) {
    stop("gs_info_ahr(): One of `event` and `analysis_time`
         must be a numeric value or vector with increasing values")
  }

  n_analysis <- 0
  if (!is.null(analysis_time)) {
    check_analysis_time(analysis_time)
    n_analysis <- length(analysis_time)
  }

  if (!is.null(event)) {
    check_event(event)
    if (n_analysis == 0) {
      n_analysis <- length(event)
    } else if (n_analysis != length(event)) {
      stop("gs_info_ahr(): If both event and analysis_time
           specified, must have same length")
    }
  }

  # ----------------------------#
  #    check input values       #
  # ----------------------------#
  avehr <- NULL
  if (!is.null(analysis_time)) {
    # calculate AHR, Events, info, info0 given the analysis_time
    avehr <- ahr(
      enroll_rate = enroll_rate, fail_rate = fail_rate,
      ratio = ratio, total_duration = analysis_time
    )
    # check if the output Events is larger enough than the targeted events
    for (i in seq_along(event)) {
      if (avehr$event[i] < event[i]) {
        avehr[i, ] <- expected_time(
          enroll_rate = enroll_rate, fail_rate = fail_rate,
          ratio = ratio, target_event = event[i],
          interval = interval
        )
      }
    }
  } else {
    for (i in seq_along(event)) {
      avehr <- rbind(
        avehr,
        expected_time(
          enroll_rate = enroll_rate, fail_rate = fail_rate,
          ratio = ratio, target_event = event[i],
          interval = interval
        )
      )
    }
  }

  # ----------------------------#
  #    compute theta            #
  # ----------------------------#
  avehr$analysis <- seq_len(nrow(avehr))
  avehr$theta <- -log(avehr$ahr)

  # ----------------------------#
  #    output results           #
  # ----------------------------#
  ans <- avehr %>% dplyr::transmute(analysis, time, event, ahr, theta, info, info0)
  return(ans)
}
