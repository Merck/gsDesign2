#  Copyright (c) 2022 Merck & Co., Inc., Rahway, NJ, USA and its affiliates. All rights reserved.
#
#  This file is part of the gsdmvn program.
#
#  gsdmvn is free software: you can redistribute it and/or modify
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

#' @importFrom tibble tibble
#' @importFrom dplyr lag
NULL
#' Information and effect size based on AHR approximation
#'
#' Based on piecewise enrollment rate, failure rate, and dropout rates computes
#' approximate information and effect size using an average hazard ratio model.
#' @param enrollRates enrollment rates
#' @param failRates failure and dropout rates
#' @param ratio Experimental:Control randomization ratio
#' @param events Targeted minimum events at each analysis
#' @param analysisTimes Targeted minimum study duration at each analysis
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input events is a numeric value vector or a vector with increasing values.
#'    \item Validate if input analysisTime is a numeric value vector or a vector with increasing values.
#'    \item Validate if inputs events and analysisTime have the same length if they are both specified.
#'    \item Compute average hazard ratio:
#'    \itemize{
#'      \item If analysisTime is specified, calculate average hazard ratio using \code{gsDesign2::AHR()}.
#'      \item If events is specified, calculate average hazard ratio using \code{gsDesign2::tEvents()}.
#'    }
#'    \item Return a tibble of Analysis, Time, AHR, Events, theta, info, info0.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @return a \code{tibble} with columns \code{Analysis, Time, AHR, Events, theta, info, info0.}
#' \code{info, info0} contains statistical information under H1, H0, respectively.
#' For analysis \code{k}, \code{Time[k]} is the maximum of \code{analysisTimes[k]} and the expected time
#' required to accrue the targeted \code{events[k]}.
#' \code{AHR} is expected average hazard ratio at each analysis.
#' @details The \code{AHR()} function computes statistical information at targeted event times.
#' The \code{tEvents()} function is used to get events and average HR at targeted \code{analysisTimes}.
#' @export
#'
#' @examples
#' library(gsDesign)
#' library(gsDesign2)
#'
#' # Only put in targeted events
#' gs_info_ahr_(events = c(30, 40, 50))
#' # Only put in targeted analysis times
#' gs_info_ahr_(analysisTimes = c(18, 27, 36))
#' # Some analysis times after time at which targeted events accrue
#' # Check that both Time >= input analysisTime and Events >= input events
#' gs_info_ahr_(events = c(30, 40, 50), analysisTimes = c(16, 19, 26))
#' gs_info_ahr_(events = c(30, 40, 50), analysisTimes = c(14, 20, 24))
gs_info_ahr_ <- function(enrollRates = tibble::tibble(
                           Stratum = "All",
                           duration = c(2, 2, 10),
                           rate = c(3, 6, 9)
                         ),
                         failRates = tibble::tibble(
                           Stratum = "All",
                           duration = c(3, 100),
                           failRate = log(2) / c(9, 18),
                           hr = c(.9, .6),
                           dropoutRate = rep(.001, 2)
                         ),
                         ratio = 1, # Experimental:Control randomization ratio
                         events = NULL, # Events at analyses
                         analysisTimes = NULL # Times of analyses
) {
  ################################################################################
  # Check input values
  K <- 0
  if (is.null(analysisTimes) && is.null(events)) stop("gs_info_ahr(): One of
                                                      events and analysisTimes must be a
                                                      numeric value or vector with increasing values")
  if (!is.null(analysisTimes)) {
    if (!is.numeric(analysisTimes) || !is.vector(analysisTimes) || min(analysisTimes - dplyr::lag(analysisTimes, def = 0)) <= 0
    ) {
      stop("gs_info_ahr(): analysisTimes must be NULL a numeric vector with positive increasing values")
    }
    K <- length(analysisTimes)
  }
  if (!is.null(events)) {
    if (!is.numeric(events) || !is.vector(events) || min(events - dplyr::lag(events, default = 0)) <= 0
    ) {
      stop("gs_info_ahr(): events must be NULL or a numeric vector with positive increasing values")
    }
    if (K == 0) {
      K <- length(events)
    } else if (K != length(events)) stop("gs_info_ahr(): If both events and analysisTimes specified, must have same length")
  }
  # end check input values
  ################################################################################
  avehr <- NULL
  if (!is.null(analysisTimes)) {
    avehr <- AHR_(
      enrollRates = enrollRates, failRates = failRates, ratio = ratio,
      totalDuration = analysisTimes
    )
    for (i in seq_along(events)) {
      if (avehr$Events[i] < events[i]) {
        avehr[i, ] <- tEvents_(
          enrollRates = enrollRates, failRates = failRates, ratio = ratio,
          targetEvents = events[i]
        )
      }
    }
  } else {
    for (i in seq_along(events)) {
      avehr <- rbind(
        avehr,
        tEvents_(
          enrollRates = enrollRates, failRates = failRates, ratio = ratio,
          targetEvents = events[i]
        )
      )
    }
  }
  avehr$Analysis <- 1:nrow(avehr)
  avehr$theta <- -log(avehr$AHR)
  return(avehr %>% dplyr::transmute(Analysis, Time, Events, AHR, theta, info, info0))
}
