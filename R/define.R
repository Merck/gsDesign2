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

#' Define enrollment rate
#'
#' @param duration A numeric vector of piecewise study duration interval.
#' @param rate A numeric vector of enrollment rate in each `duration`.
#' @param stratum A character vector of stratum name.
#'
#' @return An `enroll_rate` data frame.
#'
#' @export
#'
#' @examples
#'
#' # define enroll rate without stratum
#' define_enroll_rate(
#'   duration = c(2, 2, 10),
#'   rate = c(3, 6, 9)
#' )
#'
#' # define enroll rate with stratum
#' define_enroll_rate(
#'   stratum = c("low", "low", "high"),
#'   duration = c(2, 2, 10),
#'   rate = c(3, 6, 9)
#' )
define_enroll_rate <- function(
    duration,
    rate,
    stratum = rep("All", length(duration))) {
  # Length of variables
  l <- unique(c(length(duration), length(rate), length(stratum)))

  if (length(l) > 1) stop("define_enroll_rate: length of input arguments must be the same.")

  check_args(duration, length = l, type = c("numeric", "integer"))
  check_args(rate, length = l, type = c("numeric", "integer"))
  check_args(stratum, length = l, type = c("character"))

  if (any(duration < 0)) {
    stop("define_enroll_rate: enrollment duration `duration` can not be negative")
  }

  if (any(rate < 0)) {
    stop("define_enroll_rate: enrollment rate `rate` can not be negative")
  }

  df <- data.frame(
    stratum = stratum,
    duration = duration,
    rate = rate
  )

  class(df) <- c("enroll_rate", class(df))

  df
}
