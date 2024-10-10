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

#' Define enrollment rate
#'
#' Define the enrollment rate of subjects for a study as following a piecewise
#' exponential distribution.
#'
#' @details
#' The `duration` are ordered piecewise for a duration equal to
#' \eqn{t_i - t_{i-1}}, where \eqn{0 = t_0 < t_i < \cdots < t_M = \infty}.
#' The enrollment rates are defined in each duration with the same length.
#'
#' For a study with multiple strata, different duration and rates can be
#' specified in each stratum.
#'
#' @param duration A numeric vector of ordered piecewise study duration interval.
#' @param rate A numeric vector of enrollment rate in each `duration`.
#' @param stratum A character vector of stratum name.
#'
#' @return An `enroll_rate` data frame.
#'
#' @export
#'
#' @examples
#' # Define enroll rate without stratum
#' define_enroll_rate(
#'   duration = c(2, 2, 10),
#'   rate = c(3, 6, 9)
#' )
#'
#' # Define enroll rate with stratum
#' define_enroll_rate(
#'   duration = rep(c(2, 2, 2, 18), 3),
#'   rate = c((1:4) / 3, (1:4) / 2, (1:4) / 6),
#'   stratum = c(array("High", 4), array("Moderate", 4), array("Low", 4))
#' )
define_enroll_rate <- function(
    duration,
    rate,
    stratum = "All") {
  df <- tibble(
    stratum = stratum,
    duration = duration,
    rate = rate
  )

  check_enroll_rate(df)

  class(df) <- c("enroll_rate", class(df))

  df
}

#' Define failure rate
#'
#' Define subject failure rate for a study with two treatment groups.
#' Also supports stratified designs that have different failure rates in
#' each stratum.
#'
#' @details
#' Define the failure and dropout rate of subjects for a study as following
#' a piecewise exponential distribution.
#' The `duration` are ordered piecewise for a duration equal to
#' \eqn{t_i - t_{i-1}}, where \eqn{0 = t_0 < t_i < \cdots < t_M = \infty}.
#' The failure rate, dropout rate, and hazard ratio in a study duration
#' can be specified.
#'
#' For a study with multiple strata, different duration, failure rates,
#' dropout rates, and hazard ratios can be specified in each stratum.
#'
#' @param duration A numeric vector of ordered piecewise study duration interval.
#' @param fail_rate A numeric vector of failure rate in each `duration` in the control group.
#' @param dropout_rate A numeric vector of dropout rate in each `duration`.
#' @param hr A numeric vector of hazard ratio between treatment and control group.
#' @param stratum A character vector of stratum name.
#'
#' @return A `fail_rate` data frame.
#'
#' @export
#'
#' @examples
#' # Define enroll rate
#' define_fail_rate(
#'   duration = c(3, 100),
#'   fail_rate = log(2) / c(9, 18),
#'   hr = c(.9, .6),
#'   dropout_rate = .001
#' )
#'
#' # Define enroll rate with stratum
#' define_fail_rate(
#'   stratum = c(rep("Low", 2), rep("High", 2)),
#'   duration = 1,
#'   fail_rate = c(.1, .2, .3, .4),
#'   dropout_rate = .001,
#'   hr = c(.9, .75, .8, .6)
#' )
define_fail_rate <- function(
    duration,
    fail_rate,
    dropout_rate,
    hr = 1,
    stratum = "All") {
  df <- tibble(
    stratum = stratum,
    duration = duration,
    fail_rate = fail_rate,
    dropout_rate = dropout_rate,
    hr = hr
  )

  check_fail_rate(df)

  class(df) <- c("fail_rate", class(df))

  df
}
