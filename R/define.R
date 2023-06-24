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
#' # Define enroll rate without stratum
#' define_enroll_rate(
#'   duration = c(2, 2, 10),
#'   rate = c(3, 6, 9)
#' )
#'
#' # Define enroll rate with stratum
#' define_enroll_rate(
#'   stratum = c("low", "low", "high"),
#'   duration = c(2, 2, 10),
#'   rate = c(3, 6, 9)
#' )
define_enroll_rate <- function(
    duration,
    rate,
    stratum = "All") {
  if (is.null(duration)) {
    stop("define_enroll_rate: variable `duration` can't be NULL.")
  }

  if (is.null(rate)) {
    stop("define_enroll_rate: variable `rate` can't be NULL.")
  }

  check_args(duration, type = c("numeric", "integer"))
  check_args(rate, type = c("numeric", "integer"))
  check_args(stratum, type = c("character"))

  if (any(duration < 0)) {
    stop("define_enroll_rate: enrollment duration `duration` can't be negative.")
  }

  if (any(rate < 0)) {
    stop("define_enroll_rate: enrollment rate `rate` can't be negative.")
  }

  df <- tibble::tibble(
    stratum = stratum,
    duration = duration,
    rate = rate
  )

  class(df) <- c("enroll_rate", class(df))

  df
}

#' Define fail rate
#'
#' @param duration A numeric vector of piecewise study duration interval.
#' @param fail_rate A numeric vector of failure rate in each `duration`.
#' @param dropout_rate A numeric vector of dropout rate in each `duration`.
#' @param hr A numeric vector of hazard ratio.
#' @param stratum A character vector of stratum name.
#'
#' @return A `fail_rate` data frame.
#'
#' @export
#'
#' @examples
#' # Define enroll rate without stratum
#' define_fail_rate(
#'   duration = c(3, 100),
#'   fail_rate = log(2) / c(9, 18),
#'   hr = c(.9, .6),
#'   dropout_rate = .001
#' )
#'
#' # Define enroll rate with stratum
#' define_fail_rate(
#'   duration = c(3, 100),
#'   fail_rate = log(2) / c(9, 18),
#'   hr = c(.9, .6),
#'   dropout_rate = .001,
#'   stratum = c("low", "high")
#' )
define_fail_rate <- function(
    duration,
    fail_rate,
    dropout_rate,
    hr = 1,
    stratum = "All") {
  if (is.null(duration)) {
    stop("define_enroll_rate: variable `duration` can't be NULL.")
  }

  if (is.null(fail_rate)) {
    stop("define_enroll_rate: variable `fail_rate` can't be NULL.")
  }

  if (is.null(dropout_rate)) {
    stop("define_enroll_rate: variable `dropout_rate` can't be NULL.")
  }

  check_args(duration, type = c("numeric", "integer"))
  check_args(fail_rate, type = c("numeric", "integer"))
  check_args(dropout_rate, type = c("numeric", "integer"))
  check_args(hr, type = c("numeric", "integer"))
  check_args(stratum, type = c("character"))

  if (any(duration < 0)) {
    stop("define_fail_rate: enrollment duration `duration` can't be negative.")
  }

  if (any(fail_rate < 0)) {
    stop("define_fail_rate: failure rate `fail_rate` can't be negative.")
  }

  if (any(dropout_rate < 0)) {
    stop("define_fail_rate: failure rate `fail_rate` can't be negative.")
  }

  if (any(hr < 0)) {
    stop("define_fail_rate: hazard ratio `hr` can't be negative.")
  }

  df <- tibble::tibble(
    stratum = stratum,
    duration = duration,
    fail_rate = fail_rate,
    dropout_rate = dropout_rate,
    hr = hr
  )

  class(df) <- c("fail_rate", class(df))

  df
}
