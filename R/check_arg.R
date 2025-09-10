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

# check if values in a vector are increasing
check_increasing <- function(x, name = deparse(substitute(x)), first = TRUE) {
  # if first = TRUE, require x[1] to be positive, otherwise x[1] can be 0
  d <- if (first) diff_one(x) else diff(x)
  if (any(d <= 0)) stop(
    "`", name, "` (", deparse(x), ") must be ",
    if (first) "positive and ", "strictly increasing!"
  )
}

check_positive <- function(x, name = deparse(substitute(x))) {
  if (!is.numeric(x) || any(x <= 0)) stop(
    "`", name, "` (", deparse(x), ") must be positive!"
  )
}

check_non_negative <- function(x, name = deparse(substitute(x)), ...) {
  if (!is.numeric(x) || any(x < 0)) stop(
    "`", name, "` (", deparse(x), ") must not be negative!", ...
  )
}

#' Check the argument `enroll_rate`
#'
#' @inheritParams ahr
#' @noRd
check_enroll_rate <- function(x) {
  if (!"stratum" %in% names(x)) x$stratum <- "All"
  msg <- " Try the helper function define_enroll_rate() to prepare valid input."
  for (j in c("duration", "rate")) check_non_negative(x[[j]], j, msg)
  x
}

#' Check the argument `fail_rate`
#'
#' @inheritParams ahr
#' @noRd
check_fail_rate <- function(x) {
  if (!"hr" %in% names(x)) x$hr <- 1
  if (!"stratum" %in% names(x)) x$stratum <- "All"

  msg <- " Try the helper function define_fail_rate() to prepare valid input."
  for (j in c("duration", "fail_rate", "dropout_rate", "hr"))
    check_non_negative(x[[j]], j, msg)

  x
}

#' Check the arguments `enroll_rate` and `fail_rate`
#'
#' @inheritParams ahr
#' @noRd
check_enroll_rate_fail_rate <- function(enroll_rate, fail_rate) {
  if ("stratum" %in% colnames(enroll_rate) && "stratum" %in% colnames(fail_rate)) {
    s1 <- unique(enroll_rate$stratum)
    s2 <- unique(fail_rate$stratum)
    n1 <- length(s1); n2 <- length(s2)
    if (n1 != n2 || n1 != length(intersect(s1, s2))) stop(
      "`enroll_rate` and `fail_rate` must have same `stratum` values: ",
      "`enroll_rate` has ", deparse(s1), " but `fail_rate` has ", deparse(s2)
    )
  }
}

#' Check the arguments `analysis_time`
#' @param x Analysis time.
#' @noRd
check_analysis_time <- check_increasing

#' Check the argument `event`
#'
#' @param x Number of events.
#' @noRd
check_event <- check_increasing

#' Check the argument `total_duration`
#'
#' @param x Total duration.
#' @noRd
check_total_duration <- check_positive

#' Check the argument `ratio`
#'
#' @param x Randomization ratio.
#' @noRd
check_ratio <- check_positive

#' Check the argument `info`
#'
#' @param x Statistical information.
#' @noRd
check_info <- check_increasing

#' Check the argument `theta`
#'
#' @param theta Treatment effect.
#' @param K Number of total analysis.
#' @noRd
check_theta <- function(theta, K) {
  n <- length(theta)
  if (n > 1 && n != K) stop(
    "the length of `theta` (", n, ") differs with the number of analyses (", K, ")!"
  )
  if (last_(theta) < 0) stop(
    "final effect size of `theta` (", deparse(theta), ") must be non-negative!"
  )
  if (n == 1 && K > 1) rep(theta, K) else theta
}

#' Check the arguments `test_upper` and `test_lower`
#'
#' @param x Test upper or lower.
#' @param K Number of total analysis.
#' @noRd
check_test_upper <- function(x, K) {
  # check that final test_upper value is TRUE
  if (!last_(x)) stop("last value of `test_upper` (", deparse(x), "), must be TRUE!")
  check_test_ul(x, K, "test_upper")
}

check_test_lower <- function(x, K) check_test_ul(x, K, "test_lower")

check_test_ul <- function(x, K, name) {
  # check length
  n <- length(x)
  if (n > 1 && n != K) stop(
    "`", name, "` must be of length 1 or same length as the number of analyses (", K, ")!"
  )
  if (n == 1 && K > 1) rep(x, K) else x
}

#' Check the arguments `alpha` and `beta`
#'
#' @param alpha Type I error.
#' @param beta Type II error.
#' @noRd
check_alpha_beta <- function(alpha, beta) {
  if (!(0 < alpha && beta > 0 && alpha < 1 - beta))
    stop("`alpha` and `beta` values must satisfy 0 < alpha < 1 - beta < 1!")
}

#' Check the argument `info_frac`
#'
#' @param x Statistical informational fraction.
#' @noRd
check_info_frac <- function(x) {
  check_increasing(x, "info_frac")
  if (last_(x) != 1) stop("The last value of `info_frac` must be 1!")
}
