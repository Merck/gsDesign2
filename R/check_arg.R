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

#' A function to check the arguments `enroll_rate` used in gsDesign2
#'
#' @param enroll_rate Enrollment rates.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
#'
#' @examples
#'
#' enroll_rate <- tibble::tibble(stratum = "All", duration = c(2, 2, 10), rate = c(3, 6, 9))
#' check_enroll_rate(enroll_rate)
check_enroll_rate <- function(enroll_rate) {
  # --------------------------- #
  #   check the duration column #
  # --------------------------- #
  if (!"duration" %in% colnames(enroll_rate)) {
    stop("The enroll_rate is a tibble which contains a column called `duration`!")
  }
  # the duration is numerical values
  if (!is.numeric(enroll_rate$duration)) {
    stop("The `duration`column in enroll_rate should be numeric!")
  }

  # the duration is positive numbers
  if (sum(!enroll_rate$duration > 0) != 0) {
    stop("The `duration` column in enroll_rate should be positive numbers!")
  }

  # --------------------------- #
  #   check the rate column     #
  # --------------------------- #
  if (!"rate" %in% colnames(enroll_rate)) {
    stop("The enroll_rate is a tibble which contains a column called `rate`!")
  }

  # the rate is numerical values
  if (!is.numeric(enroll_rate$rate)) {
    stop("The `rate`column in enroll_rate should be numeric!")
  }

  # the rate is positive numbers
  if (sum(!enroll_rate$rate >= 0) != 0) {
    stop("The `rate` column in enroll_rate should be positive numbers!")
  }
}



#' A function to check the arguments `fail_rate` used in gsDesign2
#'
#' @param fail_rate Failure rates.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
#'
#' @examples
#' fail_rate <- tibble::tibble(
#'   stratum = "All", duration = c(3, 100),
#'   fail_rate = log(2) / c(9, 18), hr = c(.9, .6),
#'   dropout_rate = rep(.001, 2)
#' )
#' check_fail_rate(fail_rate)
check_fail_rate <- function(fail_rate) {
  # --------------------------- #
  #   check the duration column #
  # --------------------------- #
  if (!"duration" %in% colnames(fail_rate)) {
    stop("The fail_rate is a tibble which contains a column called `duration`!")
  }
  # the duration is numerical values
  if (!is.numeric(fail_rate$duration)) {
    stop("The `duration`column in fail_rate should be numeric!")
  }

  # the duration is positive numbers
  if (sum(!fail_rate$duration > 0) != 0) {
    stop("The `duration` column in fail_rate should be positive numbers!")
  }

  # -----------------------------#
  #   check the fail_rate column #
  # ---------------------------- #
  if (!"fail_rate" %in% colnames(fail_rate)) {
    stop("The fail_rate is a tibble which contains a column called `fail_rate`!")
  }

  # the rates are fail_rate values
  if (!is.numeric(fail_rate$fail_rate)) {
    stop("The `fail_rate`column in fail_rate should be numeric!")
  }

  # the rates are positive numbers
  if (any(fail_rate$fail_rate < 0)) {
    stop("The `fail_rate` column in fail_rate should be positive numbers!")
  }

  # at least 1 rate is positive
  if (all(fail_rate$fail_rate <= 0)) {
    stop("The `fail_rate` column in fail_rate should have at least one positive number!")
  }

  # --------------------------- #
  #   check the hr column       #
  # --------------------------- #
  if ("hr" %in% colnames(fail_rate)) {
    if (!is.numeric(fail_rate$hr)) {
      stop("The `hr`column in fail_rate should be numeric!")
    }

    if (sum(!fail_rate$hr > 0) != 0) {
      stop("The `hr` column in fail_rate should be positive numbers!")
    }
  }

  # --------------------------- #
  # check the dropout_rate column#
  # --------------------------- #
  if (!"dropout_rate" %in% colnames(fail_rate)) {
    stop("The fail_rate is a tibble which contains a column called `dropout_rate`!")
  }

  # the rate is numerical values
  if (!is.numeric(fail_rate$dropout_rate)) {
    stop("The `dropout_rate`column in fail_rate should be numeric!")
  }

  # the rate is positive numbers
  if (sum(!fail_rate$dropout_rate >= 0) != 0) {
    stop("The `dropout_rate` column in fail_rate should be positive numbers!")
  }
}



#' A function to check the arguments `enroll_rate` and `fail_rate` used in gsDesign2
#'
#' @param enroll_rate Enrollment rates.
#' @param fail_rate Failure rates.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
#'
#' @examples
#' enroll_rate <- tibble::tibble(stratum = "All", duration = c(2, 2, 10), rate = c(3, 6, 9))
#' fail_rate <- tibble::tibble(
#'   stratum = "All", duration = c(3, 100),
#'   fail_rate = log(2) / c(9, 18), hr = c(.9, .6),
#'   dropout_rate = rep(.001, 2)
#' )
#' check_enroll_rate_fail_rate(enroll_rate, fail_rate)
check_enroll_rate_fail_rate <- function(enroll_rate, fail_rate) {
  if ("stratum" %in% colnames(enroll_rate) && "stratum" %in% colnames(fail_rate)) {
    strata_enroll <- unique(enroll_rate$stratum)
    strata_fail <- unique(fail_rate$stratum)
    strata_common <- dplyr::intersect(strata_enroll, strata_fail)

    if (sum(strata_common %in% strata_enroll) != length(strata_enroll)) {
      stop("The `Strata` column in the input argument `enroll_rate` and `fail_rate` must be the same!")
    }
  }
}


#' A function to check the arguments `analysis_time` used in gsDesign2
#'
#' @param analysis_time Analysis time.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
#'
#' @examples
#' analysis_time <- 20
#' check_analysis_time(analysis_time)
#'
#' analysis_time <- c(20, 30)
#' check_analysis_time(analysis_time)
check_analysis_time <- function(analysis_time) {
  cond1 <- !is.numeric(analysis_time)
  cond2 <- !is.vector(analysis_time)
  cond3 <- min(analysis_time - dplyr::lag(analysis_time, def = 0)) <= 0
  if (cond1 || cond2 || cond3) {
    stop("The input argument `analysis_times` must be NULL a numeric vector with positive increasing values!")
  }
}


#' A function to check the arguments `events` used in gsDesign2
#'
#' @param events Number of events.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
#'
#' @examples
#' event <- 20
#' check_event(events)
#'
#' event <- c(20, 30)
#' check_event(events)
check_event <- function(event) {
  cond1 <- !is.numeric(event)
  cond2 <- !is.vector(event)
  cond3 <- min(event - dplyr::lag(event, default = 0)) <= 0
  if (cond1 || cond2 || cond3) {
    stop("The input argument `event` must be NULL or a numeric vector with positive increasing values!")
  }
}

#' A function to check the arguments `total_duration` used in gsDesign2
#'
#' @param totalDuration Total duration.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
#'
#' @examples
#' total_duration <- 36
#' check_total_duration(total_duration)
#'
#' total_duration <- c(36, 48)
#' check_total_duration(total_duration)
check_total_duration <- function(total_duration) {
  if (!is.numeric(total_duration)) {
    stop("The input argument `total_duration` must be a non-empty vector of positive numbers!")
  }

  if (sum(!total_duration > 0) != 0) {
    stop("The input argument `total_duration` must be a non-empty vector of positive numbers!")
  }
}

#' A function to check the arguments `ratio` used in gsDesign2
#'
#' @param ratio Randomization ratio.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
#'
#' @examples
#' ratio <- 1
#' check_ratio(ratio)
check_ratio <- function(ratio) {
  if (!is.numeric(ratio)) {
    stop("The input argument `ratio` must be a numerical number!")
  }

  if (ratio <= 0) {
    stop("The input argument `ratio` must be a positive number!")
  }
}

#' A function to check the arguments `info` used in `gs_power_npe()`
#' or `gs_design_npe()` in gsDesign2
#'
#' @param info Statistical information.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
#'
#' @examples
#' info <- 1:3
#' check_info(info)
check_info <- function(info) {
  if (!is.vector(info, mode = "numeric")) {
    stop("gs_design_npe() or gs_power_npe(): info must be specified numeric vector!")
  }
  if (min(info - lag(info, default = 0)) <= 0) {
    stop("gs_design_npe() or gs_power_npe(): info much be strictly increasing and positive!")
  }
}

#' A function to check the arguments `theta` used in `gs_power_npe()`
#' or `gs_design_npe()` in gsDesign2
#'
#' @param theta Treatment effect.
#' @param n_analysis Number of total analysis.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
#'
#' @examples
#' check_theta(theta = rep(0.5, 3), n_analysis = 3)
check_theta <- function(theta, n_analysis) {
  if (!is.vector(theta, mode = "numeric")) {
    stop("gs_design_npe() or gs_power_npe(): theta must be a real vector!")
  }

  if (length(theta) != n_analysis) {
    stop("gs_design_npe() or gs_power_npe(): if length(theta) > 1, must be same as info!")
  }

  if (theta[n_analysis] < 0) {
    stop("gs_design_npe() or gs_power_npe(): final effect size must be > 0!")
  }
}

#' A function to check the arguments `test_upper` used in `gs_power_npe()`
#' or `gs_design_npe()` in gsDesign2
#'
#' @param test_upper Test upper or lower.
#' @param n_analysis Number of total analysis.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
#'
#' @examples
#' test_upper <- TRUE
#' check_test_upper(test_upper, 1)
check_test_upper <- function(test_upper, n_analysis) {
  ## Check test_upper and test_lower are logical and correct length
  if (!is.vector(test_upper, mode = "logical")) {
    stop("gs_design_npe() or gs_power_npe(): test_upper must be logical!")
  }

  if (!(length(test_upper) == 1 || length(test_upper) == n_analysis)) {
    stop("gs_design_npe() or gs_power_npe(): test_upper must be length 1 or same length as info!")
  }

  # check that final test_upper value is TRUE
  if (!dplyr::last(test_upper)) {
    stop("gs_design_npe(): last value of test_upper must be TRUE!")
  }
}

#' A function to check the arguments `text_lower` used in `gs_power_npe`
#' or `gs_design_npe()` in gsDesign2
#'
#' @param test_lower Test upper or lower.
#' @param n_analysis Number of total analysis.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
#'
#' @examples
#' test_lower <- TRUE
#' check_test_lower(test_lower, 1)
check_test_lower <- function(test_lower, n_analysis) {
  ## Check test_upper and test_lower are logical and correct length
  if (!is.vector(test_lower, mode = "logical")) {
    stop("gs_design_npe() or gs_power_npe(): test_lower must be logical!")
  }

  if (!(length(test_lower) == 1 || length(test_lower) == n_analysis)) {
    stop("gs_design_npe() or gs_power_npe(): test_lower must be length 1 or same length as info!")
  }
}

#' A function to check the arguments `alpha` and `beta` in gsDesign2
#'
#' @param alpha Type I error.
#' @param beta Type II error.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
#'
#' @examples
#' alpha <- 0.025
#' beta <- 0.2
#' check_alpha_beta(alpha, beta)
check_alpha_beta <- function(alpha, beta) {
  if (!is.numeric(alpha)) stop("alpha must be numeric!")
  if (!is.numeric(beta)) stop("beta must be numeric!")
  if (length(alpha) != 1 || length(beta) != 1) stop("alpha and beta must be length 1!")
  if (alpha <= 0 || 1 - beta <= alpha || beta <= 0) stop("must have 0 < alpha < 1 - beta < 1!")
}

#' A function to check the arguments `IF` in gsDesign2
#'
#' @param info_frac Statistical informational fraction.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
#'
#' @examples
#' check_info_frac(1:3 / 3)
check_info_frac <- function(info_frac) {
  msg <- "gs_design_ahr(): info_frac must be a positive
  number or positive increasing sequence on (0, 1] with final value of 1!"
  if (!is.vector(info_frac, mode = "numeric")) stop(msg)
  if (min(info_frac - dplyr::lag(info_frac, def = 0)) <= 0) stop(msg)
  if (max(info_frac) != 1) stop(msg)
}
