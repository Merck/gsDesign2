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

#' @param alpha One-sided Type I error (strictly between 0 and 1).
#' @param power Power (`NULL` to compute power or strictly between 0
#'   and `1 - alpha` otherwise).
#' @param ratio Experimental:Control randomization ratio.
#' @param study_duration Study duration.
#' @inheritParams gs_design_ahr
#'
#' @export
#'
#' @rdname fixed_design
#'
#' @examples
#' # LF method ----
#'
#' # Example 1: given power and compute sample size
#' x <- fixed_design_lf(
#'   alpha = .025, power = .9,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 1),
#'   fail_rate = define_fail_rate(
#'     duration = 100,
#'     fail_rate = log(2) / 12,
#'     hr = .7,
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36
#' )
#' x |> summary()
#'
#' # Example 2: given sample size and compute power
#' x <- fixed_design_lf(
#'   alpha = .025,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 20),
#'   fail_rate = define_fail_rate(
#'     duration = 100,
#'     fail_rate = log(2) / 12,
#'     hr = .7,
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36
#' )
#' x |> summary()
#'
fixed_design_lf <- function(
    alpha = 0.025,
    power = NULL,
    ratio = 1,
    study_duration = 36,
    enroll_rate,
    fail_rate) {
  # Check inputs ----
  check_enroll_rate(enroll_rate)
  check_fail_rate(fail_rate)
  check_enroll_rate_fail_rate(enroll_rate, fail_rate)

  # Save inputs ----
  input <- list(
    alpha = alpha, power = power, ratio = ratio, study_duration = study_duration,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate
  )

  # Generate design ----
  # check if it is stratum
  n_stratum1 <- length(unique(enroll_rate$stratum))
  n_stratum2 <- length(unique(fail_rate$stratum))
  if (n_stratum1 != n_stratum2) {
    stop("The number of strata does not match in the enrollment/failrate.")
  } else {
    n_stratum <- n_stratum1
  }
  if (n_stratum == 1) {
    m <- length(fail_rate$fail_rate)
    lambda_cc <- fail_rate$fail_rate
    etaa <- fail_rate$dropout_rate
    gammaa <- enroll_rate$rate
    rr <- enroll_rate$duration
    if (m == 1) {
      ss <- NULL
    } else {
      ss <- fail_rate$duration[1:(m - 1)]
    }
  } else {
    warning("Lachin-Foulkes is not recommended for stratified designs!")
    temp <- fail_rate |>
      group_by(stratum) |>
      summarize(n_duration = n())
    # calculate the S: duration of piecewise constant event rates
    if (all(temp$n_duration == 1)) {
      ss <- cbind(NULL, NULL)
    } else {
      stratified_duration <- fail_rate |>
        select(stratum, duration) |>
        tidyr::pivot_wider(names_from = stratum, values_from = duration, values_fn = list)
      ss <- do.call(cbind, lapply(stratified_duration, function(x) {
        x |> unlist()
      })) |>
        as.matrix()
    }
    # calculate the lambdaC: event hazard rates for the control group
    stratified_lambdac <- fail_rate |>
      select(stratum, fail_rate) |>
      tidyr::pivot_wider(names_from = stratum, values_from = fail_rate, values_fn = list)
    lambda_cc <- do.call(cbind, lapply(stratified_lambdac, function(x) {
      x |> unlist()
    })) |>
      as.matrix()
    # calculate the eta: dropout hazard rates for the control group
    stratified_eta <- fail_rate |>
      select(stratum, dropout_rate) |>
      tidyr::pivot_wider(names_from = stratum, values_from = dropout_rate, values_fn = list)
    etaa <- do.call(cbind, lapply(stratified_eta, function(x) {
      x |> unlist()
    })) |>
      as.matrix()
    # calculate the gamma: rates of entry by time period (rows) and strata (columns)
    stratified_enroll_rate <- enroll_rate |>
      select(stratum, rate) |>
      tidyr::pivot_wider(names_from = stratum, values_from = rate, values_fn = list)
    gammaa <- do.call(cbind, lapply(stratified_enroll_rate, function(x) {
      x |> unlist()
    })) |>
      as.matrix()
    # calculate the R: duration of time periods for recruitment rates specified in rows of gamma
    stratified_enroll_duration <- enroll_rate |>
      select(stratum, duration) |>
      tidyr::pivot_wider(names_from = stratum, values_from = duration, values_fn = list)
    rr <- do.call(cbind, lapply(stratified_enroll_duration, function(x) {
      x |> unlist()
    })) |>
      as.matrix()
  }
  # calculate the ahr as the hr in nSurv
  dd <- ahr(enroll_rate = enroll_rate, fail_rate = fail_rate, total_duration = study_duration, ratio = ratio)
  # use nSuve to develop the design
  d <- gsDesign::nSurv(
    alpha = alpha, beta = if (is.null(power)) {
      NULL
    } else {
      1 - power
    },
    ratio = ratio, hr = dd$ahr,
    # fail_rate
    lambdaC = lambda_cc,
    S = ss, eta = etaa,
    # enroll_rate
    gamma = gammaa, R = if (n_stratum == 1) {
      rr
    } else {
      rr[, 1]
    },
    T = study_duration, minfup = study_duration - sum(if (n_stratum == 1) {
      rr
    } else {
      rr[, 1]
    })
  )

  # Prepare output ----
  ans <- tibble(
    design = "lf",
    n = d$n,
    event = d$d,
    time = d$T,
    bound = qnorm(1 - alpha),
    alpha = d$alpha,
    power = d$power
  )
  y <- structure(
    list(
      input = input,
      enroll_rate = enroll_rate |> mutate(rate = rate * d$n / sum(enroll_rate$duration * enroll_rate$rate)),
      fail_rate = fail_rate,
      analysis = ans,
      design = "lf"
    ),
    class = "fixed_design",
    design_display = "Lachin and Foulkes",
    title = "Fixed Design under Lachin and Foulkes Method",
    footnote = paste(
      "Power using Lachin and Foulkes method applied using expected",
      "average hazard ratio (AHR) at time of planned analysis."
    )
  )

  return(y)
}
