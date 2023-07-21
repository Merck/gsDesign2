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

#' Fixed design using Magirr-Burman method
#'
#' Computes fixed design sample size (given power) or power (given sample size)
#' for Magirr-Burman method.
#' Returns a list with a basic summary.
#'
#' @inheritParams gs_design_wlr
#' @inheritParams gs_power_wlr
#' @param power Power (`NULL` to compute power or strictly between 0
#'   and `1 - alpha` otherwise).
#' @param ratio Experimental:Control randomization ratio.
#' @param study_duration Study duration.
#' @param tau Test parameter of Magirr-Burman method.
#'
#' @return A table.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Example 1: given power and compute sample size
#' x <- fixed_design_mb(
#'   alpha = .025, power = .9,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 1),
#'   fail_rate = define_fail_rate(
#'     duration = c(4, 100),
#'     fail_rate = log(2) / 12,
#'     hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36,
#'   tau = 4
#' )
#' x %>% summary()
#'
#' # Example 2: given sample size and compute power
#' x <- fixed_design_mb(
#'   alpha = .025,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 20),
#'   fail_rate = define_fail_rate(
#'     duration = c(4, 100),
#'     fail_rate = log(2) / 12,
#'     hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36,
#'   tau = 4
#' )
#' x %>% summary()
fixed_design_mb <- function(
    alpha = 0.025,
    power = NULL,
    ratio = 1,
    study_duration = 36,
    enroll_rate,
    fail_rate,
    tau = 6) {
  # --------------------------------------------- #
  #     check inputs                              #
  # --------------------------------------------- #
  check_enroll_rate(enroll_rate)
  check_fail_rate(fail_rate)
  check_enroll_rate_fail_rate(enroll_rate, fail_rate)
  if (length(tau) > 1) {
    stop("fixed_design: multiple tau can not be used in Magirr-Burman method!")
  }
  # ------------------------- #
  #     save inputs           #
  # ------------------------- #
  input <- list(
    alpha = alpha, power = power, ratio = ratio, study_duration = study_duration,
    tau = tau,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate
  )
  # ------------------------- #
  #     generate design       #
  # ------------------------- #
  weight <- function(x, arm0, arm1) {
    wlr_weight_fh(x, arm0, arm1, rho = -1, gamma = 0, tau = tau)
  }
  if (is.null(power)) {
    d <- gs_power_wlr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      ratio = 1,
      weight = weight,
      upper = gs_b, upar = qnorm(1 - alpha),
      lower = gs_b, lpar = -Inf,
      analysis_time = study_duration,
      event = NULL
    )
  } else {
    d <- gs_design_wlr(
      alpha = alpha,
      beta = 1 - power,
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      ratio = 1,
      weight = weight,
      upper = gs_b, upar = qnorm(1 - alpha),
      lower = gs_b, lpar = -Inf,
      analysis_time = study_duration
    )
  }
  # get the output of MB
  ans <- tibble::tibble(
    design = "mb",
    n = d$analysis$n,
    event = d$analysis$event,
    time = d$analysis$time,
    bound = (d$bound %>% filter(bound == "upper"))$z,
    alpha = alpha,
    power = (d$bound %>% filter(bound == "upper"))$probability
  )
  y <- list(
    input = input, enroll_rate = d$enroll_rate, fail_rate = d$fail_rate, analysis = ans,
    design = "mb", design_par = list(tau = tau)
  )
  class(y) <- c("fixed_design", class(y))
  return(y)
}
