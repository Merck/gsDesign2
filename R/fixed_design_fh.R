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

#' @inheritParams gs_design_wlr
#' @inheritParams gs_power_wlr
#' @param power Power (`NULL` to compute power or strictly between 0
#'   and `1 - alpha` otherwise).
#' @param study_duration Study duration.
#' @param rho test parameter in Fleming-Harrington method.
#' @param gamma test parameter in Fleming-Harrington method.
#'
#' @importFrom dplyr filter
#'
#' @export
#'
#' @rdname fixed_design
#'
#' @examples
#' # WLR test with FH weights ----
#' library(dplyr)
#'
#' # Example 1: given power and compute sample size
#' x <- fixed_design_fh(
#'   alpha = .025, power = .9,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 1),
#'   fail_rate = define_fail_rate(
#'     duration = c(4, 100),
#'     fail_rate = log(2) / 12,
#'     hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36,
#'   rho = 1, gamma = 1
#' )
#' x %>% summary()
#'
#' # Example 2: given sample size and compute power
#' x <- fixed_design_fh(
#'   alpha = .025,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 20),
#'   fail_rate = define_fail_rate(
#'     duration = c(4, 100),
#'     fail_rate = log(2) / 12,
#'     hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36,
#'   rho = 1, gamma = 1
#' )
#' x %>% summary()
#'
fixed_design_fh <- function(
    alpha = 0.025,
    power = NULL,
    ratio = 1,
    study_duration = 36,
    enroll_rate,
    fail_rate,
    rho = 0,
    gamma = 0) {
  # Check inputs ----
  check_enroll_rate(enroll_rate)
  check_fail_rate(fail_rate)
  check_enroll_rate_fail_rate(enroll_rate, fail_rate)
  # check test parameters, like rho, gamma
  if (length(rho) > 1) {
    stop("fixed_design_fh: multiple rho can not be used in Fleming-Harrington method!")
  }
  if (length(gamma) > 1) {
    stop("fixed_design_fh: multiple gamma can not be used in Fleming-Harrington method!")
  }

  # Save inputs ----
  input <- list(
    alpha = alpha, power = power, ratio = ratio, study_duration = study_duration,
    rho = rho, gamma = gamma,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate
  )

  # Generate design ----
  weight <- function(x, arm0, arm1) {
    wlr_weight_fh(x, arm0, arm1, rho = rho, gamma = gamma)
  }
  if (is.null(power)) {
    d <- gs_power_wlr(
      upar = qnorm(1 - alpha), lpar = -Inf,
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      ratio = ratio,
      weight = weight,
      analysis_time = study_duration,
      event = NULL
    )
  } else {
    d <- gs_design_wlr(
      alpha = alpha, beta = 1 - power,
      upar = qnorm(1 - alpha), lpar = -Inf,
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      ratio = ratio,
      weight = weight,
      analysis_time = study_duration
    )
  }
  ans <- tibble::tibble(
    design = "fh",
    n = d$analysis$n,
    event = d$analysis$event,
    time = d$analysis$time,
    bound = (d$bound %>% filter(bound == "upper"))$z,
    alpha = alpha,
    power = (d$bound %>% filter(bound == "upper"))$probability
  )
  y <- list(
    input = input, enroll_rate = d$enroll_rate, fail_rate = d$fail_rate,
    analysis = ans,
    design = "fh", design_par = list(rho = rho, gamma = gamma)
  )
  class(y) <- c("fixed_design", class(y))
  return(y)
}
