#  Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
#  All rights reserved.
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

#' Information and effect size for weighted log-rank test
#'
#' Based on piecewise enrollment rate, failure rate, and dropout rates computes
#' approximate information and effect size using an average hazard ratio model.
#'
#' @inheritParams ahr
#' @param fail_rate Failure and dropout rates.
#' @param ratio Experimental:Control randomization ratio.
#' @param event Targeted minimum events at each analysis.
#' @param analysis_time Targeted minimum study duration at each analysis.
#' @param weight Weight of weighted log rank test:
#'   - `"1"` = unweighted.
#'   - `"n"` = Gehan-Breslow.
#'   - `"sqrtN"` = Tarone-Ware.
#'   - `"FH_p[a]_q[b]"` = Fleming-Harrington with p=a and q=b.
#' @param approx Approximate estimation method for Z statistics.
#'   - `"event_driven"` = only work under proportional hazard model with log rank test.
#'   - `"asymptotic"`.
#' @param interval An interval that is presumed to include the time at which
#'   expected event count is equal to targeted event.
#'
#' @return A tibble with columns Analysis, Time, N, Events, AHR, delta, sigma2,
#'   theta, info, info0.
#'   `info` and `info0` contain statistical information under H1, H0, respectively.
#'   For analysis `k`, `Time[k]` is the maximum of `analysis_time[k]` and the
#'   expected time required to accrue the targeted `event[k]`.
#'   `AHR` is the expected average hazard ratio at each analysis.
#'
#' @details
#' The [ahr()] function computes statistical information at targeted event times.
#' The [expected_time()] function is used to get events and average HR at
#' targeted `analysis_time`.
#'
#' @importFrom utils tail
#'
#' @export
#'
#' @examples
#' library(gsDesign2)
#'
#' # Set enrollment rates
#' enroll_rate <- define_enroll_rate(duration = 12, rate = 500 / 12)
#'
#' # Set failure rates
#' fail_rate <- define_fail_rate(
#'   duration = c(4, 100),
#'   fail_rate = log(2) / 15, # median survival 15 month
#'   hr = c(1, .6),
#'   dropout_rate = 0.001
#' )
#'
#' # Set the targeted number of events and analysis time
#' event <- c(30, 40, 50)
#' analysis_time <- c(10, 24, 30)
#'
#' gs_info_wlr(
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   event = event, analysis_time = analysis_time
#' )
gs_info_wlr <- function(
    enroll_rate = define_enroll_rate(
      duration = c(2, 2, 10),
      rate = c(3, 6, 9)
    ),
    fail_rate = define_fail_rate(
      duration = c(3, 100),
      fail_rate = log(2) / c(9, 18),
      hr = c(.9, .6),
      dropout_rate = .001
    ),
    ratio = 1, # Experimental:Control randomization ratio
    event = NULL, # Event at analyses
    analysis_time = NULL, # Times of analyses
    weight = wlr_weight_fh,
    approx = "asymptotic",
    interval = c(.01, 100)) {
  if (is.null(analysis_time) && is.null(event)) {
    stop("gs_info_wlr(): One of event and analysis_time must be a numeric value or vector with increasing values!")
  }

  # Obtain Analysis time
  avehr <- NULL
  if (!is.null(analysis_time)) {
    avehr <- ahr(
      enroll_rate = enroll_rate, fail_rate = fail_rate,
      ratio = ratio, total_duration = analysis_time
    )
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

  time <- avehr$time

  # Create Arm object
  gs_arm <- gs_create_arm(enroll_rate, fail_rate, ratio)
  arm0 <- gs_arm$arm0
  arm1 <- gs_arm$arm1

  # Randomization ratio
  p0 <- arm0$size / (arm0$size + arm1$size)
  p1 <- 1 - p0

  # Null arm
  arm_null <- arm0
  arm_null$surv_scale <- p0 * arm0$surv_scale + p1 * arm1$surv_scale

  arm_null1 <- arm_null
  arm_null1$size <- arm1$size

  delta <- c() # delta of effect size in each analysis
  sigma2_h1 <- c() # sigma square of effect size in each analysis under null
  sigma2_h0 <- c() # sigma square of effect size in each analysis under alternative
  p_event <- c() # probability of events in each analysis
  p_subject <- c() # probability of subjects enrolled
  num_log_ahr <- c()
  dem_log_ahr <- c()

  # Used to calculate average hazard ratio
  arm01 <- arm0
  arm01$size <- 1
  arm11 <- arm1
  arm11$size <- 1

  for (i in seq_along(time)) {
    t <- time[i]
    p_event[i] <- p0 * prob_event.arm(arm0, tmax = t) + p1 * prob_event.arm(arm1, tmax = t)
    p_subject[i] <- p0 * npsurvSS::paccr(t, arm0) + p1 * npsurvSS::paccr(t, arm1)
    delta[i] <- gs_delta_wlr(arm0, arm1, tmax = t, weight = weight, approx = approx)
    num_log_ahr[i] <- gs_delta_wlr(arm01, arm11, tmax = t, weight = weight, approx = approx)
    dem_log_ahr[i] <- gs_delta_wlr(arm01, arm11,
      tmax = t, weight = weight,
      approx = "generalized_schoenfeld", normalization = TRUE
    )

    sigma2_h1[i] <- gs_sigma2_wlr(arm0, arm1, tmax = t, weight = weight, approx = approx)
    sigma2_h0[i] <- gs_sigma2_wlr(arm_null, arm_null1, tmax = t, weight = weight, approx = approx)
  }

  n <- tail(avehr$event / p_event, 1) * p_subject
  theta <- (-delta) / sigma2_h1
  data.frame(
    analysis = seq_along(time),
    time = time,
    n = n,
    event = avehr$event,
    ahr = exp(num_log_ahr / dem_log_ahr),
    delta = delta,
    sigma2 = sigma2_h1,
    theta = theta,
    info = sigma2_h1 * n,
    info0 = sigma2_h0 * n
  )
}
