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

#' Group sequential design using average hazard ratio under non-proportional hazards
#'
#' @param x A design created by either [gs_design_ahr()] or [gs_power_ahr()].
#' @param alpha Type I error for the updated design.
#' @param ustime Default is NULL in which case upper bound spending time is determined by timing.
#' Otherwise, this should be a vector of length k (total number of analyses)
#' with the spending time at each analysis.
#' @param lstime Default is NULL in which case lower bound spending time is determined by timing.
#' Otherwise, this should be a vector of length k (total number of analyses)
#' with the spending time at each analysis.
#' @param event_tbl A data frame with two columns: (1) analysis and (2) event,
#' which represents the events observed at each analysis per piecewise interval.
#' This can be defined via the `pw_observed_event()` function or manually entered.
#' For example, consider a scenario with two intervals in the piecewise model:
#' the first interval lasts 6 months with a hazard ratio (HR) of 1,
#' and the second interval follows with an HR of 0.6.
#' The data frame `event_tbl = data.frame(analysis = c(1, 1, 2, 2), event = c(30, 100, 30, 200))`
#' indicates that 30 events were observed during the delayed effect period,
#' 130 events were observed at the IA, and 230 events were observed at the FA.
#'
#' @return A list with input parameters, enrollment rate, failure rate, analysis, and bound.
#'
#' @export
#'
#' @examples
#' library(gsDesign)
#' library(gsDesign2)
#'
#' alpha <- 0.025
#' beta <- 0.1
#' ratio <- 1
#'
#' # Enrollment
#' enroll_rate <- define_enroll_rate(
#'   duration = c(2, 2, 10),
#'   rate = (1:3) / 3)
#'
#' # Failure and dropout
#' fail_rate <- define_fail_rate(
#'   duration = c(3, Inf), fail_rate = log(2) / 9,
#'   hr = c(1, 0.6), dropout_rate = .0001)
#'
#' # IA and FA analysis time
#' analysis_time <- c(20, 36)
#'
#' # Randomization ratio
#' ratio <- 1
#'
#' # ------------------------------------------------- #
#' # Two-sided asymmetric design,
#' # beta-spending with non-binding lower bound
#' # ------------------------------------------------- #
#' # Original design
#' x <- gs_design_ahr(
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   alpha = alpha, beta = beta, ratio = ratio,
#'   info_scale = "h0_info",
#'   info_frac = NULL, analysis_time = c(20, 36),
#'   upper = gs_spending_bound,
#'   upar = list(sf = sfLDOF, total_spend = alpha),
#'   test_upper = TRUE,
#'   lower = gs_spending_bound,
#'   lpar = list(sf = sfLDOF, total_spend = beta),
#'   test_lower = c(TRUE, FALSE),
#'   binding = FALSE) |> to_integer()
#'
#' planned_event_ia <- x$analysis$event[1]
#' planned_event_fa <- x$analysis$event[2]
#'
#'
#' # Updated design with 190 events observed at IA,
#' # where 50 events observed during the delayed effect.
#' # IA spending = observed events / final planned events, the remaining alpha will be allocated to FA.
#' gs_update_ahr(
#'   x = x,
#'   ustime = c(190 / planned_event_fa, 1),
#'   lstime = c(190 / planned_event_fa, 1),
#'   event_tbl = data.frame(analysis = c(1, 1),
#'                          event = c(50, 140)))
#'
#' # Updated design with 190 events observed at IA, and 300 events observed at FA,
#' # where 50 events observed during the delayed effect.
#' # IA spending = observed events / final planned events, the remaining alpha will be allocated to FA.
#' gs_update_ahr(
#'   x = x,
#'   ustime = c(190 / planned_event_fa, 1),
#'   lstime = c(190 / planned_event_fa, 1),
#'   event_tbl = data.frame(analysis = c(1, 1, 2, 2),
#'                          event = c(50, 140, 50, 250)))
#'
#' # Updated design with 190 events observed at IA, and 300 events observed at FA,
#' # where 50 events observed during the delayed effect.
#' # IA spending = minimal of planned and actual information fraction spending
#' gs_update_ahr(
#'   x = x,
#'   ustime = c(min(190, planned_event_ia) / planned_event_fa, 1),
#'   lstime = c(min(190, planned_event_ia) / planned_event_fa, 1),
#'   event_tbl = data.frame(analysis = c(1, 1, 2, 2),
#'                          event = c(50, 140, 50, 250)))
#'
#' # Alpha is updated to 0.05
#' gs_update_ahr(x = x, alpha = 0.05)
gs_update_ahr <- function(
    x = NULL,
    alpha = NULL,
    ustime = NULL,
    lstime = NULL,
    event_tbl = NULL) {

  # ----------------------------------- #
  #         Check inputs                #
  # ----------------------------------- #
  if (is.null(x)) {
    stop("gs_update_ahr(): please input the original design created either by gs_design_ahr or gs_power_ahr.")
  }

  if (!x$design %in% c("ahr", "wlr")) {
    stop("gs_update_ahr(): the original design must be created either by gs_design_ahr, gs_power_ahr, gs_design_wlr, or gs_power_wlr.")
  }

  # Check if is efficacy only
  one_sided <- all(x$bound$bound == "upper")
  if (one_sided && !is.null(lstime)) {
    stop("gs_update_ahr(): lstime is not needed for one-sided design.")
  }

  # Check if futility bound is fixed. In other words, check if is the provided
  # function to compute lower bounds equivalent to gsDesign2::gs_b()
  gs_b_observed <- try(x$input$lower(par = 4:2, k = 2), silent = TRUE)
  gs_b_expected <- gs_b(par = 4:2, k = 2)
  fixed_futility_bound <- identical(gs_b_observed, gs_b_expected)
  if (fixed_futility_bound && !is.null(lstime)) {
    stop("gs_update_ahr(): lstime is not needed for two-sided design with fixed futility bounds.")
  }

  # ----------------------------------- #
  #         Get parameters              #
  # ----------------------------------- #
  # Get the total number of analyses
  n_analysis <- nrow(x$analysis)

  # Get the updated alpha
  if (is.null(alpha) && !is.null(x$input$alpha)) {
    alpha_update <- x$input$alpha
  } else if (is.null(alpha) && is.null(x$input$alpha)) {
    alpha_update <- x$input$upar$total_spend
  } else {
    alpha_update <- alpha
  }

  # ----------------------------------- #
  #         Scenario 1:                 #
  #       At design stage,              #
  #       with different alpha          #
  # ----------------------------------- #
  # If users do not input observed data, nor event_tbl
  # which means they update design with a different value of alpha
  if (is.null(event_tbl)) {

    # Check if ustime and lstime matches the spending time of the original design
    if (!is.null(ustime) && any(ustime != x$input$upar$timing)) {
      stop("gs_update_ahr(): timing specificed in the original design (x) is different from the updated timing (ustime, lstime).")
    }

    # Update alpha ---
    upar_update  <- x$input$upar
    lpar_update <- x$input$lpar
    upar_update$total_spend <- alpha_update

    # Update boundaries and crossing prob under H0 ----
    x_updated_h0 <- gs_power_npe(theta = 0,
                                 theta0 = 0,
                                 theta1 = x$analysis$theta,
                                 # statistical information at all analyses for input `theta`
                                 info = x$analysis$info0,
                                 # statistical information under null hypothesis,
                                 # impacts null hypothesis bound calculation.
                                 info0 = x$analysis$info0,
                                 # statistical information under hypothesis used for
                                 # futility bound calculation if different from `info`
                                 # impacts futility hypothesis bound calculation.
                                 info1 = x$analysis$info,
                                 info_scale = x$input$info_scale,
                                 upper = x$input$upper, upar = upar_update,
                                 test_upper = x$input$test_upper,
                                 lower = x$input$lower, lpar = x$input$lpar,
                                 test_lower = x$input$test_lower,
                                 binding = x$input$binding)

    # Update boundaries and crossing prob under H1 ----
    x_updated_h1 <- gs_power_npe(theta = x$analysis$theta,
                                 theta0 = 0,
                                 theta1 = x$analysis$theta,
                                 # statistical information under null hypothesis,
                                 # impacts null hypothesis bound calculation.
                                 info0 = x$analysis$info0,
                                 # statistical information at all analyses for input `theta`
                                 info = x$analysis$info,
                                 # statistical information under hypothesis used for
                                 # futility bound calculation if different from `info`
                                 # impacts futility hypothesis bound calculation.
                                 info1 = NULL,
                                 info_scale = x$input$info_scale,
                                 upper = x$input$upper, upar = upar_update,
                                 test_upper = x$input$test_upper,
                                 lower = x$input$lower, lpar = x$input$lpar,
                                 test_lower = x$input$test_lower,
                                 binding = x$input$binding)
  } else {
    # ----------------------------------- #
    #         Scenario 2:                 #
    #       At analysis stage,            #
    #       with different alpha          #
    # ----------------------------------- #
    # Calculate the blinded estimation of AHR
    blinded_est <- NULL
    observed_event <- NULL
    for (i in 1:n_analysis) {
      if (!(i %in% event_tbl$analysis)) {
        # if there is no observed data at analysis i,
        # for example, we only observed IA data and FA data is unavailable yet
        blinded_est_new <- data.frame(event = x$analysis$event[i],
                                      ahr = x$analysis$ahr[i],
                                      theta = x$analysis$theta[i],
                                      info0 = x$analysis$info0[i])
        event_new <- x$analysis$event[i]
      } else {
        q_e <- x$input$ratio / (1 + x$input$ratio)
        event_i <- event_tbl$event[event_tbl$analysis == i]
        hr_i <- x$fail_rate$hr
        event_new <- sum(event_i)

        blinded_est_new <- data.frame(event = sum(event_i),
                                      theta = -sum(log(hr_i) * event_i) / sum(event_i),
                                      info0 = sum(event_i) * (1 - q_e) * q_e)
        blinded_est_new$ahr <- exp(-blinded_est_new$theta)
      }

      blinded_est <- rbind(blinded_est, blinded_est_new)
      observed_event <- c(observed_event, event_new)
    }

    # Update timing
    upar_update  <- x$input$upar
    lpar_update <- x$input$lpar

    if (one_sided) {
      upar_update$timing <- ustime
    } else {
      upar_update$timing <- ustime
      if(is.list(x$input$lpar)) {
        lpar_update$timing <- lstime
      }
    }

    upar_update$total_spend <- alpha_update

    # Update boundaries and crossing prob under H0
    x_updated_h0 <- gs_power_npe(theta = 0,
                                 theta0 = 0,
                                 theta1 = blinded_est$theta,
                                 info = blinded_est$info0,
                                 info_scale = "h0_info",
                                 upper = x$input$upper, upar = upar_update,
                                 test_upper = x$input$test_upper,
                                 lower = x$input$lower, lpar = lpar_update,
                                 test_lower = x$input$test_lower,
                                 binding = x$input$binding)

    # Update boundaries and crossing prob under H1
    x_updated_h1 <- gs_power_npe(theta = blinded_est$theta,
                                 theta0 = 0,
                                 theta1 = blinded_est$theta,
                                 info = blinded_est$info0,
                                 info_scale = "h0_info",
                                 upper = x$input$upper, upar = upar_update,
                                 test_upper = x$input$test_upper,
                                 lower = x$input$lower, lpar = lpar_update,
                                 test_lower = x$input$test_lower,
                                 binding = x$input$binding)
  }

  # ----------------------------------- #
  #         Tidy outputs                #
  # ----------------------------------- #
  ans <- list()

  ans$design <- x$design

  ans$enroll_rate <- x$enroll_rate

  ans$fail_rate <- x$fail_rate

  suppressMessages(
    ans$bound <- x_updated_h0 |>
      select(analysis, bound, z, probability, info0) |>
      rename(probability0 = probability) |>
      mutate(`~hr at bound` = exp(-z / sqrt(info0)),
             `nominal p` = pnorm(-z)) |>
      left_join(x_updated_h1 |>
                  select(analysis, bound, z, probability)) |>
      select(analysis, bound, probability, probability0, z, `~hr at bound`, `nominal p`)
  )

  ans$analysis <- data.frame(
    analysis = 1:n_analysis,
    time = x$analysis$time,
    n = x$analysis$n,
    event = if (is.null(event_tbl)) {
      x$analysis$event
    } else {
      observed_event
    },
    ahr = if (is.null(event_tbl)) {
      x$analysis$ahr
    } else {
      exp(-blinded_est$theta)
    },
    theta = if (is.null(event_tbl)) {
      x$analysis$theta
    } else {
      blinded_est$theta
    },
    info = if (is.null(event_tbl)) {
      x$analysis$info
    } else {
      blinded_est$info0
    },
    info0 = if (is.null(event_tbl)) {
      x$analysis$info0
    } else {
      blinded_est$info0
    },
    info_frac = if (is.null(event_tbl)) {
      x$analysis$info_frac
    } else {
      upar_update$timing
    },
    info_frac0 = if (is.null(event_tbl)) {
      x$analysis$info_frac0
    } else {
      observed_event / max(observed_event)
    }
  )

  class(ans) <- "gs_design"
  attr(ans, "binding") <- attr(x, "binding")
  attr(ans, "uninteger_is_from") <- attr(x, "uninteger_is_from")
  attr(ans, "updated_design") <- TRUE

  return(ans)
}
