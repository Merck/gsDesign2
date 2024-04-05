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

#' Group sequential design using average hazard ratio under non-proportional hazards
#'
#' @param x A original design created by either \code{gs_design_ahr} or \code{gs_power_ahr}.
#' @param alpha Alpha for the updated design.
#' @param ia_alpha_spending Alpha spending strategy for interim analyses,
#' either `"actual_info_frac"` (default), or `"min_of_planned_and_actual_info_frac"`.
#' @param fa_alpha_spending Alpha spending strategy for final analysis,
#' either `"info_frac"` or `"full_alpha"` (default).
#' @param observed_data a list of observed datasets by analyses.
#'
#' @return A list with input parameters, enrollment rate, analysis, and bound.
#'
#' @importFrom dplyr mutate left_join select rename
#' @importFrom gsDesign gsDesign sfLDOF
#'
#' @export
#'
#' @examples
#' library(gsDesign)
#' library(gsDesign2)
#' library(dplyr)
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
#' # Example A: one-sided design (efficacy only) ----
#'
#' # Original design
#' upper <- gs_spending_bound
#' upar <- list(sf = sfLDOF, total_spend = alpha)
#' x <- gs_design_ahr(
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   alpha = alpha, beta = beta, ratio = ratio,
#'   info_scale = "h0_info",
#'   info_frac = NULL,
#'   analysis_time = c(20, 36),
#'   upper = gs_spending_bound, upar = upar,
#'   lower = gs_b, lpar = rep(-Inf, 2),
#'   test_upper = TRUE, test_lower = FALSE) |> to_integer()
#'
#' # Observed dataset at IA and FA
#' set.seed(123)
#'
#' observed_data <- simtrial::sim_pw_surv(
#'   n = x$analysis$n[x$analysis$analysis == 2],
#'   stratum = data.frame(stratum = "All", p = 1),
#'   block = c(rep("control", 2), rep("experimental", 2)),
#'   enroll_rate = x$enroll_rate,
#'   fail_rate = (fail_rate |> simtrial::to_sim_pw_surv())$fail_rate,
#'   dropout_rate = (fail_rate |> simtrial::to_sim_pw_surv())$dropout_rate)
#'
#' observed_data_ia <- observed_data |> simtrial::cut_data_by_date(x$analysis$time[1])
#' observed_data_fa <- observed_data |> simtrial::cut_data_by_date(x$analysis$time[2])
#'
#' # Example A1 ----
#' # IA spending = observed events / final planned events
#  # the remaining alpha will be allocated to FA.
#' gs_update_ahr(
#'   x = x,
#'   ia_alpha_spending = "actual_info_frac",
#'   fa_alpha_spending = "full_alpha",
#'   observed_data = list(observed_data_ia, observed_data_fa))
#'
#' # Example A2 ----
#' # IA, FA spending = observed events / final planned events
#' gs_update_ahr(
#'   x = x,
#'   ia_alpha_spending = "actual_info_frac",
#'   fa_alpha_spending = "info_frac",
#'   observed_data = list(observed_data_ia, observed_data_fa))
#'
#' # Example A3 ----
#' # IA spending = min(observed events, planned events) / final planned events
#  # the remaining alpha will be allocated to FA.
#' gs_update_ahr(
#'   x = x,
#'   ia_alpha_spending = "min_of_planned_and_actual_info_frac",
#'   fa_alpha_spending = "full_alpha",
#'   observed_data = list(observed_data_ia, observed_data_fa))
#'
#' # Example A4 ----
#' # IA spending = min(observed events, planned events) / final planned events
#' gs_update_ahr(
#'   x = x,
#'   ia_alpha_spending = "min_of_planned_and_actual_info_frac",
#'   fa_alpha_spending = "info_frac",
#'   observed_data = list(observed_data_ia, observed_data_fa))
#'
#' # Example A5 ----
#' # IA spending = min(observed events, planned events) / final planned events
#' # alpha is upadted to 0.05
#' gs_update_ahr(
#'   x = x,
#'   alpha = 0.05,
#'   ia_alpha_spending = "min_of_planned_and_actual_info_frac",
#'   fa_alpha_spending = "info_frac",
#'   observed_data = list(observed_data_ia, observed_data_fa))
#'
#' # Example B: Two-sided asymmetric design,
#' # beta-spending with non-binding lower bound ----
#'
#' # Original design
#' x <- gs_design_ahr(
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   alpha = alpha, beta = beta, ratio = ratio,
#'   info_scale = "h0_info",
#'   info_frac = NULL, analysis_time = c(20, 36),
#'   upper = gs_spending_bound,
#'   upar = list(sf = sfLDOF, total_spend = alpha, param = NULL),
#'   test_upper = TRUE,
#'   lower = gs_spending_bound,
#'   lpar = list(sf = sfLDOF, total_spend = beta, param = NULL),
#'   test_lower = c(TRUE, FALSE),
#'   binding = FALSE) |> to_integer()
#'
#' # Example B1 ----
#' # IA spending = observed events / final planned events
#' # the remaining alpha will be allocated to FA.
#' gs_update_ahr(
#'   x = x,
#'   ia_alpha_spending = "actual_info_frac",
#'   fa_alpha_spending = "full_alpha",
#'   observed_data = list(observed_data_ia, observed_data_fa))
#'
#' # Example B2 ----
#' # IA, FA spending = observed events / final planned events
#' gs_update_ahr(
#'   x = x,
#'   ia_alpha_spending = "actual_info_frac",
#'   fa_alpha_spending = "info_frac",
#'   observed_data = list(observed_data_ia, observed_data_fa))
#'
#' # Example B3 ----
#' # IA spending = min(observed events, planned events) / final planned events
#  # the remaining alpha will be allocated to FA.
#' gs_update_ahr(
#'   x = x,
#'   ia_alpha_spending = "min_of_planned_and_actual_info_frac",
#'   fa_alpha_spending = "full_alpha",
#'   observed_data = list(observed_data_ia, observed_data_fa))
#'
#' # Example B4 ----
#' # IA spending = min(observed events, planned events) / final planned events
#' gs_update_ahr(
#'   x = x,
#'   ia_alpha_spending = "min_of_planned_and_actual_info_frac",
#'   fa_alpha_spending = "info_frac",
#'   observed_data = list(observed_data_ia, observed_data_fa))
#'
#' # Example A5 ----
#' # IA spending = min(observed events, planned events) / final planned events
#' # alpha is upadted to 0.05
#' gs_update_ahr(
#'   x = x,
#'   alpha = 0.05,
#'   ia_alpha_spending = "min_of_planned_and_actual_info_frac",
#'   fa_alpha_spending = "info_frac",
#'   observed_data = list(observed_data_ia, observed_data_fa))
gs_update_ahr <- function(
    x = NULL,
    alpha = NULL,
    ia_alpha_spending = c("actual_info_frac", "min_of_planned_and_actual_info_frac"),
    fa_alpha_spending = c("full_alpha", "info_frac"),
    observed_data = NULL) {

  # Initialization ----
  ia_alpha_spending <- match.arg(ia_alpha_spending)
  fa_alpha_spending <- match.arg(fa_alpha_spending)
  one_sided <- all(x$bound$bound == "upper")
  # Check inputs ----
  if (is.null(x)) {
    stop("gs_update_ahr() please input the original design created either by gs_design_ahr or gs_power_ahr.")
  }

  if (is.null(alpha) && !is.null(x$input$alpha)) {
    alpha_update <- x$input$alpha
  } else if (is.null(alpha) && is.null(x$input$alpha)) {
    alpha_update <- x$input$upar$total_spend
  } else {
    alpha_update <- alpha
  }

  if (!("ahr" %in% class(x))) {
    stop("gs_update_ahr() the original design should be created either by gs_design_ahr or gs_power_ahr.")
  }

  # Get the total number of analyses ---
  n_analysis <- length(observed_data)

  # Calculate the blinded estimation of AHR ----
  fr_duration <- x$input$fail_rate$duration
  fr_hr <- x$input$fail_rate$hr
  all_t <- sort(c(fr_duration, x$analysis$time))

  if (is.infinite(max(x$input$fail_rate$duration))) {
    hr_interval <- cumsum(c(fr_duration[-length(fr_duration)], max(x$analysis$time) + 50))
  } else {
    hr_interval <- cumsum(fr_duration)
  }

  pw_hr <- stepfun(x = hr_interval, y = c(fr_hr, last(fr_hr)), right = TRUE)

  blinded_est <- NULL
  observed_event <- NULL
  for (i in 1:n_analysis) {
    blinded_est_new <- ahr_blinded(surv = survival::Surv(time = observed_data[[i]]$tte,
                                                         event = observed_data[[i]]$event),
                                   intervals = all_t[all_t <= x$analysis$time[i]],
                                   hr = pw_hr(all_t[all_t <= x$analysis$time[i]]),
                                   ratio = x$input$ratio)
    blinded_est <- rbind(blinded_est, blinded_est_new)
    observed_event <- c(observed_event, sum(observed_data[[i]]$event))
  }

  # Update timing ---
  upar_update  <- x$input$upar
  lpar_update <- x$input$lpar
  upar_update$total_spend <- alpha_update

  if (ia_alpha_spending == "actual_info_frac" && fa_alpha_spending == "full_alpha") {
    upar_update$timing <- observed_event / last(x$analysis$event)
    upar_update$timing[n_analysis] <- 1
  } else if (ia_alpha_spending == "actual_info_frac" && fa_alpha_spending == "info_frac") {
    upar_update$timing <- observed_event / last(x$analysis$event)
  } else if (ia_alpha_spending == "min_of_planned_and_actual_info_frac" && fa_alpha_spending == "full_alpha") {
    upar_update$timing <- pmin(observed_event, x$analysis$event) / last(x$analysis$event)
    upar_update$timing[n_analysis] <- 1
  } else if (ia_alpha_spending == "min_of_planned_and_actual_info_frac" && fa_alpha_spending == "info_frac") {
    upar_update$timing <- pmin(observed_event, x$analysis$event) / last(x$analysis$event)
  }

  if (!one_sided) {
    lpar_update$timing <- upar_update$timing
  }

  # Update boundaries and crossing prob under H0 ---
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

  # Update boundaries and crossing prob under H1 ---
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

  # Tidy outputs
  ans <- list()

  ans$enroll_rate <- x$enroll_rate

  ans$fail_rate <- x$fail_rate

  ans$bound <- x_updated_h0 %>%
    select(analysis, bound, z, probability, info0) %>%
    rename(probability0 = probability) %>%
    mutate(`~hr at bound` = exp(-z / sqrt(info0)),
           `nominal p` = pnorm(-z)) %>%
    left_join(
      x_updated_h1 %>% select(analysis, bound, z, probability)
    ) %>%
    select(analysis, bound, probability, probability0, z, `~hr at bound`, `nominal p`)

  ans$analysis <- data.frame(analysis = 1:n_analysis,
                             time = x$analysis$time,
                             n = x$analysis$n,
                             event = observed_event,
                             ahr = exp(-blinded_est$theta),
                             theta = blinded_est$theta,
                             info = blinded_est$info0,
                             info0 = blinded_est$info0,
                             info_frac = upar_update$timing)

  class(ans) <- c(class(x), "updated_design")

  return(ans)
}
