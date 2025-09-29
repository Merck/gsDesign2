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

#' Group sequential design power using weighted log rank test under non-proportional hazards
#'
#' @inheritParams gs_design_wlr
#' @inheritParams gs_power_ahr
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Compute information and effect size for Weighted Log-rank test using \code{gs_info_wlr()}.
#'    \item Compute group sequential bound computation with non-constant effect using \code{gs_power_npe()}.
#'    \item Combine information and effect size and power and return a
#'    tibble  with columns Analysis, Bound, Time, Events, Z, Probability, AHR,  theta, info, and info0.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @export
#'
#' @return A list with input parameters, enrollment rate,
#' analysis, and bound.
#'
#' @examples
#' library(gsDesign)
#' library(gsDesign2)
#'
#' # set enrollment rates
#' enroll_rate <- define_enroll_rate(duration = 12, rate = 500 / 12)
#'
#' # set failure rates
#' fail_rate <- define_fail_rate(
#'   duration = c(4, 100),
#'   fail_rate = log(2) / 15, # median survival 15 month
#'   hr = c(1, .6),
#'   dropout_rate = 0.001
#' )
#'
#' # set the targeted number of events and analysis time
#' target_events <- c(30, 40, 50)
#' target_analysisTime <- c(10, 24, 30)
#'
#' # Example 1 ----
#' \donttest{
#' # fixed bounds and calculate the power for targeted number of events
#' gs_power_wlr(
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   event = target_events,
#'   analysis_time = NULL,
#'   upper = gs_b,
#'   upar = gsDesign(
#'     k = length(target_events),
#'     test.type = 1,
#'     n.I = target_events,
#'     maxn.IPlan = max(target_events),
#'     sfu = sfLDOF,
#'     sfupar = NULL
#'   )$upper$bound,
#'   lower = gs_b,
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' )
#' }
#' # Example 2 ----
#' # fixed bounds and calculate the power for targeted analysis time
#' \donttest{
#' gs_power_wlr(
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   event = NULL,
#'   analysis_time = target_analysisTime,
#'   upper = gs_b,
#'   upar = gsDesign(
#'     k = length(target_events),
#'     test.type = 1,
#'     n.I = target_events,
#'     maxn.IPlan = max(target_events),
#'     sfu = sfLDOF,
#'     sfupar = NULL
#'   )$upper$bound,
#'   lower = gs_b,
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' )
#' }
#' # Example 3 ----
#' # fixed bounds and calculate the power for targeted analysis time & number of events
#' \donttest{
#' gs_power_wlr(
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   event = target_events,
#'   analysis_time = target_analysisTime,
#'   upper = gs_b,
#'   upar = gsDesign(
#'     k = length(target_events),
#'     test.type = 1,
#'     n.I = target_events,
#'     maxn.IPlan = max(target_events),
#'     sfu = sfLDOF,
#'     sfupar = NULL
#'   )$upper$bound,
#'   lower = gs_b,
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' )
#' }
#' # Example 4 ----
#' # spending bounds and calculate the power for targeted number of events
#' \donttest{
#' gs_power_wlr(
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   event = target_events,
#'   analysis_time = NULL,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2)
#' )
#' }
#' # Example 5 ----
#' # spending bounds and calculate the power for targeted analysis time
#' \donttest{
#' gs_power_wlr(
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   event = NULL,
#'   analysis_time = target_analysisTime,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2)
#' )
#' }
#' # Example 6 ----
#' # spending bounds and calculate the power for targeted analysis time & number of events
#' \donttest{
#' gs_power_wlr(
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   event = target_events,
#'   analysis_time = target_analysisTime,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2)
#' )
#' }
gs_power_wlr <- function(enroll_rate = define_enroll_rate(duration = c(2, 2, 10), rate = c(3, 6, 9)),
                         fail_rate = tibble(
                           stratum = "All", duration = c(3, 100), fail_rate = log(2) / c(9, 18),
                           hr = c(.9, .6), dropout_rate = rep(.001, 2)
                         ),
                         event = c(30, 40, 50),
                         analysis_time = NULL,
                         binding = FALSE,
                         h1_spending = TRUE,
                         upper = gs_spending_bound,
                         lower = gs_spending_bound,
                         upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
                         lpar = list(sf = gsDesign::sfLDOF, total_spend = NULL),
                         test_upper = TRUE,
                         test_lower = TRUE,
                         ratio = 1,
                         weight = "logrank",
                         info_scale = c("h0_h1_info", "h0_info", "h1_info"),
                         approx = "asymptotic",
                         r = 18,
                         tol = 1e-6,
                         interval = c(.01, 1000),
                         integer = FALSE) {
  # check of inputted sample size
  input_sample_size <- sum(enroll_rate$rate * enroll_rate$duration)

  # Check if user input the total spending for futility,
  # if they use spending function for futility
  if (identical(lower, gs_spending_bound) && is.null(lpar$total_spend)) {
    stop("gs_power_wlr: please input the total_spend to the futility spending function.")
  }

  # get the number of analysis
  n_analysis <- max(length(event), length(analysis_time), na.rm = TRUE)
  # get the info_scale
  info_scale <- match.arg(info_scale)

  # Calculate the asymptotic variance and statistical information ----
  x <- gs_info_wlr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = ratio,
    event = event,
    weight = weight,
    analysis_time = analysis_time,
    interval = interval
  )

  # set up H1 spending
  if (h1_spending) {
    theta1 <- x$theta
    info1 <- x$info
  } else {
    theta1 <- 0
    info1 <- x$info0
  }

  # Given the above statistical information calculate the power ----
  y_h1 <- gs_power_npe(
    theta = x$theta,
    theta0 = 0,
    theta1 = theta1,
    info = x$info,
    info0 = x$info0,
    info1 = info1,
    info_scale = info_scale,
    binding = binding,
    upper = upper,
    lower = lower,
    upar = upar,
    lpar = lpar,
    test_upper = test_upper,
    test_lower = test_lower,
    r = r,
    tol = tol
  )

  y_h0 <- gs_power_npe(
    theta = 0,
    theta0 = 0,
    theta1 = theta1,
    info = x$info0,
    info0 = x$info0,
    info1 = info1,
    info_scale = info_scale,
    binding = binding,
    upper = upper,
    lower = lower,
    upar = upar,
    lpar = lpar,
    test_upper = test_upper,
    test_lower = test_lower,
    r = r,
    tol = tol
  )

  # Get bounds to output ----
  suppressMessages(
    bounds <- y_h0 |>
      select(analysis, bound, z, probability) |>
      rename(probability0 = probability) |>
      left_join(
        x |> select(analysis, event)
      ) |>
      mutate(
        `~hr at bound` = gsDesign::zn2hr(z = z, n = event, ratio = ratio),
        `nominal p` = pnorm(-z)
      ) |>
      left_join(
        y_h1 |> select(analysis, bound, probability)
      ) |>
      select(analysis, bound, probability, probability0, z, `~hr at bound`, `nominal p`) |>
      arrange(analysis, desc(bound))
  )

  # Get analysis summary to output ----
  suppressMessages(
    analysis <- x |>
      select(analysis, time, event, ahr) |>
      mutate(n = expected_accrual(time = x$time, enroll_rate = enroll_rate)) |>
      left_join(
        y_h1 |>
          select(analysis, info, info_frac, theta) |>
          unique()
      ) |>
      left_join(
        y_h0 |>
          select(analysis, info, info_frac) |>
          rename(info0 = info, info_frac0 = info_frac) |>
          unique()
      ) |>
      select(analysis, time, n, event, ahr, theta, info, info0, info_frac, info_frac0) |>
      arrange(analysis)
  )

  # Get input parameter to output ----
  input <- list(
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    event = event, analysis_time = analysis_time,
    binding = binding, ratio = ratio,
    upper = upper, upar = upar, test_upper = test_upper,
    lower = lower, lpar = lpar, test_lower = test_lower,
    weight = weight, info_scale = info_scale,
    approx = approx, r = r, tol = tol
  )

  # Return the output ----
  ans <- structure(
    list(
      design = "wlr",
      input = input,
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      bounds = bounds |> filter(!is.infinite(z)),
      analysis = analysis
    ),
    class = "gs_design",
    binding = binding,
    uninteger_is_from = "gs_power_wlr"
  )

  return(ans)
}

is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol
