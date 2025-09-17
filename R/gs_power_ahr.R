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

#' Group sequential design power using average hazard ratio under
#' non-proportional hazards
#'
#' Calculate power given the sample size in group sequential design power using average hazard ratio under
#' non-proportional hazards.
#'
#' @inheritParams gs_design_ahr
#' @param event A numerical vector specifying the targeted events at each analysis. See details.
#' @param integer Indicator of whether integer sample size and events are intended. This argument is
#' used when using [to_integer()].
#'
#' @return A list with input parameters, enrollment rate, analysis, and bound.
#'   - `$input` a list including `alpha`, `beta`, `ratio`, etc.
#'   - `$enroll_rate` a table showing the enrollment, which is the same as input.
#'   - `$fail_rate` a table showing the failure and dropout rates, which is the same as input.
#'   - `$bound` a table summarizing the efficacy and futility bound at each analysis.
#'   - `analysis` a table summarizing the analysis time, sample size, events, average HR, treatment effect and statistical information at each analysis.
#'
#' @details
#' Note that time units are arbitrary, but should be the same for all rate parameters in `enroll_rate`, `fail_rate`, and `analysis_time`.
#'
#' Computed bounds satisfy input upper bound specification in
#' `upper`, `upar`, and lower bound specification in `lower`, `lpar`.
#' [ahr()] computes statistical information at targeted event times.
#' The [expected_time()] function is used to get events and average HR at
#' targeted `analysis_time`.
#'
#' The parameters `event` and `analysis_time` are used to determine the timing for interim and final analyses.
#'  - If analysis timing is to be determined by targeted events,
#'    then `event` is a numerical vector specifying the targeted events for each analysis;
#'    note that this can be NULL.
#'  - If interim analysis is determined by targeted calendar timing relative to start of enrollment,
#'    then `analysis_time` will be a vector specifying the calendar time from start of study for each analysis;
#'    note that this can be NULL.
#'  - A corresponding element of `event` or `analysis_time` should be provided for each analysis.
#'  - If both `event[i]` and `analysis[i]` are provided for analysis `i`, then the time corresponding to the
#'  later of these is used  for analysis `i`.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Calculate information and effect size based on AHR approximation
#'    using \code{gs_info_ahr()}.
#'    \item Return a tibble of with columns Analysis, Bound, Z, Probability,
#'    theta, Time, AHR, Events and  contains a row for each analysis
#'    and each bound.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @export
#'
#' @examples
#' library(gsDesign2)
#'
#' # Example 1 ----
#' # The default output of `gs_power_ahr()` is driven by events,
#' # i.e., `event = c(30, 40, 50)`, `analysis_time = NULL`
#' \donttest{
#' gs_power_ahr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1))
#' }
#' # Example 2 ----
#' # 2-sided symmetric O'Brien-Fleming spending bound, driven by analysis time,
#' # i.e., `event = NULL`, `analysis_time = c(12, 24, 36)`
#'
#' gs_power_ahr(
#'   analysis_time = c(12, 24, 36),
#'   event = NULL,
#'   binding = TRUE,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025)
#' )
#'
#' # Example 3 ----
#' # 2-sided symmetric O'Brien-Fleming spending bound, driven by event,
#' # i.e., `event = c(20, 50, 70)`, `analysis_time = NULL`
#' # Note that this assumes targeted final events for the design is 70 events.
#' # If actual targeted final events were 65, then `timing = c(20, 50, 70) / 65`
#' # would be added to `upar` and `lpar` lists.
#' # NOTE: at present the computed information fraction in output `analysis` is based
#' # on 70 events rather than 65 events when the `timing` argument is used in this way.
#` # This behavior is likely to be updated in the near future.
#' # A vignette on this topic will be forthcoming.
#' \donttest{
#' gs_power_ahr(
#'   analysis_time = NULL,
#'   event = c(20, 50, 70),
#'   binding = TRUE,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025)
#' )
#' }
#' # Example 4 ----
#' # 2-sided symmetric O'Brien-Fleming spending bound,
#' # driven by both `event` and `analysis_time`, i.e.,
#' # both `event` and `analysis_time` are not `NULL`,
#' # then the analysis will driven by the maximal one, i.e.,
#' # Time = max(analysis_time, calculated Time for targeted event)
#' # Events = max(events, calculated events for targeted analysis_time)
#' \donttest{
#' gs_power_ahr(
#'   analysis_time = c(12, 24, 36),
#'   event = c(30, 40, 50), h1_spending = FALSE,
#'   binding = TRUE,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025)
#' )
#' }
gs_power_ahr <- function(
    enroll_rate = define_enroll_rate(
      duration = c(2, 2, 10),
      rate = c(3, 6, 9)
    ),
    fail_rate = define_fail_rate(
      duration = c(3, 100),
      fail_rate = log(2) / c(9, 18),
      hr = c(.9, .6),
      dropout_rate = rep(.001, 2)
    ),
    event = c(30, 40, 50),
    analysis_time = NULL,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = NULL),
    test_lower = TRUE,
    test_upper = TRUE,
    ratio = 1,
    binding = FALSE,
    h1_spending = TRUE,
    info_scale = c("h0_h1_info", "h0_info", "h1_info"),
    r = 18,
    tol = 1e-6,
    interval = c(.01, 1000),
    integer = FALSE) {
  # Get the number of analysis
  n_analysis <- max(length(event), length(analysis_time), na.rm = TRUE)

  # Get the info_scale
  info_scale <- match.arg(info_scale)

  upper <- match.fun(upper)
  lower <- match.fun(lower)

  # Check if it is two-sided design or not
  if ((identical(lower, gs_b) && (!is.list(lpar))) || all(!test_lower)) {
    if (all(test_lower == FALSE)) {
      two_sided <- FALSE
    } else {
      two_sided <- ifelse(identical(lpar, rep(-Inf, n_analysis)), FALSE, TRUE)
    }
  } else {
    two_sided <- TRUE
  }

  if (!two_sided) {
    lpar <- rep(-Inf, n_analysis)
    lower <- gs_b
  }

  # Check if user input the total spending for futility,
  # if they use spending function for futility
  if (two_sided && identical(lower, gs_spending_bound)) {
    if (is.null(lpar$total_spend) && any(test_lower)) {
      stop("gs_power_ahr: please input the total_spend to the futility spending function.")
    }
  }

  # Calculate the asymptotic variance and statistical information ----
  x <- gs_info_ahr(
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    ratio = ratio, event = event, analysis_time = analysis_time,
    interval = interval
  )

  # if both events and sample size are integers, then elaborate the info and info0
  if (integer) {

    # elaborate info0
    q_e <- ratio / (1 + ratio)
    q_c <- 1 - q_e
    x$info0 <- event * q_e * q_c

    # elaborate info
    q <- event / x$event
    x$info <- x$info * q
  }

  if (h1_spending) {
      theta1 <- x$theta
      info1 <- x$info
  } else {
      theta1 <- 0
      info1 <- x$info0
  }

  # Given the above statistical information, calculate the power ----
  y_h1 <- gs_power_npe(
    theta = x$theta, theta0 = 0, theta1 = theta1,
    info = x$info, info0 = x$info0, info1 = info1, info_scale = info_scale,
    upper = upper, upar = upar, test_upper = test_upper,
    lower = lower, lpar = lpar, test_lower = test_lower,
    binding = binding, r = r, tol = tol
  )

  y_h0 <- gs_power_npe(
    theta = 0, theta0 = 0, theta1 = theta1,
    info = x$info0, info0 = x$info0, info1 = info1, info_scale = info_scale,
    upper = upper, upar = upar, test_upper = test_upper,
    lower = if (!two_sided) {
      gs_b
    } else {
      lower
    },
    lpar = if (!two_sided) {
      rep(-Inf, n_analysis)
    } else {
      lpar
    },
    test_lower = test_lower,
    binding = binding, r = r, tol = tol
  )

  # Organize the outputs ----
  # Summarize the bounds
  suppressMessages(
    bound <- y_h1 |>
      mutate(`~hr at bound` = exp(-z / sqrt(info0)), `nominal p` = pnorm(-z)) |>
      left_join(
        y_h0 |>
          select(analysis, bound, probability) |>
          rename(probability0 = probability)
      ) |>
      select(analysis, bound, probability, probability0, z, `~hr at bound`, `nominal p`) |>
      arrange(analysis, desc(bound))
  )
  # Summarize the analysis
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
    info_scale = info_scale,
    alpha = if (identical(upper, gs_spending_bound)) {upar$total_spend} else {NULL},
    upper = upper, upar = upar,
    lower = lower, lpar = lpar,
    test_lower = test_lower, test_upper = test_upper,
    ratio = ratio, binding = binding, h1_spending = h1_spending,
    info_scale = info_scale, r = r, tol = tol
  )

  ans <- structure(
    list(
      design = "ahr",
      input = input,
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      bound = bound |> filter(!is.infinite(z)),
      analysis = analysis
    ),
    class = "gs_design",
    binding = binding,
    uninteger_is_from = "gs_power_ahr"
  )

  return(ans)
}
