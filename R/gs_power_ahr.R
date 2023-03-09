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

#' Group sequential design power using average hazard ratio under
#' non-proportional hazards
#'
#' Group sequential design power using average hazard ratio under
#' non-proportional hazards.
#'
#' @param enroll_rate Enrollment rates.
#' @param fail_rate Failure and dropout rates.
#' @param ratio Experimental:Control randomization ratio (not yet implemented).
#' @param event Targeted event at each analysis.
#' @param analysis_time Minimum time of analysis.
#' @param binding Indicator of whether futility bound is binding;
#'   default of `FALSE` is recommended.
#' @param info_scale The information scale for calculation.
#' @param upper Function to compute upper bound.
#' @param upar Parameters passed to `upper`.
#' @param lower Function to compute lower bound.
#' @param lpar Parameters passed to `lower`.
#' @param test_upper Indicator of which analyses should include an upper
#'   (efficacy) bound; single value of `TRUE` (default) indicates all analyses;
#'   otherwise, a logical vector of the same length as `info` should
#'   indicate which analyses will have an efficacy bound.
#' @param test_lower Indicator of which analyses should include an lower bound;
#'   single value of `TRUE` (default) indicates all analyses;
#'   single value of `FALSE` indicated no lower bound;
#'   otherwise, a logical vector of the same length as `info` should indicate
#'   which analyses will have a lower bound.
#' @param r Integer value controlling grid for numerical integration as in
#'   Jennison and Turnbull (2000); default is 18, range is 1 to 80.
#'   Larger values provide larger number of grid points and greater accuracy.
#'   Normally, `r` will not be changed by the user.
#' @param tol Tolerance parameter for boundary convergence (on Z-scale).
#' @param interval An interval that is presumed to include the time at which
#'   expected event count is equal to targeted event.
#'
#' @return A tibble with columns `Analysis`, `Bound`, `Z`, `Probability`,
#'   `theta`, `Time`, `AHR`, `Events`.
#'   Contains a row for each analysis and each bound.
#'
#' @details
#' Bound satisfy input upper bound specification in
#' `upper`, `upar`, and lower bound specification in `lower`, `lpar`.
#' [ahr()] computes statistical information at targeted event times.
#' The [expected_time()] function is used to get events and average HR at
#' targeted `analysis_time`.
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
#' @importFrom tibble tibble
#' @importFrom gsDesign gsDesign sfLDOF
#' @importFrom stats qnorm
#' @importFrom dplyr select arrange desc
#'
#' @export
#'
#' @examples
#' library(gsDesign2)
#' library(dplyr)
#'
#' # Example 1 -----------------------------------------------------------------
#' # The default output of `gs_power_ahr()` is driven by events,
#' # i.e., `event = c(30, 40, 50)`, `analysis_time = NULL`
#' \donttest{
#' gs_power_ahr()
#' }
#' # Example 2 -----------------------------------------------------------------
#' # 2-sided symmetric O'Brien-Fleming spending bound, driven by analysis time,
#' # i.e., `event = NULL`, `analysis_time = c(12, 24, 36)`
#'
#' gs_power_ahr(
#'   analysis_time = c(12, 24, 36),
#'   event = NULL,
#'   binding = TRUE,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
#' )
#'
#' # Example 3 -----------------------------------------------------------------
#' # 2-sided symmetric O'Brien-Fleming spending bound, driven by event,
#' # i.e., `event = c(20, 50, 70)`, `analysis_time = NULL`
#' \donttest{
#' gs_power_ahr(
#'   analysis_time = NULL,
#'   event = c(20, 50, 70),
#'   binding = TRUE,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
#' )
#' }
#' # Example 4 -----------------------------------------------------------------
#' # 2-sided symmetric O'Brien-Fleming spending bound,
#' # driven by both `event` and `analysis_time`, i.e.,
#' # both `event` and `analysis_time` are not `NULL`,
#' # then the analysis will driven by the maximal one, i.e.,
#' # Time = max(analysis_time, calculated Time for targeted event)
#' # Events = max(events, calculated events for targeted analysis_time)
#' \donttest{
#' gs_power_ahr(
#'   analysis_time = c(12, 24, 36),
#'   event = c(30, 40, 50),
#'   binding = TRUE,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
#' )
#' }
gs_power_ahr <- function(enroll_rate = tibble(
                           stratum = "All",
                           duration = c(2, 2, 10),
                           rate = c(3, 6, 9)
                         ),
                         fail_rate = tibble(
                           stratum = "All",
                           duration = c(3, 100),
                           fail_rate = log(2) / c(9, 18),
                           hr = c(.9, .6),
                           dropout_rate = rep(.001, 2)
                         ),
                         event = c(30, 40, 50),
                         analysis_time = NULL,
                         upper = gs_b,
                         upar = gsDesign(
                           k = length(event),
                           test.type = 1,
                           n.I = event,
                           maxn.IPlan = max(event),
                           sfu = sfLDOF,
                           sfupar = NULL
                         )$upper$bound,
                         lower = gs_b,
                         lpar = c(qnorm(.1), rep(-Inf, 2)),
                         test_lower = TRUE,
                         test_upper = TRUE,
                         ratio = 1,
                         binding = FALSE,
                         info_scale = c(0, 1, 2),
                         r = 18,
                         tol = 1e-6,
                         interval = c(.01, 100)) {
  # Get the number of analysis
  n_analysis <- max(length(event), length(analysis_time), na.rm = TRUE)

  # Get the info_scale
  info_scale <- if (methods::missingArg(info_scale)) {
    2
  } else {
    match.arg(as.character(info_scale), choices = 0:2)
  }

  # Check if it is two-sided design or not
  if (identical(lower, gs_b) && (!is.list(lpar))) {
    if (all(test_lower) == FALSE) {
      two_sided <- FALSE
      lpar <- rep(-Inf, n_analysis)
    } else {
      two_sided <- ifelse(identical(lpar, rep(-Inf, n_analysis)), FALSE, TRUE)
    }
  } else {
    two_sided <- TRUE
  }

  # Calculate the asymptotic variance and statistical information --------------
  x <- gs_info_ahr(
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    ratio = ratio, event = event, analysis_time = analysis_time,
    interval = interval
  )

  # Given the above statistical information, calculate the power ---------------
  y_h1 <- gs_power_npe(
    theta = x$theta,
    info = x$info, info0 = x$info0, info1 = x$info, info_scale = info_scale,
    upper = upper, upar = upar, test_upper = test_upper,
    lower = lower, lpar = lpar, test_lower = test_lower,
    binding = binding, r = r, tol = tol
  )

  y_h0 <- gs_power_npe(
    theta = 0, theta0 = 0, theta1 = x$theta,
    info = x$info0, info0 = x$info0, info1 = x$info, info_scale = info_scale,
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

  # Organize the outputs -------------------------------------------------------
  # summarize the bounds
  suppressMessages(
    bound <- y_h1 %>%
      mutate(`~hr at bound` = exp(-z / sqrt(info)), `nominal p` = pnorm(-z)) %>%
      left_join(
        y_h0 %>%
          select(analysis, bound, probability) %>%
          dplyr::rename(probability0 = probability)
      ) %>%
      select(analysis, bound, probability, probability0, z, `~hr at bound`, `nominal p`) %>%
      arrange(analysis, desc(bound))
  )
  # summarize the analysis
  suppressMessages(
    analysis <- x %>%
      select(analysis, time, event, ahr) %>%
      mutate(n = expected_accrual(time = x$time, enroll_rate = enroll_rate)) %>%
      left_join(y_h1 %>%
        select(analysis, info, info_frac, theta) %>%
        unique()) %>%
      left_join(y_h0 %>%
        select(analysis, info, info_frac) %>%
        dplyr::rename(info0 = info, info_frac0 = info_frac) %>%
        unique()) %>%
      select(analysis, time, n, event, ahr, theta, info, info0, info_frac, info_frac0) %>%
      arrange(analysis)
  )

  # Get input parameter to output ----------------------------------------------
  input <- list(
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    event = event, analysis_time = analysis_time,
    upper = upper, upar = upar,
    lower = lower, lpar = lpar,
    test_lower = test_lower, test_upper = test_upper,
    ratio = ratio, binding = binding, info_scale = info_scale, r = r, tol = tol
  )

  ans <- list(
    input = input,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    bound = bound %>% filter(!is.infinite(z)),
    analysis = analysis
  )

  class(ans) <- c("ahr", "gs_design", class(ans))
  if (!binding) {
    class(ans) <- c("non_binding", class(ans))
  }

  return(ans)
}
