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

#' Calculate sample size and bounds given targeted power and Type I error in group sequential design using average hazard ratio under non-proportional hazards
#'
#' @param enroll_rate Enrollment rates defined by \code{define_enroll_rate()}.
#' @param fail_rate Failure and dropout rates defined by \code{define_fail_rate()}.
#' @param ratio Experimental:Control randomization ratio.
#' @param alpha One-sided Type I error.
#' @param beta Type II error.
#' @param info_frac Targeted information fraction for analyses. See details.
#' @param analysis_time Targeted calendar timing of analyses. See details.
#' @param binding Indicator of whether futility bound is binding;
#'   default of `FALSE` is recommended.
#' @param upper Function to compute upper bound.
#'   - \code{gs_spending_bound()}: alpha-spending efficacy bounds.
#'   - \code{gs_b()}: fixed efficacy bounds.
#' @param upar Parameters passed to `upper`.
#'   - If `upper = gs_b`, then `upar` is a numerical vector specifying the fixed efficacy bounds per analysis.
#'   - If `upper = gs_spending_bound`, then `upar` is a list including
#'       - `sf` for the spending function family.
#'       - `total_spend` for total alpha spend.
#'       - `param` for the parameter of the spending function.
#'       - `timing` specifies spending time if different from information-based spending; see details.
#' @param lower Function to compute lower bound, which can be set up similarly as `upper`.
#' See [this vignette](https://merck.github.io/gsDesign2/articles/story-seven-test-types.html).
#' @param lpar Parameters passed to `lower`, which can be set up similarly as `upar.`
#' @param info_scale Information scale for calculation. Options are:
#'   - `"h0_h1_info"` (default): variance under both null and alternative hypotheses is used.
#'   - `"h0_info"`: variance under null hypothesis is used.
#'   - `"h1_info"`: variance under alternative hypothesis is used.
#' @param h1_spending Indicator that lower bound to be set by spending
#'   under alternate hypothesis (input `fail_rate`)
#'   if spending is used for lower bound.
#'   If this is `FALSE`, then the lower bound spending is under the null hypothesis.
#'   This is for two-sided symmetric or asymmetric testing under the null hypothesis;
#'   See [this vignette](https://merck.github.io/gsDesign2/articles/story-seven-test-types.html).
#' @param test_upper Indicator of which analyses should include an upper
#'   (efficacy) bound; single value of `TRUE` (default) indicates all analyses;
#'   otherwise, a logical vector of the same length as `info` should indicate
#'   which analyses will have an efficacy bound.
#' @param test_lower Indicator of which analyses should include an lower bound;
#'   single value of `TRUE` (default) indicates all analyses;
#'   single value `FALSE` indicated no lower bound; otherwise, a logical vector
#'   of the same length as `info` should indicate which analyses will have a
#'   lower bound.
#' @param r Integer value controlling grid for numerical integration as in
#'   Jennison and Turnbull (2000); default is 18, range is 1 to 80.
#'   Larger values provide larger number of grid points and greater accuracy.
#'   Normally, `r` will not be changed by the user.
#' @param tol Tolerance parameter for boundary convergence (on Z-scale); normally not changed by the user.
#' @param interval An interval presumed to include the times at which
#'   expected event count is equal to targeted event.
#'   Normally, this can be ignored by the user as it is set to `c(.01, 1000)`.
#'
#' @return A list with input parameters, enrollment rate, analysis, and bound.
#'   - The `$input` is a list including `alpha`, `beta`, `ratio`, etc.
#'   - The `$enroll_rate` is a table showing the enrollment for arriving the targeted power (`1 - beta`).
#'   - The `$fail_rate` is a table showing the failure and dropout rates, which is the same as input.
#'   - The `$bound` is a table summarizing the efficacy and futility bound per analysis.
#'   - The `analysis` is a table summarizing the analysis time, sample size, events, average HR, treatment effect and statistical information per analysis.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input analysis_time is a positive number or positive
#'    increasing sequence.
#'    \item Validate if input info_frac is a positive number or positive
#'    increasing sequence
#'    on (0, 1] with final value of 1.
#'    \item Validate if input info_frac and analysis_time  have the same
#'    length if both have length > 1.
#'    \item Get information at input analysis_time
#'    \itemize{
#'      \item Use \code{gs_info_ahr()} to get the information and effect size
#'      based on AHR approximation.
#'      \item Extract the final event.
#'      \item Check if input If needed for (any) interim analysis timing.
#'    }
#'    \item Add the analysis column to the information at input analysis_time.
#'    \item Add the sample size column to the information at input analysis_time
#'    using \code{expected_accural()}.
#'    \item Get sample size and bounds using \code{gs_design_npe()} and
#'    save them to bounds.
#'    \item Add Time, Events, AHR, N that have already been calculated
#'    to the bounds.
#'    \item Return a list of design enrollment, failure rates, and bounds.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @export
#'
#' @details
#' The parameters `info_frac` and `analysis_time` are used to determine the timing for interim and final analyses.
#'  - If the interim analysis is determined by targeted information fraction and the study duration is known,
#'    then `info_frac` is a numerical vector where each element (greater than 0 and less than or equal to 1)
#'    represents the information fraction for each analysis.
#'    The `analysis_time`, which defaults to 36, indicates the time for the final analysis.
#'  - If interim analyses are determined solely by the targeted calendar analysis timing from start of study,
#'    then `analysis_time` will be a vector specifying the time for each analysis.
#'  - If both the targeted analysis time and the targeted information fraction are utilized for a given analysis,
#'    then timing will be the maximum of the two with both `info_frac` and `analysis_time` provided as vectors.
#'
#' @examples
#' library(gsDesign)
#' library(gsDesign2)
#'
#' # Example 1 ----
#' # call with defaults
#' gs_design_ahr()
#'
#' # Example 2 ----
#' # Single analysis
#' gs_design_ahr(analysis_time = 40)
#'
#' # Example 3 ----
#' # Multiple analysis_time
#' gs_design_ahr(analysis_time = c(12, 24, 36))
#'
#' # Example 4 ----
#' # Specified information fraction
#' \donttest{
#' gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = 36)
#' }
#'
#' # Example 5 ----
#' # multiple analysis times & info_frac
#' # driven by times
#' gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
#' # driven by info_frac
#' \donttest{
#' gs_design_ahr(info_frac = c(1 / 3, .8, 1), analysis_time = c(12, 25, 36))
#' }
#'
#' # Example 6 ----
#' # 2-sided symmetric design with O'Brien-Fleming spending
#' \donttest{
#' gs_design_ahr(
#'   analysis_time = c(12, 24, 36),
#'   binding = TRUE,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   h1_spending = FALSE
#' )
#' }
#' # 2-sided asymmetric design with O'Brien-Fleming upper spending
#' # Pocock lower spending under H1 (NPH)
#' \donttest{
#' gs_design_ahr(
#'   analysis_time = c(12, 24, 36),
#'   binding = TRUE,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDPocock, total_spend = 0.1, param = NULL, timing = NULL),
#'   h1_spending = TRUE
#' )
#' }
#'
#' # Example 7 ----
#' \donttest{
#' gs_design_ahr(
#'   alpha = 0.0125,
#'   analysis_time = c(12, 24, 36),
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.0125, param = NULL, timing = NULL),
#'   lower = gs_b,
#'   lpar = rep(-Inf, 3)
#' )
#'
#' gs_design_ahr(
#'   alpha = 0.0125,
#'   analysis_time = c(12, 24, 36),
#'   upper = gs_b,
#'   upar = gsDesign::gsDesign(
#'     k = 3, test.type = 1, n.I = c(.25, .75, 1),
#'     sfu = sfLDOF, sfupar = NULL, alpha = 0.0125
#'   )$upper$bound,
#'   lower = gs_b,
#'   lpar = rep(-Inf, 3)
#' )
#' }
gs_design_ahr <- function(
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
    alpha = 0.025, beta = 0.1,
    info_frac = NULL, analysis_time = 36,
    ratio = 1, binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = alpha),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = beta),
    h1_spending = TRUE,
    test_upper = TRUE,
    test_lower = TRUE,
    info_scale = c("h0_h1_info", "h0_info", "h1_info"),
    r = 18,
    tol = 1e-6,
    interval = c(.01, 1000)) {
  # Initialization ----
  if (is.null(info_frac)) {
    info_frac <- 1
  }
  info_scale <- match.arg(info_scale)
  upper <- match.fun(upper)
  lower <- match.fun(lower)

  # Check inputs ----
  check_analysis_time(analysis_time)
  check_info_frac(info_frac)
  if ((length(analysis_time) > 1) && (length(info_frac) > 1) && (length(info_frac) != length(analysis_time))) {
    stop("gs_design_ahr() info_frac and analysis_time must have the same length if both have length > 1.")
  }
  if (all(fail_rate$hr == 1)) {
    stop("gs_design_ahr() hr must not be equal to 1 throughout the study as this is the null hypothesis.")
  }

  # Check if alpha is same as alpha spending ----
  if (identical(upper, gs_spending_bound)) {
    if (!is.null(upar$total_spend)) {
      if (missing(alpha)) {
        alpha <- upar$total_spend
      } else {
        if (alpha != upar$total_spend) {
          stop("gs_design_ahr(): the input alpha and the spending alpha is not consistent.")
        }
      }
    }
  }

  # Get information at input analysis_time ----
  y <- gs_info_ahr(enroll_rate, fail_rate,
    ratio = ratio, event = NULL,
    analysis_time = analysis_time,
    interval = interval
  )

  # Event fraction driven by the calendar time
  final_event <- y$event[nrow(y)]
  if_alt <- y$event / final_event

  # Number of analyses (including final analysis)
  n_analysis <- max(length(analysis_time), length(info_frac))

  # Initialize the next_time as the study duration
  next_time <- max(analysis_time)

  # If info_frac is not provided by the users
  if (length(info_frac) == 1) {
    info_frac <- if_alt
  } else {
    # If there are >= 2 analysis
    if_indx <- info_frac[1:(n_analysis - 1)]
    for (i in seq_along(if_indx)) {
      # If it is fixed analysis
      # or it is information fraction driven design
      if (length(if_alt) == 1) {

        y$analysis <- n_analysis

        y <- rbind(
          expected_time(
            enroll_rate = enroll_rate, fail_rate = fail_rate,
            ratio = ratio, target_event = info_frac[n_analysis - i] * final_event,
            interval = c(.01, next_time)
          ) |>
            mutate(theta = -log(ahr), analysis = n_analysis - i),
          y
        )

        next_time <- y$time[1]
        # If the planned info_frac input by the user > event fraction
        # Equivalently, the planned info_frac happens later than planned calendar time
        # We will wait until the planned info_frac arrives
      } else if (info_frac[n_analysis - i] > if_alt[n_analysis - i]) {
        y[n_analysis - i, ] <- expected_time(
          enroll_rate = enroll_rate, fail_rate = fail_rate,
          ratio = ratio, target_event = info_frac[n_analysis - i] * final_event,
          interval = c(.01, next_time)
        ) |>
          dplyr::transmute(
            analysis = n_analysis - i, time,
            event, ahr, theta = -log(ahr),
            info, info0
          )

        next_time <- y$time[n_analysis - i]
      }

    }
  }

  # Update `y` (an object from `gs_power_ahr`) with
  # 1) analysis NO.
  # 2) the accrual sample size, i.e., `N`
  # 3) `theta1` and `info1`
  y$analysis <- 1:n_analysis
  y$n <- expected_accrual(time = y$time, enroll_rate = enroll_rate)
  if (h1_spending) {
    theta1 <- y$theta
    info1 <- y$info
  } else {
    theta1 <- 0
    info1 <- y$info0
  }

  # Combine all the calculations ----
  suppressMessages(
    allout <- gs_design_npe(
      theta = y$theta, theta0 = 0, theta1 = theta1,
      info = y$info, info0 = y$info0, info1 = info1,
      info_scale = info_scale,
      alpha = alpha, beta = beta, binding = binding,
      upper = upper, upar = upar, test_upper = test_upper,
      lower = lower, lpar = lpar, test_lower = test_lower,
      r = r, tol = tol
    )
  )

  allout <- allout |>
    # Add `~hr at bound`, `hr generic` and `nominal p`
    mutate(
      "~hr at bound" = exp(-z / sqrt(info0)),
      "nominal p" = pnorm(-z)
    ) |>
    # Add `time`, `event`, `ahr`, `n` from gs_info_ahr call above
    full_join(y |> select(-c(info, info0, theta)),
      by = "analysis"
    ) |>
    # Select variables to be output
    select(c(
      "analysis", "bound", "time", "n", "event", "z",
      "probability", "probability0", "ahr", "theta",
      "info", "info0", "info_frac", "~hr at bound", "nominal p"
    )) |>
    # Arrange the output table
    arrange(analysis, desc(bound))

  inflac_fct <- (allout |> filter(analysis == n_analysis, bound == "upper"))$info /
    (y |> filter(analysis == n_analysis))$info
  allout$event <- allout$event * inflac_fct
  allout$n <- allout$n * inflac_fct

  # Get bounds to output ----
  bound <- allout |>
    select(all_of(c(
      "analysis", "bound", "probability", "probability0",
      "z", "~hr at bound", "nominal p"
    ))) |>
    arrange(analysis, desc(bound))

  # Get analysis summary to output ----
  analysis <- allout |>
    select(analysis, time, n, event, ahr, theta, info, info0, info_frac) |>
    mutate(info_frac0 = event / last_(event)) |>
    unique() |>
    arrange(analysis)

  # Get input parameter to output ----
  input <- list(
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    alpha = alpha, beta = beta, ratio = ratio,
    info_frac = info_frac, analysis_time = analysis_time,
    info_scale = info_scale,
    upper = upper, upar = upar,
    lower = lower, lpar = lpar,
    test_upper = test_upper, test_lower = test_lower,
    h1_spending = h1_spending, binding = binding,
    info_scale = info_scale, r = r, tol = tol
  )

  # Return the output ----
  ans <- structure(
    list(
      design = "ahr",
      input = input,
      enroll_rate = enroll_rate |> mutate(rate = rate * inflac_fct),
      fail_rate = fail_rate,
      bound = bound |> filter(!is.infinite(z)),
      analysis = analysis
    ),
    class = "gs_design",
    binding = binding,
    uninteger_is_from = "gs_design_ahr"
  )

  return(ans)
}
