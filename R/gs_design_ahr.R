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

#' Group sequential design using average hazard ratio under non-proportional hazards
#'
#' @param enroll_rate Enrollment rates.
#' @param fail_rate Failure and dropout rates.
#' @param ratio Experimental:Control randomization ratio (not yet implemented).
#' @param alpha One-sided Type I error.
#' @param beta Type II error.
#' @param info_frac Targeted information fraction at each analysis.
#' @param analysis_time Minimum time of analysis.
#' @param binding Indicator of whether futility bound is binding;
#'   default of `FALSE` is recommended.
#' @param upper Function to compute upper bound.
#' @param upar Parameters passed to `upper`.
#' @param lower Function to compute lower bound.
#' @param lpar Parameters passed to `lower`.
#' @param info_scale Information scale for calculation. Options are:
#'   - `"h0_h1_info"` (default): variance under both null and alternative hypotheses is used.
#'   - `"h0_info"`: variance under null hypothesis is used.
#'   - `"h1_info"`: variance under alternative hypothesis is used.
#' @param h1_spending Indicator that lower bound to be set by spending
#'   under alternate hypothesis (input `fail_rate`)
#'   if spending is used for lower bound.
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
#' @param tol Tolerance parameter for boundary convergence (on Z-scale).
#' @param interval An interval that is presumed to include the time at which
#'   expected event count is equal to targeted event.
#'
#' @return A list with input parameters, enrollment rate, analysis, and bound.
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
#' @details
#' To be added.
#'
#' @importFrom dplyr all_of mutate full_join select arrange desc
#' @importFrom gsDesign gsDesign sfLDOF
#' @importFrom stats qnorm
#'
#' @export
#'
#' @examples
#' library(gsDesign)
#' library(gsDesign2)
#' library(dplyr)
#'
#' # ----------------- #
#' #    example 1      #
#' # ----------------- #
#' # call with defaults
#' gs_design_ahr()
#'
#' # ----------------- #
#' #    example 2      #
#' # ----------------- #
#' # Single analysis
#' gs_design_ahr(analysis_time = 40)
#'
#' # ----------------- #
#' #    example 3      #
#' # ----------------- #
#' # Multiple analysis_time
#' gs_design_ahr(analysis_time = c(12, 24, 36))
#'
#' # ----------------- #
#' #    example 4      #
#' # ----------------- #
#' # Specified information fraction
#' \donttest{
#' gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = 36)
#' }
#'
#' # ----------------- #
#' #    example 5      #
#' # ----------------- #
#' # multiple analysis times & info_frac
#' # driven by times
#' gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
#' # driven by info_frac
#' \donttest{
#' gs_design_ahr(info_frac = c(1 / 3, .8, 1), analysis_time = c(12, 25, 36))
#' }
#'
#' # ----------------- #
#' #    example 6      #
#' # ----------------- #
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
#' # ----------------- #
#' #    example 7      #
#' # ----------------- #
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
    upper = gs_b,
    upar = gsDesign::gsDesign(
      k = 3, test.type = 1,
      n.I = c(.25, .75, 1),
      sfu = sfLDOF, sfupar = NULL
    )$upper$bound,
    lower = gs_b,
    lpar = c(qnorm(.1), -Inf, -Inf),
    h1_spending = TRUE,
    test_upper = TRUE,
    test_lower = TRUE,
    info_scale = c("h0_h1_info", "h0_info", "h1_info"),
    r = 18,
    tol = 1e-6,
    interval = c(.01, 100)) {
  # --------------------------------------------- #
  #     initialization                             #
  # --------------------------------------------- #
  if (is.null(info_frac)) {
    info_frac <- 1
  }
  info_scale <- match.arg(info_scale)

  # --------------------------------------------- #
  #     check inputs                              #
  # --------------------------------------------- #
  check_analysis_time(analysis_time)
  check_info_frac(info_frac)
  if ((length(analysis_time) > 1) && (length(info_frac) > 1) && (length(info_frac) != length(analysis_time))) {
    stop("gs_design_ahr() info_frac and analysis_time must have the same length if both have length > 1.")
  }

  # --------------------------------------------- #
  #     check if alpha is same as alpha spending  #
  # --------------------------------------------- #
  if (identical(upper, gs_spending_bound)) {
    if (!is.null(upar$total_spend)) {
      if (methods::missingArg(alpha)) {
        alpha <- upar$total_spend
      } else {
        if (alpha != upar$total_spend) {
          stop("gs_design_ahr(): the input alpha and the spending alpha is not consistent.")
        }
      }
    }
  }
  # --------------------------------------------- #
  #     get information at input analysis_time    #
  # --------------------------------------------- #
  y <- gs_info_ahr(enroll_rate, fail_rate,
    ratio = ratio, event = NULL,
    analysis_time = analysis_time,
    interval = interval
  )

  final_event <- y$event[nrow(y)]
  i_falt <- y$event / final_event

  # --------------------------------------------- #
  #     check if info_frac needed for IA timing   #
  # --------------------------------------------- #
  n_analysis <- max(length(analysis_time), length(info_frac))
  next_time <- max(analysis_time)
  # if info_frac is not provided by the users
  if (length(info_frac) == 1) {
    info_frac <- i_falt
  } else {
    # if there are >= 2 analysis
    if_indx <- info_frac[1:(n_analysis - 1)]
    for (i in seq_along(if_indx)) {
      # if ...
      if (length(i_falt) == 1) {
        y <- rbind(
          expected_time(
            enroll_rate = enroll_rate, fail_rate = fail_rate,
            ratio = ratio, target_event = info_frac[n_analysis - i] * final_event,
            interval = c(.01, next_time)
          ) %>%
            mutate(theta = -log(ahr), analysis = n_analysis - i),
          y
        )
      } else if (info_frac[n_analysis - i] > i_falt[n_analysis - i]) {
        # if the planned info_frac > info_frac under H1
        y[n_analysis - i, ] <- expected_time(
          enroll_rate = enroll_rate, fail_rate = fail_rate,
          ratio = ratio, target_event = info_frac[n_analysis - i] * final_event,
          interval = c(.01, next_time)
        ) %>%
          dplyr::transmute(
            analysis = n_analysis - i, time,
            event, ahr, theta = -log(ahr),
            info, info0
          )
      }
      next_time <- y$time[n_analysis - i]
    }
  }

  # update `y` (an object from `gs_power_ahr`) with
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

  # --------------------------------------------- #
  #     combine all the calculations              #
  # --------------------------------------------- #
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

  allout <- allout %>%
    # add `~hr at bound`, `hr generic` and `nominal p`
    mutate(
      "~hr at bound" = exp(-z / sqrt(info0)),
      "nominal p" = pnorm(-z)
    ) %>%
    # Add `time`, `event`, `ahr`, `n` from gs_info_ahr call above
    full_join(y %>% select(-c(info, info0, theta)),
      by = "analysis"
    ) %>%
    # select variables to be output
    select(c(
      "analysis", "bound", "time", "n", "event", "z",
      "probability", "probability0", "ahr", "theta",
      "info", "info0", "info_frac", "~hr at bound", "nominal p"
    )) %>%
    # arrange the output table
    arrange(analysis, desc(bound))

  inflac_fct <- (allout %>% filter(analysis == n_analysis, bound == "upper"))$info /
    (y %>% filter(analysis == n_analysis))$info
  allout$event <- allout$event * inflac_fct
  allout$n <- allout$n * inflac_fct

  # --------------------------------------------- #
  #     get bounds to output                      #
  # --------------------------------------------- #
  bound <- allout %>%
    select(all_of(c(
      "analysis", "bound", "probability", "probability0",
      "z", "~hr at bound", "nominal p"
    ))) %>%
    arrange(analysis, desc(bound))
  # --------------------------------------------- #
  #     get analysis summary to output            #
  # --------------------------------------------- #
  analysis <- allout %>%
    select(analysis, time, n, event, ahr, theta, info, info0, info_frac) %>%
    unique() %>%
    arrange(analysis)
  # --------------------------------------------- #
  #     get input parameter to output             #
  # --------------------------------------------- #
  input <- list(
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    alpha = alpha, beta = beta, ratio = ratio,
    info_frac = info_frac, analysis_time = analysis_time,
    upper = upper, upar = upar,
    lower = lower, lpar = lpar,
    test_upper = test_upper, test_lower = test_lower,
    h1_spending = h1_spending, binding = binding,
    info_scale = info_scale, r = r, tol = tol
  )

  # --------------------------------------------- #
  #     return the output                         #
  # --------------------------------------------- #
  ans <- list(
    input = input,
    enroll_rate = enroll_rate %>% mutate(rate = rate * inflac_fct),
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
