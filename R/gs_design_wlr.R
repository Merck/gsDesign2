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

#' Group sequential design using weighted log-rank test under non-proportional hazards
#'
#' @inheritParams gs_design_ahr
#' @inheritParams gs_info_wlr
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input analysis_time is a positive number or a positive increasing sequence.
#'    \item Validate if input info_frac is a positive number
#'    or positive increasing sequence on (0, 1] with final value of 1.
#'    \item Validate if inputs info_frac and analysis_time  have the same length if both have length > 1.
#'    \item Compute information at input analysis_time using \code{gs_info_wlr()}.
#'    \item Compute sample size and bounds using \code{gs_design_npe()}.
#'    \item Return a list of design enrollment, failure rates, and bounds.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @export
#' @return A list with input parameters, enrollment rate, analysis, and bound.
#'
#' @examples
#' library(mvtnorm)
#' library(gsDesign)
#' library(gsDesign2)
#'
#' # set enrollment rates
#' enroll_rate <- define_enroll_rate(duration = 12, rate = 1)
#'
#' # set failure rates
#' fail_rate <- define_fail_rate(
#'   duration = c(4, 100),
#'   fail_rate = log(2) / 15, # median survival 15 month
#'   hr = c(1, .6),
#'   dropout_rate = 0.001
#' )
#'
#' # Example 1 ----
#' # Information fraction driven design
#' gs_design_wlr(
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   ratio = 1,
#'   alpha = 0.025, beta = 0.2,
#'   weight = list(method = "mb", param = list(tau = Inf, w_max = 2)),
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2),
#'   analysis_time = 36,
#'   info_frac = c(0.6, 1)
#' )
#'
#' # Example 2 ----
#' # Calendar time driven design
#' gs_design_wlr(
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   ratio = 1,
#'   alpha = 0.025, beta = 0.2,
#'   weight = list(method = "mb", param = list(tau = Inf, w_max = 2)),
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2),
#'   analysis_time = c(24, 36),
#'   info_frac = NULL
#' )
#'
#' # Example 3 ----
#' # Both calendar time and information fraction driven design
#' gs_design_wlr(
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   ratio = 1,
#'   alpha = 0.025, beta = 0.2,
#'   weight = list(method = "mb", param = list(tau = Inf, w_max = 2)),
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2),
#'   analysis_time = c(24, 36),
#'   info_frac = c(0.6, 1)
#' )
gs_design_wlr <- function(
    enroll_rate = define_enroll_rate(
      duration = c(2, 2, 10),
      rate = c(3, 6, 9)
    ),
    fail_rate = tibble(
      stratum = "All", duration = c(3, 100),
      fail_rate = log(2) / c(9, 18), hr = c(.9, .6),
      dropout_rate = rep(.001, 2)
    ),
    weight = "logrank", approx = "asymptotic",
    alpha = 0.025, beta = 0.1, ratio = 1,
    info_frac = NULL,
    info_scale = c("h0_h1_info", "h0_info", "h1_info"),
    analysis_time = 36,
    binding = FALSE,
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = alpha),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = beta),
    test_upper = TRUE,
    test_lower = TRUE,
    h1_spending = TRUE,
    r = 18, tol = 1e-6,
    interval = c(.01, 1000)) {
  # --------------------------------------- #
  #      check input values                 #
  # --------------------------------------- #
  check_increasing(analysis_time)
  info_frac <- info_frac %||% 1
  check_info_frac(info_frac)
  n1 <- length(analysis_time); n2 <- length(info_frac)
  if (n1 > 1 && n2 > 1 && n1 != n2) stop(
    "`info_frac` and `analysis_time` must have the same length if both have length > 1"
  )
  if (all(fail_rate$hr == 1)) {
    stop("`hr` must not be 1 throughout the study as this is the null hypothesis.")
  }

  # ---------------------------------------- #
  #      get some basic parameters           #
  # ---------------------------------------- #
  info_scale <- match.arg(info_scale)
  n_analysis <- max(n1, n2)

  # ---------------------------------------- #
  #      get information at input            #
  #      analysis_time, covering 3 scenarios:#
  #      (1) fixed design                    #
  #      (2) info-frac driven design with    #
  #          a known study duration          #
  #      (3) calendar time driven design     #
  # ---------------------------------------- #
  y <- gs_info_wlr(enroll_rate, fail_rate,
                   ratio = ratio, event = NULL,
                   analysis_time = analysis_time, weight = weight, approx = approx,
                   interval = interval) |>
    dplyr::select(-c(n, delta, sigma2))

  # get the FA events given the FA analysis time
  final_event <- y$event[nrow(y)]

  # calculate the FA info according to different info_scale
  # calculate the info_frac of planned analysis time provided by `analysis_time`
  if(info_scale %in% c("h0_info", "h0_h1_info")) {
    final_info <- max(y$info0)
    info_frac_by_time <- y$info0 / final_info
  } else {
    final_info <- max(y$info)
    info_frac_by_time <- y$info / final_info
  }

  # if it is info frac driven group sequential design
  # relabel the analysis to FA, and back calculate IAs from FA
  if (n_analysis > 1 && n1 == 1 && n2 > 1) {
    y$analysis <- n_analysis
  }

  # ---------------------------------------- #
  #      calculate the design to meet        #
  #      targeted info_frac or analysis_time #
  #      or both                             #
  # ---------------------------------------- #
  next_time <- max(analysis_time)

  y_ia <- NULL
  # utility function to find the analysis time to get the planned/input info_frac
  find_time_by_info_frac <- function(x, input_info_frac) {
    y_ia <<- cache_fun(
      gs_info_wlr, enroll_rate, fail_rate, ratio, event = NULL,
      analysis_time = x, weight, approx, interval = c(.01, next_time)
    )
    i <- if (info_scale %in% c("h0_info", "h0_h1_info")) "info0" else "info"
    frac <- y_ia[[i]] / final_info
    frac - input_info_frac
  }

  # utility function to find the event to get the planned/input info_frac
  find_event_by_info_frac <- function(x, input_info_frac) {
    y_ia <<- cache_fun(
      gs_info_wlr, enroll_rate, fail_rate, ratio, event = x,
      analysis_time = NULL, weight, approx, interval = c(.01, next_time)
    )
    i <- if (info_scale %in% c("h0_info", "h0_h1_info")) "info0" else "info"
    frac <- y_ia[[i]] / final_info
    frac - input_info_frac
  }

  # if it is calendar time driven design,
  # e.g., info_frac = NULL, analysis_time = c(12, 14, 36)
  if (n2 == 1) {
    info_frac <- info_frac_by_time
  } else {
    # if info_frac != NULL
    if_indx <- info_frac[1:(n_analysis - 1)]
    for (i in seq_along(if_indx)) {
      # if it is info frac driven design with a known study duration,
      # e.g., info_frac = 1:3/3, analysis_time = 36
      if (length(info_frac_by_time) == 1) {
        # search the analysis time when the input info_frac arrives
        ia_time <- uniroot(find_time_by_info_frac,
                           interval = c(0.01, next_time),
                           input_info_frac = info_frac[n_analysis - i])$root

        y_ia <- y_ia |>
          dplyr::select(-c(n, delta, sigma2)) |>
          dplyr::mutate(theta = -log(ahr), analysis = n_analysis - i)
        y <- dplyr::bind_rows(y_ia, y)
      # if it is driven by both info frac and analysis time,
      # e.g., info_frac = 1:3/2, analysis_time = c(12, 24, 36)
      } else if (info_frac[n_analysis - i] > info_frac_by_time[n_analysis - i]) {
        # search the events when the input info_frac arrives
        ia_event <- uniroot(find_event_by_info_frac,
                           interval = c(0.01, y$event[y$analysis == n_analysis - i + 1]),
                           input_info_frac = info_frac[n_analysis - i])$root

        y_ia <- y_ia |>
          dplyr::select(-c(n, delta, sigma2)) |>
          dplyr::mutate(theta = -log(ahr), analysis = n_analysis - i)

        y_exclude_ia <- y |>  filter(analysis != n_analysis - i)
        y <- dplyr::bind_rows(y_ia, y_exclude_ia)
      }

      next_time <- y$time[y$analysis == n_analysis - i]
    }
  }

  y <- y |> arrange(analysis)
  y$analysis <- 1:n_analysis
  y$n <- expected_accrual(time = y$time, enroll_rate = enroll_rate)

  # ---------------------------------------- #
  #      adjust theta1 and info1             #
  #      by h1 spending or not               #
  # ---------------------------------------- #
  if (h1_spending) {
    theta1 <- y$theta
    info1 <- y$info
  } else {
    theta1 <- 0
    info1 <- y$info0
  }

  # ---------------------------------------- #
  #      calculate the design with known     #
  #      3 theta and 3 info                  #
  # ---------------------------------------- #
  suppressMessages(
    allout <- gs_design_npe(
      theta = y$theta, theta1 = theta1,
      info = y$info, info0 = y$info0, info1 = info1, info_scale = info_scale,
      alpha = alpha, beta = beta, binding = binding,
      upper = upper, upar = upar, test_upper = test_upper,
      lower = lower, lpar = lpar, test_lower = test_lower,
      r = r, tol = tol
    ) |>
      full_join(
        y |> select(-c(info, info0, theta)),
        by = "analysis"
      ) |>
      select(c(
        "analysis", "bound", "time", "n", "event", "z",
        "probability", "probability0", "ahr",
        "theta", "info", "info0", "info_frac"
      )) |>
      arrange(analysis, desc(bound))
  )

  # calculate sample size and event
  inflac_fct <- (allout |> filter(analysis == n_analysis, bound == "upper"))$info /
    (y |> filter(analysis == n_analysis))$info
  allout$event <- allout$event * inflac_fct
  allout$n <- allout$n * inflac_fct

  # add `~hr at bound` and `nominal p`
  allout <- allout |> mutate(
    "~hr at bound" = gsDesign::zn2hr(z = z, n = event, ratio = ratio),
    "nominal p" = pnorm(-z))

  # ---------------------------------------- #
  #      return the output                   #
  # ---------------------------------------- #
  # bounds table
  bounds <- allout |>
    select(all_of(c("analysis", "bound", "probability", "probability0", "z", "~hr at bound", "nominal p"))) |>
    arrange(analysis, desc(bound))

  # analysis table
  analysis <- allout |>
    select(analysis, time, n, event, ahr, theta, info, info0, info_frac) |>
    mutate(info_frac0 = info0 / max(info0)) |>
    unique() |>
    arrange(analysis)

  # input table
  input <- list(
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    weight = weight, approx = approx,
    alpha = alpha, beta = beta, ratio = ratio,
    info_frac = info_frac, analysis_time = analysis_time,
    upper = upper, upar = upar,
    lower = lower, lpar = lpar,
    test_upper = test_upper, test_lower = test_lower,
    h1_spending = h1_spending, binding = binding,
    info_scale = info_scale, r = r, tol = tol)

  # final output
  ans <- structure(
    list(
      design = "wlr",
      input = input,
      enroll_rate = enroll_rate |> mutate(rate = rate * inflac_fct),
      fail_rate = fail_rate,
      bounds = bounds |> filter(!is.infinite(z)),
      analysis = analysis
    ),
    class = "gs_design",
    binding = binding,
    uninteger_is_from = "gs_design_wlr"
  )

  return(ans)
}
