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

#' Group sequential design of binary outcome measuring in risk difference
#'
#' @param p_c Rate at the control group.
#' @param p_e Rate at the experimental group.
#' @param info_frac Statistical information fraction.
#' @param rd0 Treatment effect under super-superiority designs, the default is 0.
#' @param alpha One-sided Type I error.
#' @param beta Type II error.
#' @param ratio Experimental:Control randomization ratio (not yet implemented).
#' @param stratum_prev Randomization ratio of different stratum.
#'   If it is unstratified design then `NULL`.
#'   Otherwise it is a tibble containing two columns (stratum and prevalence).
#' @param binding Indicator of whether futility bound is binding;
#'   default of `FALSE` is recommended.
#' @param upper Function to compute upper bound.
#' @param upar Parameters passed to `upper`.
#' @param lower Function to compute lower bound.
#' @param lpar Parameters passed to `lower`.
#' @param test_upper Indicator of which analyses should include an upper
#'   (efficacy) bound; single value of `TRUE` (default) indicates all analyses;
#'   otherwise, a logical vector of the same length as `info` should indicate
#'   which analyses will have an efficacy bound.
#' @param test_lower Indicator of which analyses should include an lower bound;
#'   single value of `TRUE` (default) indicates all analyses;
#'   single value of `FALSE` indicates no lower bound; otherwise,
#'   a logical vector of the same length as `info` should indicate which
#'   analyses will have a lower bound.
#' @param h1_spending Indicator that lower bound to be set by
#'   spending under alternate hypothesis (input `fail_rate`)
#'   if spending is used for lower bound.
#' @param r Integer value controlling grid for numerical integration
#'   as in Jennison and Turnbull (2000); default is 18, range is 1 to 80.
#'   Larger values provide larger number of grid points and greater accuracy.
#'   Normally, `r` will not be changed by the user.
#' @param info_scale Information scale for calculation. Options are:
#'   - `"h0_h1_info"` (default): variance under both null and alternative hypotheses is used.
#'   - `"h0_info"`: variance under null hypothesis is used.
#'   - `"h1_info"`: variance under alternative hypothesis is used.
#' @param weight The weighting scheme for stratified population.
#' @param tol Tolerance parameter for boundary convergence (on Z-scale).
#'
#' @return A list with input parameters, analysis, and bound.
#'
#' @details
#' To be added.
#'
#' @export
#'
#' @examples
#' library(gsDesign)
#'
#' # Example 1 ----
#' # unstratified group sequential design
#' x <- gs_design_rd(
#'   p_c = tibble::tibble(stratum = "All", rate = .2),
#'   p_e = tibble::tibble(stratum = "All", rate = .15),
#'   info_frac = c(0.7, 1),
#'   rd0 = 0,
#'   alpha = .025,
#'   beta = .1,
#'   ratio = 1,
#'   stratum_prev = NULL,
#'   weight = "unstratified",
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign(k = 2, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' )
#'
#' y <- gs_power_rd(
#'   p_c = tibble::tibble(stratum = "All", rate = .2),
#'   p_e = tibble::tibble(stratum = "All", rate = .15),
#'   n = tibble::tibble(stratum = "All", n = x$analysis$n, analysis = 1:2),
#'   rd0 = 0,
#'   ratio = 1,
#'   weight = "unstratified",
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign(k = 2, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' )
#'
#' # The above 2 design share the same power with the same sample size and treatment effect
#' x$bound$probability[x$bound$bound == "upper" & x$bound$analysis == 2]
#' y$bound$probability[y$bound$bound == "upper" & y$bound$analysis == 2]
#'
#' # Example 2 ----
#' # stratified group sequential design
#' gs_design_rd(
#'   p_c = tibble::tibble(
#'     stratum = c("biomarker positive", "biomarker negative"),
#'     rate = c(.2, .25)
#'   ),
#'   p_e = tibble::tibble(
#'     stratum = c("biomarker positive", "biomarker negative"),
#'     rate = c(.15, .22)
#'   ),
#'   info_frac = c(0.7, 1),
#'   rd0 = 0,
#'   alpha = .025,
#'   beta = .1,
#'   ratio = 1,
#'   stratum_prev = tibble::tibble(
#'     stratum = c("biomarker positive", "biomarker negative"),
#'     prevalence = c(.4, .6)
#'   ),
#'   weight = "ss",
#'   upper = gs_spending_bound, lower = gs_b,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lpar = rep(-Inf, 2)
#' )
gs_design_rd <- function(p_c = tibble::tibble(stratum = "All", rate = .2),
                         p_e = tibble::tibble(stratum = "All", rate = .15),
                         info_frac = 1:3 / 3,
                         rd0 = 0,
                         alpha = .025,
                         beta = .1,
                         ratio = 1,
                         stratum_prev = NULL,
                         weight = c("unstratified", "ss", "invar"),
                         upper = gs_b,
                         lower = gs_b,
                         upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
                         lpar = c(qnorm(.1), rep(-Inf, 2)),
                         test_upper = TRUE,
                         test_lower = TRUE,
                         info_scale = c("h0_h1_info", "h0_info", "h1_info"),
                         binding = FALSE,
                         r = 18,
                         tol = 1e-6,
                         h1_spending = TRUE) {
  # Check input values ----
  info_scale <- match.arg(info_scale)

  weight <- if (methods::missingArg(weight)) {
    "unstratified"
  } else {
    match.arg(weight)
  }
  n_strata <- length(unique(p_c$stratum))
  if (methods::missingArg(info_frac)) {
    k <- 1
  } else {
    k <- length(info_frac)
  }

  # Calculate the sample size under fixed design ----
  x_fix <- gs_info_rd(
    p_c = p_c,
    p_e = p_e,
    n = tibble(
      analysis = 1,
      stratum = p_c$stratum,
      n = if (is.null(stratum_prev)) {
        1
      } else {
        (stratum_prev |> mutate(x = prevalence / sum(prevalence)))$x
      }
    ),
    rd0 = rd0,
    ratio = ratio,
    weight = weight
  )

  # Calculate the sample size under group sequential design ----
  x_gs <- gs_info_rd(
    p_c = p_c,
    p_e = p_e,
    n = tibble(
      analysis = rep(1:k, n_strata),
      stratum = rep(p_c$stratum, each = k),
      n = if (is.null(stratum_prev)) {
        info_frac
      } else {
        rep((stratum_prev |> mutate(x = prevalence / sum(prevalence)))$x, each = k) * info_frac
      }
    ),
    rd0 = rd0,
    ratio = ratio,
    weight = weight
  )

  if (k == 1) {
    x <- x_fix
  } else {
    x <- x_gs
  }

  if (h1_spending) {
    theta1 <- x$theta1
    info1 <- x$info1
  } else {
    theta1 <- 0
    info1 <- x$info0
  }

  y_gs <- gs_design_npe(
    theta = x$rd, theta1 = theta1,
    info = x$info1, info0 = x$info0, info1 = info1,
    info_scale = info_scale,
    alpha = alpha, beta = beta, binding = binding,
    upper = upper, upar = upar, test_upper = test_upper,
    lower = lower, lpar = lpar, test_lower = test_lower,
    r = r, tol = tol
  )

  # Get statistical information ----
  inflac_fct <- if (info_scale == "h0_info") {
    (y_gs |> filter(bound == "upper", analysis == k))$info0 / x_fix$info0[1]
  } else if (info_scale == "h1_info") {
    (y_gs |> filter(bound == "upper", analysis == k))$info1 / x_fix$info1[1]
  } else if (info_scale == "h0_h1_info") {
    (y_gs |> filter(bound == "upper", analysis == k))$info / x_fix$info1[1]
  }

  allout <- y_gs |>
    mutate(
      rd = x_fix$rd,
      rd0 = rd0,
      "~risk difference at bound" = z / sqrt(info) / theta * (rd - rd0) + rd0,
      "nominal p" = pnorm(-z),
      info_frac0 = if (sum(!is.na(info0)) == 0) {
        NA
      } else {
        info0 / max(info0)
      },
      n = inflac_fct * info_frac
    ) |>
    select(c(
      analysis, bound, n, rd, rd0, z, probability, probability0,
      info, info0, info_frac, info_frac0, `~risk difference at bound`, `nominal p`
    )) |>
    arrange(analysis, desc(bound))

  # Get input parameters to output ----
  input <- list(
    p_c = p_c, p_e = p_e,
    info_frac = info_frac, rd0 = rd0, alpha = alpha, beta = beta,
    ratio = ratio, stratum_prev = stratum_prev, weight = weight,
    upper = upper, upar = upar, test_upper = test_upper,
    lower = lower, lpar = lpar, test_lower = test_lower,
    h1_spending = h1_spending,
    binding = binding, info_scale = info_scale, r = r, tol = tol
  )

  # Get bounds to output ----
  bound <- allout |>
    select(analysis, bound, probability, probability0, z, `~risk difference at bound`, `nominal p`)

  # Get analysis summary to output ----
  analysis <- allout |>
    filter(bound == "upper") |>
    select(analysis, n, rd, rd0, info, info0, info_frac, info_frac0)

  # Return the output ----
  ans <- structure(
    list(
      design = "rd",
      input = input,
      bound = bound |> filter(!is.infinite(z)),
      analysis = analysis
    ),
    class = "gs_design",
    binding = binding,
    uninteger_is_from = "gs_design_rd"
  )

  return(ans)
}
