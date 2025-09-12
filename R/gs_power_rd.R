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

#' Group sequential design power of binary outcome measuring in risk difference
#'
#' @param p_c Rate at the control group.
#' @param p_e Rate at the experimental group.
#' @param n Sample size.
#' @param rd0 Treatment effect under super-superiority designs, the default is 0.
#' @param ratio Experimental:control randomization ratio.
#' @param upper Function to compute upper bound.
#' @param upar Parameters passed to `upper`.
#' @param lower Function to compare lower bound.
#' @param lpar Parameters passed to `lower`.
#' @param info_scale Information scale for calculation. Options are:
#'   - `"h0_h1_info"` (default): variance under both null and alternative hypotheses is used.
#'   - `"h0_info"`: variance under null hypothesis is used.
#'   - `"h1_info"`: variance under alternative hypothesis is used.
#' @param weight Weighting method, can be `"unstratified"`, `"ss"`,
#'   or `"invar"`.
#' @param binding Indicator of whether futility bound is binding;
#'   default of `FALSE` is recommended.
#' @param test_upper Indicator of which analyses should include an upper
#'   (efficacy) bound; single value of `TRUE` (default)  indicates all analyses;
#'   otherwise, a logical vector of the same length as `info` should indicate
#'   which analyses will have an efficacy bound.
#' @param test_lower Indicator of which analyses should include a lower bound;
#'   single value of `TRUE` (default) indicates all analyses;
#'   single value `FALSE` indicated no lower bound; otherwise,
#'   a logical vector of the same length as `info` should indicate which
#'   analyses will have a lower bound.
#' @param r Integer value controlling grid for numerical integration as in
#'   Jennison and Turnbull (2000); default is 18, range is 1 to 80.
#'   Larger values provide larger number of grid points and greater accuracy.
#'   Normally, `r` will not be changed by the user.
#' @param tol Tolerance parameter for boundary convergence (on Z-scale).
#'
#' @return A list with input parameter, analysis, and bound.
#'
#' @export
#'
#' @examples
#' # Example 1 ----
#' library(gsDesign)
#'
#' # unstratified case with H0: rd0 = 0
#' gs_power_rd(
#'   p_c = tibble::tibble(
#'     stratum = "All",
#'     rate = .2
#'   ),
#'   p_e = tibble::tibble(
#'     stratum = "All",
#'     rate = .15
#'   ),
#'   n = tibble::tibble(
#'     stratum = "All",
#'     n = c(20, 40, 60),
#'     analysis = 1:3
#'   ),
#'   rd0 = 0,
#'   ratio = 1,
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' )
#'
#' # Example 2 ----
#' # unstratified case with H0: rd0 != 0
#' gs_power_rd(
#'   p_c = tibble::tibble(
#'     stratum = "All",
#'     rate = .2
#'   ),
#'   p_e = tibble::tibble(
#'     stratum = "All",
#'     rate = .15
#'   ),
#'   n = tibble::tibble(
#'     stratum = "All",
#'     n = c(20, 40, 60),
#'     analysis = 1:3
#'   ),
#'   rd0 = 0.005,
#'   ratio = 1,
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' )
#'
#' # use spending function
#' gs_power_rd(
#'   p_c = tibble::tibble(
#'     stratum = "All",
#'     rate = .2
#'   ),
#'   p_e = tibble::tibble(
#'     stratum = "All",
#'     rate = .15
#'   ),
#'   n = tibble::tibble(
#'     stratum = "All",
#'     n = c(20, 40, 60),
#'     analysis = 1:3
#'   ),
#'   rd0 = 0.005,
#'   ratio = 1,
#'   upper = gs_spending_bound,
#'   lower = gs_b,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' )
#'
#' # Example 3 ----
#' # stratified case under sample size weighting and H0: rd0 = 0
#' gs_power_rd(
#'   p_c = tibble::tibble(
#'     stratum = c("S1", "S2", "S3"),
#'     rate = c(.15, .2, .25)
#'   ),
#'   p_e = tibble::tibble(
#'     stratum = c("S1", "S2", "S3"),
#'     rate = c(.1, .16, .19)
#'   ),
#'   n = tibble::tibble(
#'     stratum = rep(c("S1", "S2", "S3"), each = 3),
#'     analysis = rep(1:3, 3),
#'     n = c(10, 20, 24, 18, 26, 30, 10, 20, 24)
#'   ),
#'   rd0 = 0,
#'   ratio = 1,
#'   weight = "ss",
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' )
#'
#' # Example 4 ----
#' # stratified case under inverse variance weighting and H0: rd0 = 0
#' gs_power_rd(
#'   p_c = tibble::tibble(
#'     stratum = c("S1", "S2", "S3"),
#'     rate = c(.15, .2, .25)
#'   ),
#'   p_e = tibble::tibble(
#'     stratum = c("S1", "S2", "S3"),
#'     rate = c(.1, .16, .19)
#'   ),
#'   n = tibble::tibble(
#'     stratum = rep(c("S1", "S2", "S3"), each = 3),
#'     analysis = rep(1:3, 3),
#'     n = c(10, 20, 24, 18, 26, 30, 10, 20, 24)
#'   ),
#'   rd0 = 0,
#'   ratio = 1,
#'   weight = "invar",
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' )
#'
#' # Example 5 ----
#' # stratified case under sample size weighting and H0: rd0 != 0
#' gs_power_rd(
#'   p_c = tibble::tibble(
#'     stratum = c("S1", "S2", "S3"),
#'     rate = c(.15, .2, .25)
#'   ),
#'   p_e = tibble::tibble(
#'     stratum = c("S1", "S2", "S3"),
#'     rate = c(.1, .16, .19)
#'   ),
#'   n = tibble::tibble(
#'     stratum = rep(c("S1", "S2", "S3"), each = 3),
#'     analysis = rep(1:3, 3),
#'     n = c(10, 20, 24, 18, 26, 30, 10, 20, 24)
#'   ),
#'   rd0 = 0.02,
#'   ratio = 1,
#'   weight = "ss",
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' )
#'
#' # Example 6 ----
#' # stratified case under inverse variance weighting and H0: rd0 != 0
#' gs_power_rd(
#'   p_c = tibble::tibble(
#'     stratum = c("S1", "S2", "S3"),
#'     rate = c(.15, .2, .25)
#'   ),
#'   p_e = tibble::tibble(
#'     stratum = c("S1", "S2", "S3"),
#'     rate = c(.1, .16, .19)
#'   ),
#'   n = tibble::tibble(
#'     stratum = rep(c("S1", "S2", "S3"), each = 3),
#'     analysis = rep(1:3, 3),
#'     n = c(10, 20, 24, 18, 26, 30, 10, 20, 24)
#'   ),
#'   rd0 = 0.03,
#'   ratio = 1,
#'   weight = "invar",
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' )
gs_power_rd <- function(
    p_c = tibble::tibble(
      stratum = "All",
      rate = .2
    ),
    p_e = tibble::tibble(
      stratum = "All",
      rate = .15
    ),
    n = tibble::tibble(
      stratum = "All",
      n = c(40, 50, 60),
      analysis = 1:3
    ),
    rd0 = 0,
    ratio = 1,
    weight = c("unstratified", "ss", "invar"),
    upper = gs_b,
    lower = gs_b,
    upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
    lpar = c(qnorm(.1), rep(-Inf, 2)),
    info_scale = c("h0_h1_info", "h0_info", "h1_info"),
    binding = FALSE,
    test_upper = TRUE,
    test_lower = TRUE,
    r = 18,
    tol = 1e-6) {
  # get the number of analysis
  n_analysis <- max(n$analysis)
  # get the info_scale
  info_scale <- match.arg(info_scale)
  # get the weighting scheme
  weight <- if (methods::missingArg(weight)) {
    "unstratified"
  } else {
    match.arg(weight)
  }

  # Calculate the asymptotic variance and statistical information ----
  x <- gs_info_rd(
    p_c = p_c,
    p_e = p_e,
    n = n,
    rd0 = rd0,
    ratio = ratio,
    weight = weight
  )

  # Given the above statistical information calculate the power ----
  y_h1 <- gs_power_npe(
    theta = x$rd,
    info = x$info1,
    info0 = x$info0,
    info1 = x$info1,
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
    theta = x$rd0,
    info = x$info0,
    info0 = x$info0,
    info1 = x$info1,
    info_scale = info_scale,
    binding = binding,
    upper = upper,
    upar = upar,
    test_upper = test_upper,
    lower = lower,
    lpar = lpar,
    test_lower = test_lower,
    r = r,
    tol = tol
  )

  # Organize the outputs ----
  # summarize the bounds
  suppressMessages(
    bound <- y_h1 |>
      mutate(
        `~risk difference at bound` = z / sqrt(info) / theta * (x$rd[1] - x$rd0[1]) + x$rd0[1],
        `nominal p` = pnorm(-z)
      ) |>
      left_join(
        y_h0 |>
          select(analysis, bound, probability) |>
          rename(probability0 = probability)
      ) |>
      select(analysis, bound, probability, probability0, z, `~risk difference at bound`, `nominal p`)
  )
  # summarize the analysis
  suppressMessages(
    analysis <- x |>
      select(analysis, n, rd, rd0, theta1, theta0) |>
      left_join(
        y_h1 |>
          select(analysis, info, info_frac) |>
          unique()
      ) |>
      left_join(
        y_h0 |>
          select(analysis, info, info_frac) |>
          rename(info0 = info, info_frac0 = info_frac) |>
          unique()
      ) |>
      select(analysis, n, rd, rd0, theta1, theta0, info, info0, info_frac, info_frac0)
  )

  ans <- structure(
    list(
      design = "rd",
      bound = bound |> filter(!is.infinite(z)),
      analysis = analysis
    ),
    class = "gs_design",
    binding = binding,
    uninteger_is_from = "gs_power_rd"
  )

  return(ans)
}
