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

#' Information and effect size under risk difference
#'
#' @param p_c Rate at the control group.
#' @param p_e Rate at the experimental group.
#' @param n Sample size.
#' @param rd0 The risk difference under H0.
#' @param ratio Experimental:Control randomization ratio.
#' @param weight Weighting method, can be `"unstratified"`, `"ss"`,
#'   or `"invar"`.
#'
#' @return A tibble with columns as analysis index, sample size,
#'   risk difference, risk difference under null hypothesis, theta1
#'   (standardized treatment effect under alternative hypothesis),
#'   theta0 (standardized treatment effect under null hypothesis),
#'   and statistical information.
#'
#' @importFrom dplyr group_by left_join mutate select summarize ungroup
#'
#' @export
#'
#' @examples
#' # Example 1 ----
#' # unstratified case with H0: rd0 = 0
#' gs_info_rd(
#'   p_c = tibble::tibble(stratum = "All", rate = .15),
#'   p_e = tibble::tibble(stratum = "All", rate = .1),
#'   n = tibble::tibble(stratum = "All", n = c(100, 200, 300), analysis = 1:3),
#'   rd0 = 0,
#'   ratio = 1
#' )
#'
#' # Example 2 ----
#' # unstratified case with H0: rd0 != 0
#' gs_info_rd(
#'   p_c = tibble::tibble(stratum = "All", rate = .2),
#'   p_e = tibble::tibble(stratum = "All", rate = .15),
#'   n = tibble::tibble(stratum = "All", n = c(100, 200, 300), analysis = 1:3),
#'   rd0 = 0.005,
#'   ratio = 1
#' )
#'
#' # Example 3 ----
#' # stratified case under sample size weighting and H0: rd0 = 0
#' gs_info_rd(
#'   p_c = tibble::tibble(stratum = c("S1", "S2", "S3"), rate = c(.15, .2, .25)),
#'   p_e = tibble::tibble(stratum = c("S1", "S2", "S3"), rate = c(.1, .16, .19)),
#'   n = tibble::tibble(
#'     stratum = rep(c("S1", "S2", "S3"), each = 3),
#'     analysis = rep(1:3, 3),
#'     n = c(50, 100, 200, 40, 80, 160, 60, 120, 240)
#'   ),
#'   rd0 = 0,
#'   ratio = 1,
#'   weight = "ss"
#' )
#'
#' # Example 4 ----
#' # stratified case under inverse variance weighting and H0: rd0 = 0
#' gs_info_rd(
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
#'     n = c(50, 100, 200, 40, 80, 160, 60, 120, 240)
#'   ),
#'   rd0 = 0,
#'   ratio = 1,
#'   weight = "invar"
#' )
#'
#' # Example 5 ----
#' # stratified case under sample size weighting and H0: rd0 != 0
#' gs_info_rd(
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
#'     n = c(50, 100, 200, 40, 80, 160, 60, 120, 240)
#'   ),
#'   rd0 = 0.02,
#'   ratio = 1,
#'   weight = "ss"
#' )
#'
#' # Example 6 ----
#' # stratified case under inverse variance weighting and H0: rd0 != 0
#' gs_info_rd(
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
#'     n = c(50, 100, 200, 40, 80, 160, 60, 120, 240)
#'   ),
#'   rd0 = 0.02,
#'   ratio = 1,
#'   weight = "invar"
#' )
#'
#' # Example 7 ----
#' # stratified case under inverse variance weighting and H0: rd0 != 0 and
#' # rd0 difference for different statum
#' gs_info_rd(
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
#'     n = c(50, 100, 200, 40, 80, 160, 60, 120, 240)
#'   ),
#'   rd0 = tibble::tibble(
#'     stratum = c("S1", "S2", "S3"),
#'     rd0 = c(0.01, 0.02, 0.03)
#'   ),
#'   ratio = 1,
#'   weight = "invar"
#' )
gs_info_rd <- function(
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
      n = c(100, 200, 300),
      analysis = 1:3
    ),
    rd0 = 0,
    ratio = 1,
    weight = c("unstratified", "ss", "invar")) {
  n_analysis <- max(n$analysis)
  weight <- match.arg(weight)

  # Pool the input arguments together ----
  suppressMessages(
    tbl <- n %>%
      left_join(p_c) %>%
      dplyr::rename(p_c = rate) %>%
      left_join(p_e) %>%
      dplyr::rename(p_e = rate) %>%
      left_join(if ("data.frame" %in% class(rd0)) {
        rd0
      } else {
        tibble::tibble(analysis = 1:n_analysis, rd0 = rd0)
      }) %>%
      mutate(
        n_e = n / (1 + ratio),
        n_c = n * ratio / (1 + ratio),
        d = ifelse(p_c > p_e, 1, -1),
        p_pool_per_k_per_s = (n_c * p_c + n_e * p_e) / n,
        p_e0 = (p_c + ratio * p_e - d * rd0) / (ratio + 1),
        p_c0 = p_e0 + d * rd0
      )
  )

  # Calculate the variance of the risk difference ----
  if (is.numeric(rd0) && rd0 == 0) {
    tbl <- tbl %>% mutate(
      sigma2_H0_per_k_per_s = p_pool_per_k_per_s * (1 - p_pool_per_k_per_s) * (1 / n_c + 1 / n_e),
      sigma2_H1_per_k_per_s = p_c * (1 - p_c) / n_c + p_e * (1 - p_e) / n_e
    )
  } else if ("data.frame" %in% class(rd0) || rd0 != 0) {
    tbl <- tbl %>% mutate(
      sigma2_H0_per_k_per_s = p_c0 * (1 - p_c0) / n_c + p_e0 * (1 - p_e0) / n_e,
      sigma2_H1_per_k_per_s = p_c * (1 - p_c) / n_c + p_e * (1 - p_e) / n_e
    )
  }

  # Assign weights ----
  if (weight == "unstratified") {
    tbl <- tbl %>% mutate(weight_per_k_per_s = 1)
  } else if (weight == "ss") {
    suppressMessages(
      tbl <- tbl %>%
        left_join(
          tbl %>%
            dplyr::group_by(analysis) %>%
            summarize(sum_ss = sum(n_c * n_e / (n_c + n_e)))
        ) %>%
        mutate(weight_per_k_per_s = n_c * n_e / (n_c + n_e) / sum_ss) %>%
        select(-sum_ss)
    )
  } else if (weight == "invar") {
    suppressMessages(
      tbl <- tbl %>%
        left_join(
          tbl %>%
            dplyr::group_by(analysis) %>%
            summarize(sum_inv_var_per_s = sum(1 / sigma2_H1_per_k_per_s))
        ) %>%
        mutate(weight_per_k_per_s = 1 / sigma2_H1_per_k_per_s / sum_inv_var_per_s) %>%
        select(-sum_inv_var_per_s)
    )
  }

  # Pool the strata together ----
  ans <- tbl %>%
    dplyr::group_by(analysis) %>%
    summarize(
      n = sum(n),
      rd = sum((p_c - p_e) * d * weight_per_k_per_s),
      rd0 = sum(rd0 * weight_per_k_per_s),
      sigma2_H0 = sum(if (sum(rd0 == 0) == 0) {
        weight_per_k_per_s^2 * p_pool_per_k_per_s * (1 - p_pool_per_k_per_s) * (1 / n_c + 1 / n_e)
      } else {
        weight_per_k_per_s^2 * p_c0 * (1 - p_c0) / n_c + weight_per_k_per_s^2 * p_e0 * (1 - p_e0) / n_e
      }),
      sigma2_H1 = sum(weight_per_k_per_s^2 * p_c * (1 - p_c) / n_c + weight_per_k_per_s^2 * p_e * (1 - p_e) / n_e)
    ) %>%
    mutate(
      theta1 = rd,
      theta0 = rd0,
      info1 = 1 / sigma2_H1,
      info0 = 1 / sigma2_H0
    ) %>%
    dplyr::ungroup() %>%
    select(analysis, n, rd, rd0, theta1, theta0, info1, info0)

  return(ans)
}
