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

#' Conditional power computation with non-constant effect
#'
#' @param theta0 Natural parameter for null hypothesis (default is all zero).
#' @param theta1 Natural parameter for alternate hypothesis.
#' @param theta_hat Natural parameter estimated based on observed blinded data.
#' @param info0 Statistical information under null hypothesis.
#' @param info1 Statistical information under alternative hypothesis.
#' @param info_hat Statistical information estimated based on observed blinded data.
#' @param info_scale Information scale for calculation. Options are:
#'   - `"h0_h1_info"` (default): variance under both null and alternative hypotheses is used.
#'   - `"h0_info"`: variance under null hypothesis is used.
#'   - `"h1_info"`: variance under alternative hypothesis is used.
#' @param i Index of current analysis.
#' @param j Index of the future analysis to calculate the conditional power.
#' @param z_i Interim z-value at analysis i (scalar).
#' @param z_j Efficacy boundary at analysis j (scalar).
#' @param local_alternative Logical value whether the local alternative hypothesis is used or not.
#'
#' @export
#'
#' @examples
#' library(gsDesign2)
#' gs_cp_npe(theta0 = c(0, 0, 0),
#'           theta1 = c(0.1, 0.2, 0.3),
#'           theta_hat = NULL,
#'           info0 = c(10, 20, 30),
#'           info1 = c(9.8, 19.6, 29.9),
#'           info_hat = NULL,
#'           info_scale = "h0_h1_info",
#'           i = 1, j = 3,
#'           z_i = 1.5, z_j = 1.96,
#'           local_alternative = TRUE)
gs_cp_npe <- function(theta0 = NULL, theta1 = NULL, theta_hat = NULL, # 3 theta
                      info0 = NULL, info1 = NULL, info_hat = NULL, # 3 info
                      info_scale = c("h0_h1_info", "h0_info", "h1_info"),
                      i = NULL, j = NULL,
                      z_i = NULL, z_j = NULL,
                      local_alternative = TRUE
                      ) {
  # --------------------------------------- #
  #       input checking                    #
  # --------------------------------------- #
  n_analysis <- length(info0)

  # check theta0
  if (is.null(theta0)) {
    theta0 <- rep(0, n_analysis)
  } else if (length(theta0) == 1) {
    theta0 <- rep(theta0, n_analysis)
  }

  # check theta1
  if (is.null(theta1)) {
    stop("Please provide theta1 (treatment effect under H1) to calculate conditional power.")
  } else if (length(theta1) == 1) {
    theta1 <- rep(theta1, n_analysis)
  }

  # check theta_hat
  if (is.null(theta_hat)) {
    theta_hat <- rep(0, n_analysis)
  } else if (length(theta_hat) == 1) {
    theta_hat <- rep(theta_hat, n_analysis)
  }

  # check info0
  if (is.null(info0)) {
    stop("Please provide info0 (statistical information under H0) to calculate conditional power.")
  }

  # check info1
  if (is.null(info1)) {
    stop("Please provide info1 (statistical information under H1) to calculate conditional power.")
  }

  # check info_hat
  if (is.null(info_hat)) {
    info_hat <- info1
  }

  # set up info_scale
  info_scale <- match.arg(info_scale)

  if (info_scale == "h0_info") {
    info1 <- info0
    info_hat <- info0
  }

  if (info_scale == "h1_info") {
    info0 <- info1
    info_hat <- info1
  }

  # check info
  check_info(info0)
  check_info(info1)
  if (length(info0) != length(info_hat)) stop("Length of info_hat, info0 must be the same!")
  if (length(info1) != length(info_hat)) stop("Length of info_hat, info1 must be the same!")

  # calculate information fraction
  info_frac <- info0 / max(info0)

  # --------------------------------------- #
  #   calculate conditional error           #
  # --------------------------------------- #
  numerator <- sqrt(info_frac[j]) * z_j - sqrt(info_frac[i]) * z_i
  denominator <- sqrt(info_frac[j] - info_frac[i])
  conditional_error <- pnorm(numerator / denominator, lower.tail = FALSE)

  # --------------------------------------- #
  #   calculate conditional power under H1  #
  # --------------------------------------- #
  numerator1 <- z_j * sqrt(info_frac[j]) - z_i * sqrt(info_frac[i])
  numerator2 <- theta1[j] * sqrt(info_frac[j] * info0[j]) - theta1[i] * sqrt(info_frac[i] * info0[i])
  denominator <- if (local_alternative) {
    sqrt(info_frac[j] - info_frac[i])
  } else {
    sqrt(info_frac[j] * info0[j] / info1[j] - info_frac[i] * info0[i] / info1[i])
  }
  conditional_power_h1 <- pnorm((numerator1 - numerator2)  / denominator, lower.tail = FALSE)

  # --------------------------------------- #
  #   calculate conditional power with est  #
  # --------------------------------------- #
  numerator1 <- z_j * sqrt(info_frac[j]) - z_i * sqrt(info_frac[i])
  numerator2 <- theta1[j] * sqrt(info_frac[j] * info0[j]) - theta_hat[i] * sqrt(info_frac[i] * info_hat[i])
  denominator <- if (local_alternative) {
    sqrt(info_frac[j] - info_frac[i])
  } else {
    sqrt(info_frac[j] * info0[j] / info1[j] - info_frac[i] * info_hat[i] / info1[i])
  }
  conditional_power_est <- pnorm((numerator1 - numerator2)  / denominator, lower.tail = FALSE)

  # --------------------------------------- #
  #   output results                        #
  # --------------------------------------- #
  ans <- tibble::tibble(scenario = c("Under H0", "Under H1", "Under interim estimation"),
                        i = i, z_i = z_i,
                        j = j, z_j = z_j,
                        cond_power = c(conditional_error, conditional_power_h1, conditional_power_est),
                        theta_over_analyses = c(paste0(theta0, collapse = ", "),
                                                paste0(round(theta1, 4), collapse = ", "),
                                                paste0(round(theta_hat, 4), collapse = ", ")),
                        info_over_analyses = c(paste0(info0, collapse = ","),
                                               paste0(round(info1, 4), collapse = ", "),
                                               paste0(round(info_hat, 4), collapse = ", "))
  )
  return(ans)
}
