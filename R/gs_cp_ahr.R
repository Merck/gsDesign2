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

#' Conditional power computation under the logrank test
#'
#' @param x A original design created by either [gs_design_ahr()] or [gs_power_ahr()] or [gs_update_ahr()].
#' @param i The index of the current analysis.
#' @param z_i The observed Z-score at analysis i.
#'
#' @export
#'
#' @return A list of conditional power under treatment effect of H0, H1, and interim estimation.
#' @examples
#' library(gsDesign)
#' library(gsDesign2)
#' library(dplyr)
#'
#' # ----------------------------- #
#' # Design parameters
#' # ---------------------------- #
#' alpha <- 0.025
#' beta <- 0.1
#' ratio <- 1
#'
#' # Enrollment
#' enroll_rate <- define_enroll_rate(
#'   duration = c(2, 2, 10),
#'   rate = (1:3) / 3)
#'
#' # Failure and dropout
#' fail_rate <- define_fail_rate(
#'   duration = c(3, Inf), fail_rate = log(2) / 9,
#'   hr = c(1, 0.6), dropout_rate = .0001)
#'
#' # IA and FA analysis time
#' analysis_time <- c(20, 36)
#'
#' # Randomization ratio
#' ratio <- 1
#'
#' # ----------------------------- #
#' # Original design
#' # ---------------------------- #
#' upper <- gs_spending_bound
#' upar <- list(sf = sfLDOF, total_spend = alpha)
#' x <- gs_design_ahr(
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   alpha = alpha, beta = beta, ratio = ratio,
#'   info_scale = "h0_info",
#'   info_frac = NULL,
#'   analysis_time = c(12, 24, 36),
#'   upper = gs_spending_bound, upar = upar,
#'   lower = gs_b, lpar = rep(-Inf, 3),
#'   test_upper = TRUE, test_lower = FALSE) |> to_integer()
#'
#' # ----------------------------- #
#' # Simulate a dataset to mimic
#' # observed IA1 data
#' # ---------------------------- #
#' set.seed(123)
#'
#' observed_data <- simtrial::sim_pw_surv(
#'   n = max(x$analysis$n),
#'   stratum = data.frame(stratum = "All", p = 1),
#'   block = c(rep("control", 2), rep("experimental", 2)),
#'   enroll_rate = x$enroll_rate,
#'   fail_rate = (fail_rate |> simtrial::to_sim_pw_surv())$fail_rate,
#'   dropout_rate = (fail_rate |> simtrial::to_sim_pw_surv())$dropout_rate)
#'
#' observed_data_ia1 <- observed_data |> simtrial::cut_data_by_date(x$analysis$time[1])
#' observed_event_ia1 <- sum(observed_data_ia1$event)
#' planned_event_ia1 <- x$analysis$event[1]
#' planned_event_fa <- x$analysis$event[3]
#'
#' # ----------------------------- #
#' # Update design based on
#' # observed IA1 data
#' # ---------------------------- #
#' xu <- gs_update_ahr(
#'   x = x,
#'   ustime = c(observed_event_ia1 / planned_event_fa, x$analysis$info_frac[2], 1),
#'   observed_data = list(observed_data_ia1, NULL, NULL))
#'
#' # ----------------------------- #
#' # Calculate conditional power
#' # ---------------------------- #
#' gs_cp_ahr(x = xu,
#'           i = 1, z_i = -qnorm(0.04))
#'
#' gs_cp_ahr(x = xu,
#'           i = 1, z_i = -qnorm(0.04))
gs_cp_ahr <- function(x, i = 1, z_i = 0) {

  n_analysis <- nrow(x$analysis)
  # --------------------------------------- #
  #   get theta0, theta1, theta_hat         #
  # --------------------------------------- #
  theta0 <- rep(0, n_analysis)
  theta <- x$analysis$theta

  # --------------------------------------- #
  #   get info0, info1, info_hat            #
  # --------------------------------------- #
  info0 <- x$analysis$info0
  info <- x$analysis$info

  # --------------------------------------- #
  #   calculate conditional prob            #
  #       crossing upper bound              #
  # --------------------------------------- #
  ans_upper <- NULL

  for (j in (i+1):n_analysis) {
    if (j %in% (x$bound %>% filter(bound == "upper"))$analysis) {

      cp_upper <- gs_cp_npe(i = i, j = j,
                            z_i = z_i,
                            z_j = (x$bound %>% filter(bound == "upper", analysis == j))$z ,
                            # 2 theta
                            theta0 = theta0, theta = theta,
                            # 2 info
                            info0 = info0, info = info)

      ans_upper_new <- tibble(analysis = j,
                              theta_over_analysis = cp_upper$theta_over_analysis,
                              cond_power = cp_upper$cond_power)

      ans_upper <- rbind(ans_upper, ans_upper_new)
    }
  }


  # --------------------------------------- #
  #   calculate conditional prob            #
  #       crossing lower bound              #
  # --------------------------------------- #
  ans_lower <- NULL

  for (j in (i+1):n_analysis) {
    if (j %in% (x$bound %>% filter(bound == "lower"))$analysis) {

      cp_lower <- gs_cp_npe(i = i, j = j,
                            z_i = z_i,
                            z_j = (x$bound %>% filter(bound == "lower", analysis == j))$z ,
                            # 2 theta
                            theta0 = theta0, theta = theta,
                            # 2 info
                            info0 = info0, info = info)

      ans_lower_new <- tibble(analysis = j,
                              theta_over_analysis = cp_lower$theta_over_analysis,
                              cond_power = 1 - cp_lower$cond_power)

      ans_lower <- rbind(ans_lower, ans_lower_new)

    }
  }

  return(list(upper = ans_upper %>% tidyr::pivot_wider(names_from = analysis, values_from = cond_power),
              lower = ans_lower %>% tidyr::pivot_wider(names_from = analysis, values_from = cond_power)))
}
