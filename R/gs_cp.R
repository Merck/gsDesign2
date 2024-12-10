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


#' @title Group sequential design - Conditional Probability under non-proportional hazards
#' @description  \code{gs_cp()} computes conditional boundary crossing probabilities at future planned analyses
#' for a given group sequential design assuming an interim z-statistic at a specified interim analysis.
#' @param x An object of type \code{gsDesign2}
#' @param x_updated An object of type \code{gsDesign2}
#' @param i Analysis at which interim z-value is given; must be from 1 to max(x$analysis$analysis)
#' @param zi Interim z-value at analysis i (scalar)
#' @param local_alternative A TRUE or FALSE value that indicates if we apply local approximation theorem
#' @return A list with: theta -- a list containing theta0 (theta under H0, i.e., 0,) and theta1 (a vector of theta under H1)
#'                      upper_bound -- the upper bound value return by any gs_design_ahr function
#'                      upper_prob -- a list contains the conditional probability given zi under H0, H1 and estimated treatment effect, respectively
#'
#' @examples
#' library(gsDesign2)
#' library(dplyr)
#'
#' # Example
#'
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
#' upper <- gs_spending_bound
#' upar <- list(sf = sfLDOF, total_spend = alpha)
#' x <- gs_design_ahr(
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   alpha = alpha, beta = beta, ratio = ratio,
#'   info_scale = "h0_info",
#'   info_frac = NULL,
#'   analysis_time = c(20, 36, 50),
#'   upper = gs_spending_bound, upar = upar,
#'   lower = gs_b, lpar = rep(-Inf, 3),
#'   test_upper = TRUE, test_lower = FALSE) |> to_integer()

# Observed dataset at IA1
#' set.seed(123)

#' observed_data <- simtrial::sim_pw_surv(
#'   n = x$analysis$n[x$analysis$analysis == 3],
#'   stratum = data.frame(stratum = "All", p = 1),
#'   block = c(rep("control", 2), rep("experimental", 2)),
#'   enroll_rate = x$enroll_rate,
#'   fail_rate = (fail_rate |> simtrial::to_sim_pw_surv())$fail_rate,
#'   dropout_rate = (fail_rate |> simtrial::to_sim_pw_surv())$dropout_rate)

#' observed_data_ia1 <- observed_data |> simtrial::cut_data_by_date(x$analysis$time[1])
#' # cut_data_by_event
#' observed_event_ia1 <- sum(observed_data_ia1$event)
#' planned_event_ia2 <- x$analysis$event[2]
#' planned_event_fa <- x$analysis$event[3]
#'  # Example A1 ----
#' ustime <- c(observed_event_ia1/planned_event_fa, planned_event_ia2/planned_event_fa, 1)
#' x_updated <- gs_update_ahr(
#'   x = x,
#'   ustime = ustime,
#'   observed_data = list(observed_data_ia, NULL, NULL))
#'
#' gs_cp(x = x, x_updated = x_updated, i = 1, zi = 2, j = 3)




gs_cp <- function(x, x_updated, i, zi, j, local_alternative){

  # input check
  # Check if 'x' has an 'analysis' element and it's a matrix or data frame
  if (!is.list(x) || !"analysis" %in% names(x) || (!is.matrix(x$analysis) && !is.data.frame(x$analysis))) {
    stop("'x' should be a list containing an 'analysis' matrix or data frame.")
  }

  # Check if 'x_updated' has an 'analysis' element and it's a matrix or data frame
  if (!is.list(x_updated) || !"analysis" %in% names(x_updated) || (!is.matrix(x_updated$analysis) && !is.data.frame(x_updated$analysis))) {
    stop("'x_updated' should be a list containing an 'analysis' matrix or data frame.")
  }

  # Check if 'i' and 'j' are numeric and within the appropriate range
  if (!is.numeric(i) || !is.numeric(j)) {
    stop("'i' and 'j' should be numeric.")
  }

  if (!(1 <= i && i < j && j <= dim(x$analysis)[1]) |
      !(j <= dim(x_updated$analysis)[1])) {
    stop("Please ensure that 1 <= i < j and j <= dim(x$analysis)[1] and j <= dim(x_updated$analysis)[1].")
  }

  # Check the class of `x`, which should be an output from `gs_update_ahr`
  if(!("updated_design" %in% class(x_updated))){
    stop("'x_updated' should be an output from gs_update_ahr() ")
  }

  # Check the # of upper bound is equal to # of analysis
  # ! what is efficacy is only at IA2 and FA? >= 3 .. Check if j has upper bound
  if(length(which(x$bound$bound == "upper")) != dim(x$analysis)[1] |
     length(which(x_updated$bound$bound == "upper")) != dim(x_updated$analysis)[1]){
    stop("'x' and 'x_updated' should contains the same number of upper bounds as the number of analysis")
  }

  # obtain necessary information from x
  info_frac <- x$analysis$info_frac
  info0 <- x$analysis$info0 # under local asymptotic, assume H0 ~= H1
  info <- x$analysis$info
  info0_hat <- x_updated$analysis$info0
  # default theta: under H0 and under H1
  theta0 <- rep(0, dim(x$analysis)[1])
  theta1 <- x$analysis$theta
  theta_est <- x_updated$analysis$theta

  # compute the conditional probability under H0
  eff_bound <- as.data.frame(x$bound) %>%
    filter(bound == "upper") %>%
    select(z)

  # compute conditional power under H0
  prob0 <- 1 - pnorm((sqrt(info_frac[j])*eff_bound[j, ] - sqrt(info_frac[i])*zi)/sqrt(info_frac[j] - info_frac[i]))


  # compute conditional power under H1
  mu_star <- sqrt(info_frac[j])*sqrt(info0[j])*theta1[j] - sqrt(info_frac[i])*sqrt(info0[i])*theta1[i]

  if(local_alternative){
    sigma2_star <- info_frac[j] - info_frac[i]
  }else{
    sigma2_star <- info_frac[j]*info0[j]/info[j] - info_frac[i]*info0[i]/info[i]
  }

  prob1 <- 1 - pnorm((sqrt(info_frac[j])*eff_bound[j, ] - sqrt(info_frac[i])*zi - mu_star)/sqrt(sigma2_star))


  # compute conditional power under estimated theta
  mu_star_est <- sqrt(info_frac[j])*sqrt(info0[j])*theta1[j] - sqrt(info_frac[i])*sqrt(info0_hat[i])*theta_est[i]
  if(local_alternative){
    sigma2_star_est <- info_frac[j] - info_frac[i]*info0_hat[i]/info[i]
  }else{
    sigma2_star_est <- info_frac[j]*info0[j]/info[j] - info_frac[i]*info0_hat[i]/info[i]
  }

  prob_est <- 1 - pnorm((sqrt(info_frac[j])*eff_bound[j, ] - sqrt(info_frac[i])*zi - mu_star_est)/sqrt(sigma2_star_est))

  # return list of results
  output <- list(theta = list(theta0 = 0, theta1 = theta1, theta_est = theta_est),
                 upper_bound = eff_bound,
                 upper_prob = list(prob0 = prob0, prob1 = prob1, prob_est = prob_est))

  return(output)
}



