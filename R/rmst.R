#  Copyright (c) 2022 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co., Inc., Rahway, NJ, USA.
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

#' Sample Size Calculation based on RMST method 
#' 
#' @param enroll_rate enrollment rates
#' @param fail_rate failure and dropout rates
#' @param analysis_time Minimum time of analysis
#' @param ratio Experimental:Control randomization ratio
#' @param alpha One-sided Type I error (strictly between 0 and 1)
#' @param beta  Power (`NULL` to compute power or strictly between 0 and `1 - alpha` otherwise)
#' @param test A string specifies the type of statistical test.
#'             Default is \code{"survival difference"} (a Kaplan-Meier based test). 
#'             One can also set it as \code{"rmst difference"} (another Kaplan-Meier based test)
#' @param tau desired milestone for \code{test = "survival difference"} or \code{test = "rmst difference"}
#' @return a list with \code{enroll_rate}, \code{fail_rate}, \code{bounds}, \code{analysis} and \code{design}
#'
#' @examples 
#' # set enrollment rates
#' enroll_rate <- tibble::tibble(stratum = "All", duration = 12, rate = 500/12)
#' 
#' # set failure rates
#' fail_rate <- tibble::tibble(
#'   stratum = "All",
#'   duration = c(4, 100),
#'   fail_rate = log(2) / 15,  # median survival 15 month
#'   hr = c(1, .6),
#'   dropout_rate = 0.001)
#'
#' fixed_design_size_rmst(enroll_rate, fail_rate, analysis_time = 36)
#' fixed_design_size_rmst(enroll_rate, fail_rate, analysis_time = 36, beta = 1 - 0.887)
#' fixed_design_size_rmst(enroll_rate, fail_rate, analysis_time = 36, tau = 18)
#' 
#' @noRd
fixed_design_size_rmst <- function(enroll_rate, 
                                   fail_rate, 
                                   analysis_time,
                                   ratio = 1,
                                   alpha = 0.025, 
                                   beta = 0.1,
                                   test = "rmst difference",
                                   tau = NULL){
  
  gs_arm <- gs_create_arm(enroll_rate, fail_rate, ratio = ratio,  total_time = analysis_time) 
  arm0 <- gs_arm[["arm0"]]
  arm1 <- gs_arm[["arm1"]]
  
  n <- sum(enroll_rate$duration * enroll_rate$rate)
  
  # Sample size for RMST at cut point 
  npsurv <- npsurvSS::size_two_arm(arm0, arm1, 
                                   alpha = alpha, power = 1 - beta,  
                                   test = list(test = test, milestone = if(is.null(tau)){arm0$total_time}else{tau})) 
  bounds <- tibble::tibble(
    Analysis = 1,
    Bound = "Upper",
    Probability = 1 - beta,
    Probability0 = alpha,
    Z = - qnorm(alpha)
  )
  
  analysis <- tibble::tibble(
    Analysis = 1, 
    Time = analysis_time,
    N = npsurv[["n"]], 
    Events = npsurv[["d"]]
  )
  
  
  res <- list(enroll_rate = enroll_rate %>% mutate(rate = rate * npsurv[["n"]] / n), 
              fail_rate = fail_rate, 
              bounds = bounds,
              analysis = analysis)
  
  res
}


#' Power calculation based on RMST method 
#' 
#' @param enroll_rate enrollment rates
#' @param fail_rate failure and dropout rates
#' @param analysis_time Minimum time of analysis
#' @param ratio Experimental:Control randomization ratio
#' @param alpha One-sided Type I error (strictly between 0 and 1)
#' @param test A string specifies the type of statistical test.
#'             Default is \code{"survival difference"} (a Kaplan-Meier based test). 
#'             One can also set it as \code{"rmst difference"} (another Kaplan-Meier based test)
#' @param tau desired milestone for \code{test = "survival difference"} or \code{test = "rmst difference"}
#' 
#' @examples 
#' # set enrollment rates
#' enroll_rate <- tibble::tibble(stratum = "All", duration = 12, rate = 500/12)
#' 
#' # set failure rates
#' fail_rate <- tibble::tibble(
#'   stratum = "All",
#'   duration = c(4, 100),
#'   fail_rate = log(2) / 15,  # median survival 15 month
#'   hr = c(1, .6),
#'   dropout_rate = 0.001)
#'
#' fixed_design_power_rmst(enroll_rate, fail_rate, analysis_time = 36)
#' fixed_design_power_rmst(enroll_rate, fail_rate, analysis_time = 36, tau = 18)
#' 
#' @noRd
fixed_design_power_rmst <- function(enroll_rate, 
                                    fail_rate, 
                                    analysis_time,
                                    ratio = 1,
                                    alpha = 0.025,
                                    test = "rmst difference",
                                    tau = NULL){
  
  gs_arm <- gs_create_arm(enroll_rate, fail_rate, ratio = ratio,  total_time = analysis_time) 
  arm0 <- gs_arm[["arm0"]]
  arm1 <- gs_arm[["arm1"]]
  
  n <- sum(enroll_rate$duration * enroll_rate$rate)
  n0 <- n / (ratio + 1)
  n1 <- n - n0
  arm0$size <- n0 
  arm1$size <- n1
  
  d <- prob_event.arm(arm0, tmax =  arm0$total_time) * n0 + prob_event.arm(arm1, tmax =  arm0$total_time) * n1
  
  # Sample size for RMST at cut point 
  npsurv <- npsurvSS::power_two_arm(arm0, arm1, 
                                    alpha = alpha, 
                                    test = list(test = test, milestone = if(is.null(tau)){arm0$total_time}else{tau})) 
  
  bounds <- tibble::tibble(
    Analysis = 1,
    Bound = "Upper",
    Probability = npsurv,
    Probability0 = alpha,
    Z = - qnorm(alpha))
  
  analysis <- tibble::tibble(
    Analysis = 1, 
    Time = analysis_time,
    N = n, 
    Events = d)
  
  res <- list(enroll_rate = enroll_rate, 
              fail_rate = fail_rate, 
              bounds = bounds,
              analysis = analysis)
  
  res
}