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

#' @importFrom tibble tibble
#' @importFrom gsDesign gsDesign sfLDOF
#' @importFrom stats qnorm
#' @importFrom dplyr mutate full_join select arrange desc
NULL

#' Group sequential design using average hazard ratio under non-proportional hazards
#'
#' @param enroll_rate enrollment rates
#' @param fail_rate failure and dropout rates
#' @param ratio Experimental:Control randomization ratio (not yet implemented)
#' @param alpha One-sided Type I error
#' @param beta Type II error
#' @param info_frac Targeted information fraction at each analysis
#' @param analysis_time Minimum time of analysis
#' @param binding indicator of whether futility bound is binding; default of FALSE is recommended
#' @param upper Function to compute upper bound
#' @param upar Parameter passed to \code{upper()}
#' @param lower Function to compute lower bound
#' @param lpar Parameter passed to \code{lower()}
#' @param info_scale the information scale for calculation
#' @param h1_spending Indicator that lower bound to be set by spending under alternate hypothesis (input \code{fail_rate})
#' if spending is used for lower bound
#' @param test_upper indicator of which analyses should include an upper (efficacy) bound; single value of TRUE (default) indicates all analyses;
#' otherwise, a logical vector of the same length as \code{info} should indicate which analyses will have an efficacy bound
#' @param test_lower indicator of which analyses should include an lower bound; single value of TRUE (default) indicates all analyses;
#' single value FALSE indicated no lower bound; otherwise, a logical vector of the same length as \code{info} should indicate which analyses will have a
#' lower bound
#' @param r  Integer, at least 2; default of 18 recommended by Jennison and Turnbull
#' @param tol Tolerance parameter for boundary convergence (on Z-scale)
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input analysis_time is a positive number or positive increasing sequence.
#'    \item Validate if input info_frac is a positive number or positive increasing sequence
#'    on (0, 1] with final value of 1.
#'    \item Validate if input info_frac and analysis_time  have the same length if both have length > 1.
#'    \item Get information at input analysis_time
#'    \itemize{
#'      \item Use \code{gs_info_ahr()} to get the information and effect size based on AHR approximation.
#'      \item Extract the final event.
#'      \item Check if input If needed for (any) interim analysis timing.
#'    }
#'    \item Add the analysis column to the information at input analysis_time.
#'    \item Add the sample size column to the information at input analysis_time using \code{expected_accural()}.
#'    \item Get sample size and bounds using \code{gs_design_npe()} and save them to bounds.
#'    \item Add Time, Events, AHR, N that have already been calculated to the bounds.
#'    \item Return a list of design enrollment, failure rates, and bounds.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @return a \code{tibble} with columns Analysis, Bound, Z, Probability, theta, Time, AHR, Events
#' @details Need to be added
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
#' gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = 36)
#' 
#' # ----------------- #
#' #    example 5      #
#' # ----------------- #
#' # multiple analysis times & info_frac
#' # driven by times
#' gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
#' # driven by info_frac
#' gs_design_ahr(info_frac = c(1/3, .8, 1), analysis_time = c(12, 25, 36))
#' 
#' # ----------------- #
#' #    example 6      #
#' # ----------------- #
#' # 2-sided symmetric design with O'Brien-Fleming spending
#' gs_design_ahr(
#'   analysis_time = c(12, 24, 36),
#'   binding = TRUE,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   h1_spending = FALSE)
#' 
#' # 2-sided asymmetric design with O'Brien-Fleming upper spending
#' # Pocock lower spending under H1 (NPH)
#' gs_design_ahr(
#'   analysis_time = c(12, 24, 36),
#'   binding = TRUE,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDPocock, total_spend = 0.1, param = NULL, timing = NULL),
#'   h1_spending = TRUE)
#'   
gs_design_ahr <- function(enroll_rate = tibble(Stratum = "All", duration = c(2, 2, 10), rate = c(3, 6, 9)),
                          fail_rate = tibble(Stratum = "All", duration = c(3, 100), fail_rate = log(2) / c(9, 18),
                                             hr = c(.9, .6), dropout_rate = rep(.001, 2)),
                          alpha = 0.025, beta = 0.1, 
                          info_frac = NULL, analysis_time = 36, 
                          ratio = 1, binding = FALSE,
                          upper = gs_b,         
                          upar = gsDesign::gsDesign(k = 3, test.type = 1, n.I = c(.25, .75, 1), sfu = sfLDOF, sfupar = NULL)$upper$bound,
                          lower = gs_b,
                          lpar = c(qnorm(.1), -Inf, -Inf), 
                          h1_spending = TRUE,
                          test_upper = TRUE,
                          test_lower = TRUE,
                          info_scale = c(0, 1, 2),
                          r = 18,
                          tol = 1e-6){
  # --------------------------------------------- #
  #     initialization                             #
  # --------------------------------------------- #
  if(is.null(info_frac)){info_frac <- 1}
  info_scale <- if(methods::missingArg(info_scale)){2}else{match.arg(as.character(info_scale), choices = 0:2)}

  # --------------------------------------------- #
  #     check inputs                              #
  # --------------------------------------------- #
  check_analysis_time(analysis_time)
  check_info_frac(info_frac)
  if((length(analysis_time) > 1) & (length(info_frac) > 1) & (length(info_frac) != length(analysis_time))){
    stop("gs_design_ahr() info_frac and analysis_time must have the same length if both have length > 1!")
  } 
  
  # --------------------------------------------- #
  #     get information at input analysis_time    #
  # --------------------------------------------- #
  y <- gs_info_ahr(enroll_rate, fail_rate, ratio = ratio, events = NULL, analysis_time = analysis_time)
  
  finalEvents <- y$Events[nrow(y)]
  IFalt <- y$Events / finalEvents
  
  # --------------------------------------------- #
  #     check if info_frac needed for IA timing   #
  # --------------------------------------------- #
  K <- max(length(analysis_time), length(info_frac))
  nextTime <- max(analysis_time)
  # if info_frac is not provided by the users
  if(length(info_frac) == 1){
    info_frac <- IFalt
  }else{
    # if there are >= 2 analysis
    IFindx <- info_frac[1:(K-1)]
    for(i in seq_along(IFindx)){
      # if ...
      if(length(IFalt) == 1){
        y <- rbind(expected_time(enroll_rate = enroll_rate, fail_rate = fail_rate, 
                           ratio = ratio, target_event = info_frac[K - i] * finalEvents, 
                           interval = c(.01, nextTime)) %>% 
                     mutate(theta = -log(AHR), Analysis = K - i),
                   y)
      }else if(info_frac[K-i] > IFalt[K-i]){
        # if the planned info_frac > info_frac under H1
        y[K - i,] <- expected_time(enroll_rate = enroll_rate, fail_rate = fail_rate, 
                             ratio = ratio, target_event = info_frac[K - i] * finalEvents, 
                             interval = c(.01, nextTime)) %>%
          dplyr::transmute(Analysis = K - i, Time, Events, AHR, theta = -log(AHR), info, info0)
      } 
      nextTime <- y$Time[K - i]
    }
  }
  
  # update `y` (an object from `gs_power_ahr`) with 
  # 1) analysis NO.
  # 2) the accrual sample size, i.e., `N`
  # 3) `theta1` and `info1`
  y$Analysis <- 1:K
  y$N <- expected_accural(time = y$Time, enroll_rate = enroll_rate)
  if(h1_spending){
    theta1 <- y$theta  
    info1 <- y$info
  }else{
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
      r = r, tol = tol)
  )
  
  allout <- allout %>%
    # add `~HR at bound`, `HR generic` and `Nominal p`
    mutate("~HR at bound" = exp(-Z / sqrt(info0)), "Nominal p" = pnorm(-Z)) %>% 
    # Add `Time`, `Events`, `AHR`, `N` from gs_info_ahr call above
    full_join(y %>% select(-c(info, info0, theta)), by = "Analysis") %>%
    # select variables to be output
    select(c("Analysis", "Bound", "Time", "N", "Events", "Z", "Probability", "Probability0", "AHR", "theta", 
             "info", "info0", "info_frac", "~HR at bound", "Nominal p")) %>% 
    # arrange the output table
    arrange(Analysis, desc(Bound))
  
  inflac_fct <- (allout %>% filter(Analysis == K, Bound == "Upper"))$info / (y %>% filter(Analysis == K))$info
  allout$Events <- allout$Events * inflac_fct
  allout$N <- allout$N * inflac_fct
  
  # --------------------------------------------- #
  #     get bounds to output                      #
  # --------------------------------------------- #
  bounds <- allout %>% 
    select(all_of(c("Analysis", "Bound", "Probability", "Probability0", "Z", "~HR at bound", "Nominal p"))) %>% 
    arrange(Analysis, desc(Bound))
  # --------------------------------------------- #
  #     get analysis summary to output            #
  # --------------------------------------------- #
  analysis <- allout %>% 
    select(Analysis, Time, N, Events, AHR, theta, info, info0, info_frac) %>% 
    unique() %>% 
    arrange(Analysis)
  
  # --------------------------------------------- #
  #     return the output                         #
  # --------------------------------------------- #
  ans <- list(
    enroll_rate = enroll_rate %>% mutate(rate = rate * inflac_fct),
    fail_rate = fail_rate,
    bounds = bounds %>% filter(!is.infinite(Z)),
    analysis = analysis)
  
  class(ans) <- c("ahr", "gs_design", class(ans))
  if(!binding){
    class(ans) <- c("non-binding", class(ans))
  }
  
  return(ans)
  
}