#  Copyright (c) 2021 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co., Inc., Kenilworth, NJ, USA.
#
#  This file is part of the gsdmvn program.
#
#  gsdmvn is free software: you can redistribute it and/or modify
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
#' @import tibble tibble
#' @inheritParams gs_design_ahr
#' @inheritParams gs_info_wlr
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input analysisTimes is a positive number or a positive increasing sequence.
#'    \item Validate if input IF is a positive number or positive increasing sequence on (0, 1] with final value of 1.
#'    \item Validate if inputs IF and analysisTimes  have the same length if both have length > 1.
#'    \item Compute information at input analysisTimes using \code{gs_info_wlr()}.
#'    \item Compute sample size and bounds using \code{gs_design_npe()}.
#'    \item Return a list of design enrollment, failure rates, and bounds.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @export
#' 
#' @examples
#' library(dplyr)
#' library(mvtnorm)
#' library(gsDesign)
#' library(tibble)
#' library(gsDesign2)
#' 
#' # set enrollment rates
#' enrollRates <- tibble(Stratum = "All", duration = 12, rate = 500/12)
#' 
#' # set failure rates
#' failRates <- tibble(
#'   Stratum = "All",
#'   duration = c(4, 100),
#'   failRate = log(2) / 15,  # median survival 15 month
#'   hr = c(1, .6),
#'   dropoutRate = 0.001)
#' 
#' # -------------------------#
#' #       example 1          #
#' # ------------------------ #
#' # Boundary is fixed 
#' x <- gsSurv(
#'   k = 3, 
#'   test.type = 4, 
#'   alpha = 0.025, beta = 0.2, 
#'   astar = 0, timing = 1,
#'   sfu = sfLDOF, sfupar = 0, 
#'   sfl = sfLDOF, sflpar = 0, 
#'   lambdaC = 0.1, 
#'   hr = 0.6, hr0 = 1, 
#'   eta = 0.01, gamma = 10,
#'   R = 12, S = NULL,
#'   T = 36, minfup = 24, 
#'   ratio = 1)
#' 
#' gs_design_wlr(
#'   enrollRates = enrollRates, 
#'   failRates = failRates,
#'   ratio = 1, 
#'   alpha = 0.025, beta = 0.2,
#'   weight = function(x, arm0, arm1){wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 0.5)},
#'   upper = gs_b,
#'   upar = x$upper$bound,
#'   lower = gs_b,
#'   lpar = x$lower$bound,
#'   analysisTimes = c(12, 24, 36))
#' 
#' # -------------------------#
#' #       example 2          #
#' # ------------------------ #
#' # Boundary derived by spending function
#' gs_design_wlr(
#'   enrollRates = enrollRates, 
#'   failRates = failRates,
#'   ratio = 1, 
#'   alpha = 0.025, beta = 0.2,
#'   weight = function(x, arm0, arm1){wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 0.5)},
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2),
#'   analysisTimes = c(12, 24, 36))
#'
gs_design_wlr <- function(enrollRates = tibble(Stratum = "All", duration = c(2, 2, 10), 
                                               rate = c(3, 6, 9)),
                          failRates = tibble(Stratum = "All", duration = c(3, 100),
                                             failRate = log(2)/c(9, 18), hr = c(.9, .6),
                                             dropoutRate = rep(.001, 2)),
                          weight = wlr_weight_fh, approx = "asymptotic",
                          alpha = 0.025, beta = 0.1, ratio = 1,           
                          IF = NULL, info_scale = c(0, 1, 2),     
                          analysisTimes = 36,    
                          binding = FALSE,
                          upper = gs_b,
                          upar = gsDesign(k = 3, test.type = 1, n.I = c(.25, .75, 1), sfu = sfLDOF, sfupar = NULL)$upper$bound,
                          lower = gs_b,
                          lpar = c(qnorm(.1), -Inf, -Inf), 
                          test_upper = TRUE,
                          test_lower = TRUE,
                          h1_spending = TRUE,
                          r = 18, tol = 1e-6
){
  # --------------------------------------------- #
  #     check input values                        #
  # --------------------------------------------- #
  msg <- "gs_design_wlr(): analysisTimes must be a positive number or positive increasing sequence"
  if (!is.vector(analysisTimes,mode = "numeric")) stop(msg)
  if (min(analysisTimes - dplyr::lag(analysisTimes, def = 0)) <= 0) stop(msg)
  msg <- "gs_design_wlr(): IF must be a positive number or positive increasing sequence on (0, 1] with final value of 1"
  if (is.null(IF)){IF <- 1}
  if (!is.vector(IF,mode = "numeric")) stop(msg)
  if (min(IF - dplyr::lag(IF, def = 0)) <= 0) stop(msg)
  if (max(IF) != 1) stop(msg)
  msg <- "gs_design_wlr(): IF and analysisTimes must have the same length if both have length > 1"
  if ((length(analysisTimes) > 1) & (length(IF) > 1) & (length(IF) != length(analysisTimes))) stop(msg)
  # get the info_scale
  info_scale <- if(methods::missingArg(info_scale)){2}else{match.arg(as.character(info_scale), choices = 0:2)}
  
  # --------------------------------------------- #
  #     get information at input analysisTimes    #
  # --------------------------------------------- #
  y <- gs_info_wlr(enrollRates, failRates, ratio = ratio, events = NULL, 
                   analysisTimes = analysisTimes, weight = weight, approx = approx)
  
  finalEvents <- y$Events[nrow(y)]
  IFalt <- y$Events / finalEvents
  
  # Check if IF needed for (any) IA timing
  K <- max(length(analysisTimes), length(IF))
  nextTime <- max(analysisTimes)
  
  if(length(IF) == 1){
    IF <- IFalt
  }else{
    IFindx <- IF[1 : (K-1)]
    for(i in seq_along(IFindx)){
      if(length(IFalt) == 1){
        y <- rbind(tEvents(enrollRates, failRates, 
                          targetEvents = IF[K - i] * finalEvents, 
                          ratio = ratio, interval = c(.01, nextTime)) %>% 
                     mutate(theta = -log(AHR), Analysis = K - i),
                   y)
      }else if(IF[K-i] > IFalt[K-i]){
        y[K - i,] <- tEvents(enrollRates, failRates, 
                             targetEvents = IF[K - i] * finalEvents,
                             ratio = ratio, interval = c(.01, nextTime)) %>%
          dplyr::transmute(Analysis = K - i, Time, Events, AHR, theta = -log(AHR), info, info0)
      } 
      nextTime <- y$Time[K - i]
    }
  }
  
  y$Analysis <- 1:K
  y$N <- eAccrual(x = y$Time, enrollRates = enrollRates)
  
  # h1 spending
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
    allout <- gs_design_npe(theta = y$theta, theta1 = theta1,
                            info = y$info, info0 = y$info0, info1 = info1, info_scale = info_scale,
                            alpha = alpha, beta = beta, binding = binding,
                            upper = upper, upar = upar, test_upper = test_upper,
                            lower = lower, lpar = lpar, test_lower = test_lower,
                            r = r, tol = tol) %>%
      full_join(y %>% select(-c(info, info0, theta)), by = "Analysis") %>%
      select(c("Analysis", "Bound", "Time", "N", "Events", "Z", 
               "Probability", "Probability0", "AHR", "theta", "info", "info0", "IF")) %>%  
      arrange(Analysis, desc(Bound))  
  )
  
  # calculate sample size & events
  inflac_fct <- (allout %>% filter(Analysis == K, Bound == "Upper"))$info / (y %>% filter(Analysis == K))$info
  allout$Events <- allout$Events * inflac_fct
  allout$N <- allout$N * inflac_fct
  
  # add `~HR at bound`, `HR generic` and `Nominal p`
  allout <- allout %>% mutate(
    "~HR at bound" = gsDesign::zn2hr(z = Z, n = Events, ratio = ratio),
    "Nominal p" = pnorm(-Z)
  ) 
  
  # --------------------------------------------- #
  #     return the output                         #
  # --------------------------------------------- #
  # bounds table
  bounds <- allout %>% 
    select(all_of(c("Analysis", "Bound", "Probability", "Probability0", "Z", "~HR at bound", "Nominal p" ))) %>%  
    arrange(Analysis, desc(Bound))  
  
  # analysis table
  analysis <- allout %>% 
    select(Analysis, Time, N, Events, AHR, theta, info, info0, IF) %>% 
    unique() %>%  
    arrange(Analysis)
  
  # final output
  ans <- list(
    enrollRates = enrollRates %>% mutate(rate = rate * inflac_fct),
    failRates = failRates,
    bounds = bounds,
    analysis = analysis)
  class(ans) <- c("wlr", "gs_design", class(ans))
  
  return(ans)
  
}