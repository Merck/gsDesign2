#  Copyright (c) 2021 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co., Inc., Kenilworth, NJ, USA.
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

#' Group sequential design power using weighted log rank test under non-proportional hazards
#'
#' @importFrom tibble tibble
#' @importFrom gsDesign gsDesign
#' @importFrom dplyr left_join
#' 
#' @inheritParams gs_design_wlr
#' @inheritParams gs_power_ahr
#' 
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Compute information and effect size for Weighted Log-rank test using \code{gs_info_wlr()}.
#'    \item Compute group sequential bound computation with non-constant effect using \code{gs_power_npe()}.
#'    \item Combine information and effect size and power and return a
#'    tibble  with columns Analysis, Bound, Time, Events, Z, Probability, AHR,  theta, info, and info0.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @export
#' 
#' @examples 
#' library(tibble)
#' library(gsDesign)
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
#' # set the targeted number of events and analysis time
#' target_events <- c(30, 40, 50)
#' target_analysisTime <- c(10, 24, 30)
#' 
#' # -------------------------#
#' #       example 1          #
#' # ------------------------ #
#' # fixed bounds and calculate the power for targeted number of events
#' gs_power_wlr(
#'   enrollRates = enrollRates,
#'   failRates = failRates,
#'   events = target_events,
#'   analysisTimes = NULL,
#'   upper = gs_b,
#'   upar = gsDesign(k = length(target_events), test.type = 1, n.I = target_events, maxn.IPlan = max(target_events), sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lower = gs_b,
#'   lpar = c(qnorm(.1), rep(-Inf, 2)))
#'   
#' # -------------------------#
#' #       example 2          #
#' # ------------------------ #
#' # fixed bounds and calculate the power for targeted analysis time
#' gs_power_wlr(
#'   enrollRates = enrollRates,
#'   failRates = failRates,
#'   events = NULL,
#'   analysisTimes = target_analysisTime,
#'   upper = gs_b,
#'   upar = gsDesign(k = length(target_events), test.type = 1, n.I = target_events, maxn.IPlan = max(target_events), sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lower = gs_b,
#'   lpar = c(qnorm(.1), rep(-Inf, 2)))
#'
#' # -------------------------#
#' #       example 3          #
#' # ------------------------ #
#' # fixed bounds and calculate the power for targeted analysis time & number of events
#' gs_power_wlr(
#'   enrollRates = enrollRates,
#'   failRates = failRates,
#'   events = target_events,
#'   analysisTimes = target_analysisTime,
#'   upper = gs_b,
#'   upar = gsDesign(k = length(target_events), test.type = 1, n.I = target_events, maxn.IPlan = max(target_events), sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lower = gs_b,
#'   lpar = c(qnorm(.1), rep(-Inf, 2)))
#'   
#' # -------------------------#
#' #       example 4          #
#' # ------------------------ #
#' # spending bounds and calculate the power for targeted number of events
#' gs_power_wlr(
#'   enrollRates = enrollRates,
#'   failRates = failRates,
#'   events = target_events,
#'   analysisTimes = NULL,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2))
#'   
#' # -------------------------#
#' #       example 5          #
#' # ------------------------ #
#' # spending bounds and calculate the power for targeted analysis time
#' gs_power_wlr(
#'   enrollRates = enrollRates,
#'   failRates = failRates,
#'   events = NULL,
#'   analysisTimes = target_analysisTime,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2))
#'   
#' # -------------------------#
#' #       example 6          #
#' # ------------------------ #
#' # spending bounds and calculate the power for targeted analysis time & number of events
#' gs_power_wlr(
#'   enrollRates = enrollRates,
#'   failRates = failRates,
#'   events = target_events,
#'   analysisTimes = target_analysisTime,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2))
#'   
gs_power_wlr <- function(enrollRates = tibble(Stratum = "All", duration = c(2, 2, 10), rate = c(3, 6, 9)),
                         failRates = tibble(Stratum = "All", duration = c(3, 100), failRate = log(2)/c(9, 18),
                                            hr = c(.9, .6), dropoutRate = rep(.001, 2)),
                         events = c(30, 40, 50), 
                         analysisTimes = NULL, 
                         binding = FALSE,
                         upper = gs_b, 
                         lower = gs_b,               
                         upar = gsDesign(k = 3, test.type = 1, n.I = c(30, 40, 50), maxn.IPlan = 50, sfu = sfLDOF, sfupar = NULL)$upper$bound,
                         lpar = c(qnorm(.1), rep(-Inf, 2)), 
                         test_upper = TRUE, 
                         test_lower = TRUE,
                         ratio = 1, 
                         weight = wlr_weight_fh, 
                         info_scale = c(0, 1, 2), 
                         approx = "asymptotic", 
                         r = 18, 
                         tol = 1e-6){
  # get the number of analysis
  K <- max(length(events), length(analysisTimes), na.rm = TRUE)
  # get the info_scale
  info_scale <- if(methods::missingArg(info_scale)){2}else{match.arg(as.character(info_scale), choices = 0:2)}
  
  # ---------------------------------------- #
  #    calculate the asymptotic variance     #
  #       and statistical information        #
  # ---------------------------------------- #
  x <- gs_info_wlr(
    enrollRates = enrollRates,
    failRates = failRates,
    ratio = ratio,
    events = events,
    weight = weight,
    analysisTimes = analysisTimes
  )
  
  # ---------------------------------------- #
  #  given the above statistical information #
  #         calculate the power              #
  # ---------------------------------------- #
  y_H1 <- gs_power_npe(
    theta = x$theta, 
    info = x$info, 
    info0 = x$info0,
    info_scale = info_scale,
    binding = binding,
    upper = upper, 
    lower = lower, 
    upar = upar, 
    lpar= lpar,
    test_upper = test_upper, 
    test_lower = test_lower,
    r = r, 
    tol = tol)
  
  y_H0 <- gs_power_npe(
    theta = 0, #x$theta, 
    info = x$info0, 
    info0 = x$info0,
    info_scale = info_scale,
    binding = binding,
    upper = upper, 
    lower = lower, 
    upar = upar, 
    lpar= lpar,
    test_upper = test_upper, 
    test_lower = test_lower,
    r = r, 
    tol = tol)
  
  # ---------------------------------------- #
  #         organize the outputs             #
  # ---------------------------------------- #
  # summarize the bounds
  suppressMessages(
    bounds <- y_H0 %>%
      select(Analysis, Bound, Z, Probability) %>% 
      dplyr::rename(Probability0 = Probability) %>%
      dplyr::left_join(x %>% select(Analysis, Events)) %>% 
      mutate(`~HR at bound` = gsDesign::zn2hr(z = Z, n = Events, ratio = ratio), `Nominal p` = pnorm(-Z)) %>% 
      dplyr::left_join(y_H1 %>% select(Analysis, Bound, Probability)) %>% 
      select(Analysis, Bound, Probability, Probability0, Z, `~HR at bound`, `Nominal p`) %>% 
      arrange(Analysis, desc(Bound))
  )
  
  # summarize the analysis
  suppressMessages(
    analysis <- x %>% 
      select(Analysis, Time, Events, AHR) %>% 
      mutate(N = eAccrual(x = x$Time, enrollRates = enrollRates)) %>% 
      dplyr::left_join(y_H1 %>% select(Analysis, info, IF, theta) %>% unique()) %>%
      dplyr::left_join(y_H0 %>% select(Analysis, info, IF) %>% dplyr::rename(info0 = info, IF0 = IF) %>% unique()) %>%
      select(Analysis, Time, N, Events, AHR, theta, info, info0, IF, IF0) %>% 
      arrange(Analysis)
  )
  
  ans <- list(
    enrollRates = enrollRates, 
    failRates = failRates,
    bounds = bounds,
    analysis = analysis)
  
  class(ans) <- c("wlr", "gs_design", class(ans))
  
  return(ans)
}
