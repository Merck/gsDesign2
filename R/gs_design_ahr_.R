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

#' @importFrom tibble tibble
#' @importFrom gsDesign gsDesign sfLDOF
#' @importFrom stats qnorm
#' @importFrom dplyr mutate full_join select arrange desc
NULL
#' Group sequential design using average hazard ratio under non-proportional hazards
#'
#' @param enrollRates enrollment rates
#' @param failRates failure and dropout rates
#' @param ratio Experimental:Control randomization ratio (not yet implemented)
#' @param alpha One-sided Type I error
#' @param beta Type II error
#' @param IF Targeted information fraction at each analysis
#' @param analysisTimes Minimum time of analysis
#' @param binding indicator of whether futility bound is binding; default of FALSE is recommended
#' @param upper Function to compute upper bound
#' @param upar Parameter passed to \code{upper()}
#' @param lower Function to compute lower bound
#' @param lpar Parameter passed to \code{lower()}
#' @param h1_spending Indicator that lower bound to be set by spending under alternate hypothesis (input \code{failRates})
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
#'    \item Validate if input analysisTimes is a positive number or positive increasing sequence.
#'    \item Validate if input IF is a positive number or positive increasing sequence
#'    on (0, 1] with final value of 1.
#'    \item Validate if input IF and analysisTimes  have the same length if both have length > 1.
#'    \item Get information at input analysisTimes
#'    \itemize{
#'      \item Use \code{gs_info_ahr()} to get the information and effect size based on AHR approximation.
#'      \item Extract the final event.
#'      \item Check if input If needed for (any) interim analysis timing.
#'    }
#'    \item Add the analysis column to the information at input analysisTimes.
#'    \item Add the sample size column to the information at input analysisTimes using \code{eAccrual()}.
#'    \item Get sample size and bounds using \code{gs_design_npe()} and save them to bounds.
#'    \item Add Time, Events, AHR, N that have already been calculated to the bounds.
#'    \item Return a list of design enrollment, failure rates, and bounds.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @return a \code{tibble} with columns Analysis, Bound, Z, Probability, theta, Time, AHR, Events
#' @details Need to be added
#' @noRd
#'
#' @examples
#' library(gsDesign)
#' library(gsDesign2)
#' library(dplyr)
#' # call with defaults
#' gs_design_ahr()
#'
#' # Single analysis
#' gs_design_ahr(analysisTimes = 40)
#'
#' # Multiple analysisTimes
#' gs_design_ahr(analysisTimes = c(12,24,36))
#'
#' # Specified information fraction
#' gs_design_ahr(IF = c(.25,.75,1), analysisTimes = 36)
#'
#' # multiple analysis times & IF
#' # driven by times
#' gs_design_ahr(IF = c(.25,.75,1), analysisTimes = c(12,25,36))
#' # driven by IF
#' gs_design_ahr(IF = c(1/3, .8, 1), analysisTimes = c(12,25,36))
#'
#' # 2-sided symmetric design with O'Brien-Fleming spending
#' gs_design_ahr(analysisTimes = c(12, 24, 36),
#'               binding = TRUE,
#'               upper = gs_spending_bound,
#'               upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, 
#'                           param = NULL, timing = NULL),
#'               lower = gs_spending_bound,
#'               lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, 
#'                           param = NULL, timing = NULL),
#'               h1_spending = FALSE)
#'
#' # 2-sided asymmetric design with O'Brien-Fleming upper spending
#' # Pocock lower spending under H1 (NPH)
#' gs_design_ahr(analysisTimes = c(12, 24, 36),
#'               binding = TRUE,
#'               upper = gs_spending_bound,
#'               upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, 
#'                           param = NULL, timing = NULL),
#'               lower = gs_spending_bound,
#'               lpar = list(sf = gsDesign::sfLDPocock, total_spend = 0.1, 
#'                           param = NULL, timing = NULL),
#'               h1_spending = TRUE)
gs_design_ahr_ <- function(enrollRates=tibble::tibble(Stratum="All",
                                                     duration=c(2,2,10),
                                                     rate=c(3,6,9)),
                          failRates=tibble::tibble(Stratum="All",
                                                   duration=c(3,100),
                                                   failRate=log(2)/c(9,18),
                                                   hr=c(.9,.6),
                                                   dropoutRate=rep(.001,2)),
                          ratio=1,               # Experimental:Control randomization ratio
                          alpha = 0.025,         # One-sided Type I error
                          beta = 0.1,            # NULL if enrollment is not adapted
                          IF = NULL,             # relative information fraction timing (vector, if not NULL; increasing to 1)
                          analysisTimes = 36,    # Targeted times of analysis or just planned study duration
                          binding = FALSE,
                          upper = gs_b,
                          # Default is Lan-DeMets approximation of
                          upar = gsDesign(k=3, test.type=1,
                                          n.I=c(.25, .75, 1),
                                          sfu=sfLDOF, sfupar = NULL)$upper$bound,
                          lower = gs_b,
                          lpar = c(qnorm(.1), -Inf, -Inf), # Futility only at IA1
                          h1_spending = TRUE,
                          test_upper = TRUE,
                          test_lower = TRUE,
                          r = 18,
                          tol = 1e-6
){
  ################################################################################
  # Check input values
  msg <- "analysisTimes must be a positive number or positive increasing sequence"
  if (!is.vector(analysisTimes,mode = "numeric")) stop(msg)
  if (min(analysisTimes - dplyr::lag(analysisTimes, def=0))<=0) stop(msg)
  msg <- "gs_design_ahr(): IF must be a positive number or positive increasing sequence on (0, 1] with final value of 1"
  if (is.null(IF)){IF <- 1}
  if (!is.vector(IF,mode = "numeric")) stop(msg)
  if (min(IF - dplyr::lag(IF, def=0))<=0) stop(msg)
  if (max(IF) != 1) stop(msg)
  msg <- "gs_design_ahr() IF and analysisTimes must have the same length if both have length > 1"
  if ((length(analysisTimes)>1) & (length(IF) > 1) & (length(IF) != length(analysisTimes))) stop(msg)
  # end check input values
  ################################################################################
  # Get information at input analysisTimes
  y <- gs_info_ahr(enrollRates, failRates, ratio = ratio, events = NULL, analysisTimes=analysisTimes)
  finalEvents <- y$Events[nrow(y)]
  IFalt <- y$Events / finalEvents
  # Check if IF needed for (any) IA timing
  K <- max(length(analysisTimes), length(IF))
  nextTime <- max(analysisTimes)
  if(length(IF)==1){IF <- IFalt}else{
    IFindx <- IF[1:(K-1)]
    for(i in seq_along(IFindx)){
      if(length(IFalt) == 1){y <-
        rbind(tEvents(enrollRates, failRates, targetEvents = IF[K - i] * finalEvents, ratio = ratio,
                      interval = c(.01, nextTime)) %>% mutate(theta=-log(AHR), Analysis=K-i),
              y)
      }else if (IF[K-i] > IFalt[K-i]) y[K - i,] <-
          tEvents(enrollRates, failRates, targetEvents = IF[K - i] * finalEvents, ratio = ratio,
                  interval = c(.01, nextTime)) %>%
          dplyr::transmute(Analysis = K - i, Time, Events, AHR, theta=-log(AHR), info, info0)
      nextTime <- y$Time[K - i]
    }
  }
  y$Analysis <- 1:K
  y$N <- eAccrual(x = y$Time, enrollRates = enrollRates)
  if(h1_spending){
    theta1 <- y$theta
    info1 <- y$info
  }else{
    theta1 <- 0
    info1 <- y$info0
  }
  
  # Get sample size and bounds using gs_design_npe
  bounds <- gs_design_npe_(theta = y$theta,
                          theta1 = theta1,
                          info = y$info,
                          info0 = y$info0,
                          info1 = info1,
                          alpha = alpha,
                          beta = beta,
                          binding = binding,
                          upper = upper,
                          lower = lower,
                          upar = upar,
                          lpar = lpar,
                          test_upper = test_upper,
                          test_lower = test_lower,
                          r = r,
                          tol = tol) %>%
    # Add Time, Events, AHR, N from gs_info_ahr call above
    full_join(y %>% select(-c(info,info0,theta)), by = "Analysis") %>%
    select(c("Analysis", "Bound", "Time", "N", "Events", "Z", "Probability", "AHR", "theta", "info", "info0")) %>%
    arrange(desc(Bound), Analysis)
  bounds$Events <- bounds$Events * bounds$info[K] / y$info[K]
  bounds$N <- bounds$N * bounds$info[K] / y$info[K]
  
  # Document design enrollment, failure rates, and bounds
  return(list(enrollRates = enrollRates %>%
                mutate(rate = rate * bounds$info[K] / y$info[K]),
              failRates = failRates,
              bounds = bounds)
  )
}