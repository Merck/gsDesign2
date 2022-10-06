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
#' @param p_c rate at the control group
#' @param p_e rate at the experimental group 
#' @param IF statistical information fraction
#' @param rd0 treatment effect under super-superiority designs, the default is 0
#' @param alpha One-sided Type I error
#' @param beta Type II error
#' @param ratio Experimental:Control randomization ratio (not yet implemented)
#' @param stratum_prev randomization ratio of different stratum. 
#' If it is un-stratified design then \code{NULL}.
#' Otherwise it is a tibble containing two columns (Stratum and prevalence).
#' @param binding indicator of whether futility bound is binding; default of FALSE is recommended
#' @param upper Function to compute upper bound
#' @param upar Parameter passed to \code{upper()}
#' @param lower Function to compute lower bound
#' @param lpar Parameter passed to \code{lower()}
#' @param test_upper indicator of which analyses should include an upper (efficacy) bound; single value of TRUE (default) indicates all analyses;
#' otherwise, a logical vector of the same length as \code{info} should indicate which analyses will have an efficacy bound
#' @param test_lower indicator of which analyses should include an lower bound; single value of TRUE (default) indicates all analyses;
#' single value FALSE indicated no lower bound; otherwise, a logical vector of the same length as \code{info} should indicate which analyses will have a
#' lower bound
#' @param h1_spending Indicator that lower bound to be set by spending under alternate hypothesis (input \code{failRates})
#' if spending is used for lower bound
#' 
#' @param r  Integer, at least 2; default of 18 recommended by Jennison and Turnbull

#' 
#' @param info_scale the information scale for calculation
#' @param weight the weighting scheme for stratified population
#' @param tol Tolerance parameter for boundary convergence (on Z-scale)
#'
#' @return a \code{tibble} with columns Analysis, Bound, Z, Probability, theta, Time, AHR, Events
#' @details Need to be added
#' @export 
#'
#' @examples
#' library(tibble)
#' library(gsDesign)
#' 
#' # ----------------- #
#' #    example 1      #
#' #------------------ #
#' # un-stratified group sequential design
#' gs_design_rd(
#'   p_c = tibble(Stratum = "All", Rate = .2),
#'   p_e = tibble(Stratum = "All", Rate = .15),
#'   IF = c(0.7, 1),
#'   rd0 = 0, 
#'   alpha = .025,                  
#'   beta = .1,                    
#'   ratio = 1,
#'   stratum_prev = NULL,
#'   weight = "un-stratified",
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#'   )
#'   
#' # ----------------- #
#' #     example 2     #
#' # ----------------- #
#' # stratified group sequential design
#' gs_design_rd(
#'   p_c = tibble(Stratum = c("biomarker positive", "biomarker negative"), Rate = c(.2, .25)),
#'   p_e = tibble(Stratum = c("biomarker positive", "biomarker negative"), Rate = c(.15,.22)),
#'   IF = c(0.7, 1),
#'   rd0 = 0, 
#'   alpha = .025,                  
#'   beta = .1,                    
#'   ratio = 1,
#'   stratum_prev = tibble(Stratum = c("biomarker positive", "biomarker negative"), prevalence = c(.4, .6)),
#'   weight = "ss",
#'   upper = gs_spending_bound,lower = gs_b,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lpar = rep(-Inf, 2)
#' )
#' 
gs_design_rd <- function(
    p_c = tibble(Stratum = "All", Rate = .2),
    p_e = tibble(Stratum = "All", Rate = .15),
    IF = 1:3/3,
    rd0 = 0, 
    alpha = .025,                  
    beta = .1,                    
    ratio = 1,
    stratum_prev = NULL,
    weight = c("un-stratified", "ss", "invar"),
    upper = gs_b,
    lower = gs_b,
    upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
    lpar = c(qnorm(.1), rep(-Inf, 2)),
    test_upper = TRUE,
    test_lower = TRUE,
    info_scale = c(0, 1, 2),
    binding = FALSE,
    r = 18,
    tol = 1e-6,
    h1_spending = FALSE
){
  # --------------------------------------------- #
  #     check input values                        #
  # --------------------------------------------- #
  info_scale <- if(methods::missingArg(info_scale)){2}else{match.arg(as.character(info_scale), choices = 0:2)}
  weight <- if(methods::missingArg(weight)){"un-stratified"}else{match.arg(weight)}
  n_strata <- length(unique(p_c$Stratum))
  if(methods::missingArg(IF)){
    k <- 1
  }else{
    k <- length(IF)
  }
  
  # --------------------------------------------- #
  #     calculate the sample size                 #
  #          under fixed design                   #
  # --------------------------------------------- #
  x_fix <- gs_info_rd(
    p_c = p_c, 
    p_e = p_e,
    N = tibble(Analysis = 1, 
               Stratum = p_c$Stratum, 
               N = if(is.null(stratum_prev)){1}else{(stratum_prev %>% mutate(x = prevalence / sum(prevalence)))$x}), 
    rd0 = rd0,
    ratio = ratio,
    weight = weight) 
  
  # --------------------------------------------- #
  #     calculate the sample size                 #
  #     under group sequential design             #
  # --------------------------------------------- #
  x_gs <- gs_info_rd(
    p_c = p_c, 
    p_e = p_e,
    N = tibble(Analysis = rep(1:k, n_strata), 
               Stratum = rep(p_c$Stratum, each = k), 
               N = if(is.null(stratum_prev)){
                      IF
                   }else{
                     rep((stratum_prev %>% mutate(x = prevalence / sum(prevalence)))$x, each = k) * IF
                   }), 
    rd0 = rd0,
    ratio = ratio,
    weight = weight)
  
  if(k == 1){
    x <- x_fix
  }else{
    x <- x_gs
  }
  
  if(h1_spending){
    theta1 <- x$theta 
    info1 <- x$info
  }else{
    theta1 <- 0
    info1 <- x$info0
  }
  
  y_gs <- gs_design_npe(theta = x$rd, theta1 = theta1, 
                        info = x$info1, info0 = x$info0, info1 = info1, 
                        info_scale = info_scale,
                        alpha = alpha, beta = beta, binding = binding,
                        upper = upper, upar = upar, test_upper = test_upper,
                        lower = lower, lpar = lpar, test_lower = test_lower,
                        r = r, tol = tol)
 
  
  
  
  # --------------------------------------------- #
  #     get statistical information               #
  # --------------------------------------------- #
  allout <-  y_gs %>%
    mutate(rd = x_fix$rd,
           rd0 = rd0,
           "~Risk difference at bound" = Z / sqrt(info) / theta * (rd -rd0)  + rd0, 
           "Nominal p" = pnorm(-Z),
           IF0 = if(sum(!is.na(info0)) == 0){NA}else{info0 / max(info0)},
           N = (y_gs %>% filter(Bound == "Upper", Analysis == k))$info
               / ifelse(info_scale == 0, x_fix$info0[1], x_fix$info1[1])  * IF) %>% 
    select(c(Analysis, Bound,  N, rd, rd0, Z, Probability, Probability0, info, info0, IF, IF0, `~Risk difference at bound`, `Nominal p`)) %>% 
    arrange(Analysis, desc(Bound)) 
  
  # --------------------------------------------- #
  #     get bounds to output                      #
  # --------------------------------------------- #
  bounds <- allout %>%  
    select(Analysis, Bound, Probability, Probability0, Z, `~Risk difference at bound`, `Nominal p`)
  
  # --------------------------------------------- #
  #     get analysis summary to output            #
  # --------------------------------------------- #
  analysis <- allout %>% 
    filter(Bound == "Upper") %>% 
    select(Analysis, N, rd, rd0, info, info0, IF, IF0) 
  
  # --------------------------------------------- #
  #     return the output                         #
  # --------------------------------------------- #
  ans <- list(
    bounds = bounds,
    analysis = analysis)
  
  class(ans) <- c("rd", "gs_design", class(ans))
  
  return(ans)
}
