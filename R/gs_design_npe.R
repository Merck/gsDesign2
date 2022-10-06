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

#' @import tibble tibble
#' @importFrom stats qnorm uniroot
#' @import Rcpp
NULL

#' Group sequential design computation with non-constant effect and information
#'
#' \code{gs_design_npe()} derives group sequential design size, bounds and boundary crossing probabilities based on proportionate
#' information and effect size at analyses.
#' It allows a non-constant treatment effect over time, but also can be applied for the usual homogeneous effect size designs.
#' It requires treatment effect and proportionate statistical information at each analysis as well as a method of deriving bounds, such as spending.
#' The routine enables two things not available in the gsDesign package: 1) non-constant effect, 2) more flexibility in boundary selection.
#' For many applications, the non-proportional-hazards design function \code{gs_design_nph()} will be used; it calls this function.
#' Initial bound types supported are 1) spending bounds, 2) fixed bounds, and 3) Haybittle-Peto-like bounds.
#' The requirement is to have a boundary update method that can each bound without knowledge of future bounds.
#' As an example, bounds based on conditional power that require knowledge of all future bounds are not supported by this routine;
#' a more limited conditional power method will be demonstrated.
#' Boundary family designs Wang-Tsiatis designs including the original (non-spending-function-based) O'Brien-Fleming and Pocock designs
#' are not supported by \code{gs_power_npe()}.
#' @param theta natural parameter for group sequential design representing expected incremental drift at all analyses;
#' used for power calculation
#' @param theta0 natural parameter used for upper bound spending; if \code{NULL}, this will be set to 0
#' @param theta1 natural parameter used for lower bound spending; if \code{NULL}, this will be set to \code{theta}
#' which yields the usual beta-spending. If set to 0, spending is 2-sided under null hypothesis.
#' @param info proportionate statistical information at all analyses for input \code{theta}
#' @param info0 proportionate statistical information under null hypothesis, if different than alternative;
#' impacts null hypothesis bound calculation
#' @param info1 proportionate statistical information under alternate hypothesis;
#' impacts null hypothesis bound calculation
#' @param info_scale the information scale for calculation
#' @param alpha One-sided Type I error
#' @param beta Type II error
#' @param binding indicator of whether futility bound is binding; default of FALSE is recommended
#' @param upper function to compute upper bound
#' @param lower function to compare lower bound
#' @param upar parameter to pass to function provided in \code{upper}
#' @param lpar Parameter passed to function provided in \code{lower}
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
#'    \item Validate if input info is a numeric vector  or NULL, if non-NULL validate if it
#'    is strictly increasing and positive.
#'    \item Validate if input info0 is a numeric vector or NULL, if non-NULL validate if it
#'     is strictly increasing and positive.
#'    \item Validate if input info1 is a numeric vector or NULL, if non-NULL validate if it
#'    is strictly increasing and positive.
#'    \item Validate if input theta is a real vector and has the same length as info.
#'    \item Validate if input theta1 is a real vector and has the same length as info.
#'    \item Validate if input test_upper and test_lower are logical and have the same length as info.
#'    \item Validate if input test_upper value is TRUE.
#'    \item Validate if input alpha and beta are positive and of length one.
#'    \item Validate if input alpha and beta are from the unit interval and alpha is smaller than beta.
#'    \item Initialize bounds, numerical integration grids, boundary crossing probabilities.
#'    \item Compute fixed sample size for desired power and Type I error.
#'    \item Find an interval for information inflation to give correct power using \code{gs_power_npe()}.

#'    \item
#'    \item If there is no interim analysis, return a tibble including Analysis time, upper bound, Z-value,
#'    Probability of crossing bound, theta, info0 and info1.
#'    \item If the design is a group sequential design, return a tibble of Analysis,
#'     Bound, Z, Probability,  theta, info, info0.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @return a \code{tibble} with columns Analysis, Bound, Z, Probability,  theta, info, info0
#' @details The inputs \code{info} and \code{info0} should be vectors of the same length with increasing positive numbers.
#' The design returned will change these by some constant scale factor to ensure the design has power \code{1 - beta}.
#' The bound specifications in \code{upper, lower, upar, lpar} will be used to ensure Type I error and other boundary properties are as specified.
#' 
#' @author Keaven Anderson \email{keaven_anderson@@merck.com}
#'
#' @export
#' 
#' @examples
#' library(dplyr)
#' library(gsDesign)
#' 
#' # ---------------------------------# 
#' #         example 1                #
#' # ---------------------------------# 
#' # Single analysis
#' # Lachin book p 71 difference of proportions example
#' pc <- .28            # Control response rate
#' pe <- .40            # Experimental response rate
#' p0 <- (pc + pe) / 2  # Ave response rate under H0
#' 
#' # Information per increment of 1 in sample size
#' info0 <- 1 / (p0 * (1 - p0) * 4)
#' info <- 1 / (pc * (1 - pc) * 2 + pe * (1 - pe) * 2)
#' 
#' # Result should round up to next even number = 652
#' # Divide information needed under H1 by information per patient added
#' gs_design_npe(theta = pe - pc, info = info, info0 = info0)
#' 
#' 
#' # ---------------------------------# 
#' #         example 2                #
#' # ---------------------------------# 
#' # Fixed bound
#' x <- gs_design_npe(
#'   theta = c(.1, .2, .3),
#'   info = (1:3) * 80,
#'   info0 = (1:3) * 80,
#'   upper = gs_b,
#'   upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
#'   lower = gs_b,
#'   lpar = c(-1, 0, 0))
#' x
#' 
#' # Same upper bound; this represents non-binding Type I error and will total 0.025
#' gs_power_npe(
#'   theta = rep(0, 3),
#'   info = (x %>% filter(Bound == "Upper"))$info,
#'   upper = gs_b,
#'   upar = (x %>% filter(Bound == "Upper"))$Z,
#'   lower = gs_b,
#'   lpar = rep(-Inf, 3))
#' 
#' # ---------------------------------# 
#' #         example 3                #
#' # ---------------------------------# 
#' # Spending bound examples
#' # Design with futility only at analysis 1; efficacy only at analyses 2, 3
#' # Spending bound for efficacy; fixed bound for futility
#' # NOTE: test_upper and test_lower DO NOT WORK with gs_b; must explicitly make bounds infinite
#' # test_upper and test_lower DO WORK with gs_spending_bound
#' gs_design_npe(
#'   theta = c(.1, .2, .3),
#'   info = (1:3) * 40,
#'   info0 = (1:3) * 40,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lower = gs_b,
#'   lpar = c(-1, -Inf, -Inf),
#'   test_upper = c(FALSE, TRUE, TRUE))
#'   
#' # one can try `info_scale = 1` or `info_scale = 0` here
#' gs_design_npe(
#'   theta = c(.1, .2, .3),
#'   info = (1:3) * 40,
#'   info0 = (1:3) * 30,
#'   info_scale = 1,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lower = gs_b,
#'   lpar = c(-1, -Inf, -Inf),
#'   test_upper = c(FALSE, TRUE, TRUE))
#' 
#' # ---------------------------------# 
#' #         example 4                #
#' # ---------------------------------# 
#' # Spending function bounds
#' # 2-sided asymmetric bounds
#' # Lower spending based on non-zero effect
#' gs_design_npe(
#'   theta = c(.1, .2, .3),
#'   info = (1:3) * 40,
#'   info0 = (1:3) * 30,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL))
#' 
#' # ---------------------------------# 
#' #         example 5                #
#' # ---------------------------------# 
#' # Two-sided symmetric spend, O'Brien-Fleming spending
#' # Typically, 2-sided bounds are binding
#' xx <- gs_design_npe(
#'   theta = c(.1, .2, .3),
#'   info = (1:3) * 40,
#'   binding = TRUE,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL))
#' xx
#' 
#' # Re-use these bounds under alternate hypothesis
#' # Always use binding = TRUE for power calculations
#' gs_power_npe(
#'   theta = c(.1, .2, .3),
#'   info = (1:3) * 40,
#'   binding = TRUE,
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = (xx %>% filter(Bound == "Upper"))$Z,
#'   lpar = -(xx %>% filter(Bound == "Upper"))$Z)
#'   
gs_design_npe <- function(theta = .1, theta0 = NULL, theta1 = NULL,    # 3 theta
                          info = 1, info0 = NULL, info1 = NULL,        # 3 info
                          info_scale = c(0, 1, 2),
                          alpha = 0.025, beta = .1, 
                          upper = gs_b, upar = qnorm(.975),
                          lower = gs_b, lpar = -Inf,
                          test_upper = TRUE, test_lower = TRUE, binding = FALSE,
                          r = 18, tol = 1e-6){
  
  # --------------------------------------------- #
  #     check & set up parameters                 #
  # --------------------------------------------- #
  K <- length(info)
  
  # check alpha & beta
  check_alpha_beta(alpha, beta)
  
  # check theta, theta0, theta1
  if(length(theta) == 1){
    theta <- rep(theta, K)
  }
  
  if(is.null(theta1)){
    theta1 <- theta
  }else if(length(theta1) == 1){
    theta1 <- rep(theta1, K)
  }
  
  if(is.null(theta0)){
    theta0 <- rep(0, K)
  }else if(length(theta0) == 1){
    theta0 <- rep(theta0, K)
  }
  
  check_theta(theta, K)
  check_theta(theta0, K)
  check_theta(theta1, K)
  
  # check test_upper & test_lower
  if (length(test_upper) == 1 && K > 1) test_upper <- rep(test_upper, K)
  if (length(test_lower) == 1 && K > 1) test_lower <- rep(test_lower, K)
  check_test_upper(test_upper, K)
  check_test_lower(test_lower, K)
  
  # --------------------------------------------- #
  #     set up info                               #
  # --------------------------------------------- #
  if(is.null(info0)){
    info0 <- info
  }
  
  if(is.null(info1)){
    info1 <- info
  }
  
  # set up info_scale
  info_scale <- if(methods::missingArg(info_scale)){2}else{match.arg(as.character(info_scale), choices = 0:2)}
  if(info_scale == 0){
    info <- info0
    info1 <- info0
  }
  if(info_scale == 1){
    info <- info1
    info0 <- info1
  }
  
  # check info, info0, info1
  check_info(info)
  check_info(info0)
  check_info(info1)
  if(length(info0) != length(info)) stop("gs_design_npe(): length of info, info0 must be the same!")
  if(length(info1) != length(info)) stop("gs_design_npe(): length of info, info1 must be the same!")
  
  # --------------------------------------------- #
  #     check design type                         #
  # --------------------------------------------- #
  if(identical(lower, gs_b) & (!is.list(lpar))){
    two_sided <- ifelse(identical(lpar, rep(-Inf, K)), FALSE, TRUE)
  }else{
    two_sided <- TRUE
  }
  
  # --------------------------------------------- #
  #     initialization                            #
  # --------------------------------------------- #
  a <- rep(-Inf, K)          # bounds
  b <- rep(Inf, K)
  hgm1_0 <- NULL             # numerical integration grids
  hgm1_1 <- NULL
  upperProb <- rep(NA, K)    # boundary crossing probabilities
  lowerProb <- rep(NA, K)
  
  # --------------------------------------------- #
  #     fixed design                              #
  # --------------------------------------------- #
  # compute fixed sample size for desired power and Type I error.
  min_x <- ((qnorm(alpha) / sqrt(info0[K]) + qnorm(beta) / sqrt(info[K])) / theta[K])^2
  # for a fixed design, this is all you need.
  if (K == 1){
    ans <- tibble(Analysis = 1, Bound = "Upper", Z = qnorm(1 - alpha),
                  Probability = 1 - beta, Probability0 = alpha, theta = theta, 
                  info = info * min_x, info0 = info0 * min_x, info1 = info1 * min_x,
                  IF = info / max(info))
    return(ans)
  } 
  
  # --------------------------------------------- #
  #     search for the inflation factor to info   #
  # --------------------------------------------- #
  # ensure `min_x` gives power < 1 - beta 
  # and `max_x` gives power > 1 - beta
  min_temp <- gs_power_npe(theta = theta, theta0 = theta0, theta1 = theta1,
                           info = info * min_x, info0 = info0 * min_x, info1 = info * min_x, info_scale = info_scale,
                           upper = upper, upar = upar, test_upper = test_upper,
                           lower = lower, lpar = lpar, test_lower = test_lower,
                           binding = binding, r = r, tol = tol) 
  min_power <- (min_temp[min_temp$Bound == "Upper" & min_temp$Analysis == K, ])$Probability
  
  # a flag indicates if max_x can be found
  flag <- FALSE                   
  if (min_power < 1 - beta){
    # if min_power < 1 - beta
    # then find a max_power > 1 - beta 
    # by increasing `min_x` to `max_x` until `max_power` > 1 - beta
    max_x <- 1.05 * min_x           
    
    for(i in 1:10){
      max_temp <- gs_power_npe(theta = theta, theta0 = theta0, theta1 = theta1,
                               info = info * max_x, info0 = info0 * max_x, info1 = info * max_x, info_scale = info_scale,
                               upper = upper, upar = upar, test_upper = test_upper, 
                               lower = lower, lpar = lpar, test_lower = test_lower,
                               binding = binding, r = r, tol = tol) 
      max_power <- (max_temp[max_temp$Bound == "Upper" & max_temp$Analysis == K, ])$Probability
    
      if (max_power < 1 - beta){
        min_x <- max_x
        max_x <- 1.05 * max_x
      }else{
        flag <- TRUE
        break
      }
    }
    if(!flag) stop("gs_design_npe: could not inflate information to bracket power before root finding!")
  }else{
    # if min_power  > 1 - beta
    # then find a micro_power < 1 - beta 
    # by decreasing `min_x` to `micro_x` until `micro_power` < 1 - beta
    micro_x <- min_x / 1.05
    
    for(i in 1:10){
      micro_temp <- gs_power_npe(theta = theta, theta0 = theta0, theta1 = theta1,
                                 info = info * micro_x, info0 = info0 * micro_x, info1 = info * micro_x, info_scale = info_scale,
                                 upper = upper, upar = upar, test_upper = test_upper, 
                                 lower = lower, lpar = lpar, test_lower = test_lower,
                                 binding = binding, r = r, tol = tol)
      micro_power <- (micro_temp[micro_temp$Bound == "Upper" & micro_temp$Analysis == K, ])$Probability
      
      if(micro_power > 1 - beta){
        min_x <- micro_x
        micro_x <- micro_x / 1.05
      }else{
        flag <- TRUE
        break
      }
    }
    
    if(!flag) stop("gs_design_npe: could not deflate information to bracket targeted power before root finding!")
    max_x <- min_x
    min_x <- micro_x
  }
  
  # use root finding with the above function to find needed sample size inflation
  # now we can solve for the inflation factor for the enrollment rate to achieve the desired power
  res <- try(uniroot(errbeta, 
                     lower = min_x, upper = max_x,
                     theta = theta, theta0 = theta0, theta1 = theta1, 
                     info = info, info0 = info0, info1 = info1, info_scale = info_scale,
                     Zupper = upper, upar = upar, test_upper = test_upper,
                     Zlower = lower, lpar = lpar, test_lower = test_lower,
                     beta = beta, K = K, binding = binding, r = r, tol = tol))
  if(inherits(res, "try-error")){
    stop("gs_design_npe(): Sample size solution not found!")
  }else{
    inflation_factor <- res$root
  }
  
  # --------------------------------------------- #
  #     return the output                         #
  # --------------------------------------------- #
  # calculate the probability under H1
  ans_H1 <- gs_power_npe(theta = theta, theta0 = theta0, theta1 = theta1,
                         info = info * inflation_factor, info0 = info0 * inflation_factor, info1 = info1 * inflation_factor, 
                         info_scale = info_scale,
                         upper = upper, upar = upar, 
                         lower = lower, lpar = lpar, 
                         test_upper = test_upper, test_lower = test_lower,
                         binding = binding, r = r, tol = tol)
  
  # calculate the probability under H0
  ans_H0 <- gs_power_npe(theta = 0, theta0 = theta0, theta1 = theta1, 
                         info = info * inflation_factor, info0 = info0 * inflation_factor, info1 = info1 * inflation_factor,
                         info_scale = info_scale,
                         upper = upper, upar = upar, 
                         lower = if(!two_sided){gs_b}else{lower}, 
                         lpar = if(!two_sided){rep(-Inf, K)}else{lpar},
                         test_upper = test_upper, test_lower = test_lower,
                         binding = binding, r = r, tol = tol)
  
  # combine probability under H0 and H1
  suppressMessages(
    ans <- ans_H1 %>% full_join(ans_H0 %>% select(Analysis, Bound, Probability) %>% dplyr::rename(Probability0 = Probability))
  )
  
  ans <- ans %>% select(Analysis, Bound, Z, Probability, Probability0, theta, IF, info, info0, info1) 
  
  ans <- ans %>% arrange(Analysis)
  
  return(ans)
}


## Create a function that uses gs_power_npe to compute difference from targeted power
## for a given sample size inflation factor
errbeta <- function(x = 1, K = 1, 
                    beta = .1, 
                    theta = .1, theta0 = 0, theta1 = .1,
                    info = 1, info0 = 1, info1 = 1, info_scale = 2,
                    Zupper = gs_b, upar = qnorm(.975), 
                    Zlower = gs_b, lpar = -Inf, 
                    test_upper = TRUE, test_lower = TRUE,
                    binding = FALSE, r = 18, tol = 1e-6){
  
  x_temp <- gs_power_npe(theta = theta,  theta0 = theta0, theta1 = theta1,
                         info = info * x, info0 = info0 * x, info1 = info1 * x, info_scale = info_scale,
                         upper = Zupper, upar = upar, test_upper = test_upper, 
                         lower = Zlower, lpar = lpar, test_lower = test_lower,
                         binding = binding, r = r, tol = tol)
  
  x_power <- (x_temp[x_temp$Bound == "Upper" & x_temp$Analysis == K, ])$Probability
  
  ans <- 1 - beta - x_power 
  return(ans) 
}
