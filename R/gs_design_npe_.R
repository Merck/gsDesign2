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
#' @importFrom stats qnorm uniroot
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
#' @param theta1 natural parameter used for lower bound spending; if \code{NULL}, this will be set to \code{theta}
#' which yields the usual beta-spending. If set to 0, spending is 2-sided under null hypothesis.
#' @param info proportionate statistical information at all analyses for input \code{theta}
#' @param info0 proportionate statistical information under null hypothesis, if different than alternative;
#' impacts null hypothesis bound calculation
#' @param info1 proportionate statistical information under alternate hypothesis;
#' impacts null hypothesis bound calculation
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
#'    \item If the desing is a group sequential design, return a tibble of Analysis,
#'     Bound, Z, Probability,  theta, info, info0.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @return a \code{tibble} with columns Analysis, Bound, Z, Probability,  theta, info, info0
#' @details The inputs \code{info} and \code{info0} should be vectors of the same length with increasing positive numbers.
#' The design returned will change these by some constant scale factor to ensure the design has power \code{1 - beta}.
#' The bound specifications in \code{upper, lower, upar, lpar} will be used to ensure Type I error and other boundary properties are as specified.
#' @author Keaven Anderson \email{keaven_anderson@@merck.com}
#'
#' @noRd
#'
#' @examples
#'
#' library(gsDesign)
#' library(gsDesign2)
#' library(dplyr)
#' 
#' # Single analysis
#' # Lachin book p 71 difference of proportions example
#' pc <- .28 # Control response rate
#' pe <- .40 # Experimental response rate
#' p0 <- (pc + pe) / 2 # Ave response rate under H0
#' # Information per increment of 1 in sample size
#' info0 <- 1 / (p0 * (1 - p0) * 4)
#' info1 <- 1 / (pc * (1 - pc) * 2 + pe * (1 - pe) * 2)
#' # Result should round up to next even number = 652
#' # Divide information needed under H1 by information per patient added
#' gsDesign2:::gs_design_npe_(theta = pe - pc, info = info1, info0 = info0)$info[1] / info1
#'
#' # Fixed bound
#' design <- gsDesign2:::gs_design_npe_(theta = c(.1, .2, .3), info = (1:3) * 80, 
#'               info0 = (1:3) * 80, info1 = (1:3) * 80,
#'               upper = gs_b, upar = gsDesign::gsDesign(k=3,sfu=gsDesign::sfLDOF)$upper$bound,
#'               lower = gs_b, lpar = c(-1, 0, 0))
#' design
#'
#' # Same fixed bounds, null hypothesis
#' gsDesign2:::gs_design_npe_(theta = rep(0,3), info = design$info0[1:3], 
#'             upar = design$Z[1:3], lpar = design$Z[4:6])
#'
#' # Same upper bound; this represents non-binding Type I error and will total 0.025
#' gsDesign2:::gs_design_npe_(theta = rep(0,3), info = design$info0[1:3], 
#'              upar = design$Z[1:3], lpar = rep(-Inf,3)) %>% 
#'   filter(Bound=="Upper")
#'
#' # Spending bound examples
#'
#' # Design with futility only at analysis 1; efficacy only at analyses 2, 3
#' # Spending bound for efficacy; fixed bound for futility
#' # NOTE: test_upper and test_lower DO NOT WORK with gs_b; must explicitly make bounds infinite
#' # test_upper and test_lower DO WORK with gs_spending_bound
#' design <- gsDesign2:::gs_design_npe_(theta = c(.1, .2, .3), info = (1:3) * 40, info0 = (1:3) * 40,
#'               upper = gs_spending_bound,
#'               upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, 
#'                           param = NULL, timing = NULL),
#'               lower = gs_b, lpar = c(-1, -Inf, -Inf),
#'               test_upper = c(FALSE, TRUE, TRUE))
#' design
#'
#' # Spending function bounds
#' # 2-sided asymmetric bounds
#' # Lower spending based on non-zero effect
#' gsDesign2:::gs_design_npe_(theta = c(.1, .2, .3), info = (1:3) * 40,
#'              upper = gs_spending_bound,
#'              upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, 
#'                          param = NULL, timing = NULL),
#'              lower = gs_spending_bound,
#'              lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, 
#'                          param = -1, timing = NULL))
#'
#' # Two-sided symmetric spend, O'Brien-Fleming spending
#' # Typically, 2-sided bounds are binding
#' xx <- gsDesign2:::gs_design_npe_(theta = c(.1, .2, .3), theta1 = rep(0, 3), info = (1:3) * 40,
#'                     binding = TRUE,
#'                     upper = gs_spending_bound,
#'                     upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, 
#'                                 param = NULL, timing = NULL),
#'                     lower = gs_spending_bound,
#'                     lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, 
#'                                 param = NULL, timing = NULL))
#' xx
#'
#' # Re-use these bounds under alternate hypothesis
#' # Always use binding = TRUE for power calculations
#' upar <- (xx %>% filter(Bound=="Upper"))$Z
#' gsDesign2:::gs_design_npe_(theta = c(.1, .2, .3), info = (1:3) * 40,
#'              binding = TRUE,
#'              upar = upar,
#'              lpar = -upar)
#'
gs_design_npe_ <- function(theta = .1, theta1 = NULL, info = 1, info0 = NULL, info1 = NULL,
                          alpha = 0.025, beta = .1, binding = FALSE,
                          upper=gs_b, lower=gs_b, upar = qnorm(.975), lpar= -Inf,
                          test_upper = TRUE, test_lower = TRUE,
                          r = 18, tol = 1e-6){
  #######################################################################################
  # WRITE INPUT CHECK TESTS AND RETURN APPROPRIATE ERROR MESSAGES
  # info should be a scalar or vector of positive increasing values
  # info0, info1 should be NULL or of the same form as info
  # theta should be a scalar or vector of real values; if vector, same length as info
  # theta0, theta1 should be NULL or same form and length as theta
  # test_upper and test_lower should be logical scalar or vector; if vector same length as info
  # alpha and beta should be scalars with 0 < alpha < 1 - beta < 1
  
  # CHECK STATISTICAL INFORMATION PARAMETERS: info, info0, info1
  if (!is.vector(info, mode = "numeric")) stop("gs_design_npe(): info must be specified numeric vector")
  K <- length(info)
  if (is.null(info0)) info0 <- info
  if (is.null(info1)) info1 <- info
  if (!is.vector(info0, mode = "numeric")) stop("gs_design_npe(): info0 must be specified numeric vector or NULL")
  if (!is.vector(info1, mode = "numeric")) stop("gs_design_npe(): info1 must be specified numeric vector or NULL")
  if (length(info1) != length(info) || length(info0) != length(info) ) stop("gs_design_npe(): length of info, info0, info1 must be the same")
  if (min(info - lag(info,default = 0)<=0)) stop("gs_design_npe(): info much be strictly increasing and positive")
  if (min(info0 - lag(info0,default = 0)<=0)) stop("gs_design_npe(): info0 much be NULL or strictly increasing and positive")
  if (min(info1 - lag(info1,default = 0)<=0)) stop("gs_design_npe(): info1 much be NULL or strictly increasing and positive")
  
  # CHECK TREATMENT EFFECT PARAMETERS: theta, theta0, theta1
  if (!is.vector(theta, mode = "numeric")) stop("gs_design_npe(): theta must be a real vector")
  if (length(theta) == 1 && K > 1) theta <- rep(theta, K)
  if (length(theta) != K) stop("gs_design_npe(): if length(theta) > 1, must be same as info")
  if (theta[K] <= 0) stop("gs_design_npe(): final effect size must be > 0")
  if (is.null(theta1)){theta1 <- theta}else if (length(theta1)==1) theta1 <- rep(theta1,K)
  if (!is.vector(theta1, mode = "numeric")) stop("gs_design_npe(): theta1 must be a real vector")
  if (length(theta1) != K) stop("gs_design_npe(): if length(theta1) > 1, must be same as info")
  # CHECK CORRECT SPEC OF test_upper and test_lower
  if (length(test_upper) == 1 && K > 1) test_upper <- rep(test_upper, K)
  if (length(test_lower) == 1 && K > 1) test_lower <- rep(test_lower, K)
  
  ## Check test_upper and test_lower are logical and correct length
  if (!is.vector(test_upper, mode = "logical") || !is.vector(test_lower, mode = "logical"))
    stop("gs_design_npe(): test_upper and test_lower must be logical")
  if (!(length(test_upper) == 1 || length(test_upper) == K))
    stop("gs_design_npe(): test_upper must be length 1 or same length as info")
  if (!(length(test_lower) == 1 || length(test_lower) == K))
    stop("gs_design_npe(): test_lower must be length 1 or same length as info")
  ## Check that final test_upper value is TRUE
  if (!dplyr::last(test_upper)) stop("gs_design_npe(): last value of test_upper must be TRUE")
  
  ## Check alpha and beta numeric, scalar, 0 < alpha < 1 - beta
  if (!is.numeric(alpha)) stop("gs_design_npe(): alpha must be numeric")
  if (!is.numeric(beta)) stop("gs_design_npe(): beta must be numeric")
  if (length(alpha) != 1 || length(beta) != 1) stop("gs_design_npe(): alpha and beta must be length 1")
  if (alpha <= 0 || 1 - beta <= alpha || beta <= 0) stop("gs_design_npe(): must have 0 < alpha < 1 - beta < 1")
  
  ## END OF INPUT CHECKS ############################################################################
  
  # Initialize bounds, numerical integration grids, boundary crossing probabilities
  a <- rep(-Inf, K)
  b <- rep(Inf, K)
  hgm1_0 <- NULL
  hgm1_1 <- NULL
  upperProb <- rep(NA, K)
  lowerProb <- rep(NA, K)
  
  ## Compute fixed sample size for desired power and Type I error.
  minx <- ((qnorm(alpha) / sqrt(info0[K]) + qnorm(beta) / sqrt(info[K])) / theta[K])^2
  
  ## For a fixed design, this is all you need.
  if (K == 1) return(tibble::tibble(
    Analysis = 1,
    Bound = "Upper",
    Z= qnorm(1-alpha),
    Probability = 1 - beta,
    theta = theta,
    info = info * minx,
    info0 =info0 * minx)
  )
  
  ## Find an interval for information inflation to give correct power
  minpwr <- gs_power_npe_(theta = theta, theta1 = theta1,
                         info = info * minx, info1 = info * minx, info0 = info0 * minx,
                         binding = binding,
                         upper=upper, lower=lower, upar = upar, lpar= lpar,
                         test_upper = test_upper, test_lower = test_lower,
                         r = r, tol = tol)$Probability[K]
  
  ##### FOLLOWING IS PAINFUL AND SHOULD NEVER BE NEEDED
  ##### BUT IF IT IS NEEDED, IT TELLS YOU WHAT WENT WRONG!
  ##### NEED TO BRACKET TARGETED POWER BEFORE ROOT FINDING
  
  ## Ensure minx gives power < 1 - beta and maxx gives power > 1 - beta
  if (minpwr < 1 - beta){
    maxx <- 1.05 * minx
    ## Ensure maxx is sufficient information inflation to overpower
    err <- 1
    for(i in 1:10){
      maxpwr <- gs_power_npe_(theta = theta, theta1 = theta1,
                             info = info * maxx, info1 = info * maxx, info0 = info0 * maxx,
                             binding = binding,
                             upper=upper, lower=lower, upar = upar, lpar= lpar,
                             test_upper = test_upper, test_lower = test_lower,
                             r = r, tol = tol)$Probability[K]
      if (1  - beta > maxpwr){
        minx <- maxx
        maxx <- 1.05 * maxx
      }else{
        err <- 0
        break
      }
    }
    if (err) stop("gs_design_npe: could not inflate information to bracket power before root finding")
  }else{
    maxx <- minx
    minx <- maxx / 1.05
    err <- 1
    for(i in 1:10){
      if (1  - beta < gs_power_npe_(theta = theta, theta1 = theta1,
                                   info = info * minx, info1 = info1 * minx, info0 = info0 * minx,
                                   binding = binding,
                                   upper=upper, lower=lower, upar = upar, lpar= lpar,
                                   test_upper = test_upper, test_lower = test_lower,
                                   r = r, tol = tol)$Probability[K]
      ){maxx <- minx
      minx <- minx / 1.05}else{err <- 0
      break
      }
    }
    if (err) stop("gs_design_npe: could not deflate information to bracket targeted power before root finding")
  }
  #### EITHER TARGETED POWER NOW BRACKETED OR ERROR MESSAGE HAS BEEN RETURNED
  #### AND WE CAN ACTUALLY GO ON TO FIND THE ROOT
  
  ## Use root finding with the above function to find needed sample size inflation
  # Now we can solve for the inflation factor for the enrollment rate to achieve the desired power
  res <- try(
    uniroot(errbeta_, lower = minx, upper = maxx,
            theta = theta, theta1 = theta1, K = K, beta = beta,
            info = info, info1 = info1, info0 = info0, binding = binding,
            Zupper=upper, Zlower=lower, upar = upar, lpar= lpar,
            test_upper = test_upper, test_lower = test_lower,
            r = r, tol = tol)
  )
  if(inherits(res,"try-error")){stop("gs_design_npe: Sample size solution not found")}
  
  ## Update targeted info, info0 based on inflation factor and return a tibble with
  ## bounds, targeted information, and boundary crossing probabilities at each analysis
  return(gs_power_npe_(theta = theta, theta1 = theta1,
                      info = info * res$root, info1 = info1 * res$root, info0 = info0 * res$root,
                      binding = binding,
                      upper=upper, lower=lower, upar = upar, lpar= lpar,
                      test_upper = test_upper, test_lower = test_lower,
                      r = r, tol = tol))
}
## Create a function that uses gs_power_npe to compute difference from targeted power
## for a given sample size inflation factor
errbeta_ <- function(x = 1, K = 1, beta = .1, theta = .1, theta1 = .1, info = 1, info1 = 1, info0 = 1, binding = FALSE,
                    Zupper=gs_b, Zlower=gs_b, upar = qnorm(.975), lpar= -Inf,
                    test_upper = TRUE, test_lower = TRUE,
                    r = 18, tol = 1e-6){
  return(1 -  beta -
           gs_power_npe_(theta = theta, theta1 = theta1,
                        info = info * x, info1 = info1 * x, info0 = info0 * x, binding = binding,
                        upper = Zupper, lower = Zlower, upar = upar, lpar= lpar,
                        test_upper = test_upper, test_lower = test_lower,
                        r = r, tol = tol)$Probability[K])
}
