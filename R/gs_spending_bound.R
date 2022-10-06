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

#' @importFrom dplyr summarize
#' @importFrom gsDesign gsDesign sfLDOF
#' @importFrom stats qnorm
NULL
#' Derive spending bound for group sequential boundary
#'
#' Computes one bound at a time based on spending under given distributional assumptions.
#' While user specifies \code{gs_spending_bound()} for use with other functions,
#' it is not intended for use on its own.
#' Most important user specifications are made through a list provided to functions using \code{gs_spending_bound()}.
#' Function uses numerical integration and Newton-Raphson iteration to derive an individual bound for a group sequential
#' design that satisfies a targeted boundary crossing probability.
#' Algorithm is a simple extension of that in Chapter 19 of Jennison and Turnbull (2000).
#'
#' @param k analysis for which bound is to be computed
#' @param par a list with the following items:
#' \code{sf} (class spending function),
#' \code{total_spend} (total spend),
#' \code{param} (any parameters needed by the spending function \code{sf()}),
#' \code{timing} (a vector containing values at which spending function is to be evaluated or NULL if information-based spending is used),
#' \code{max_info} (when \code{timing} is NULL, this can be input as positive number to be used with \code{info} for information fraction at each analysis)
#' @param hgm1 subdensity grid from h1 (k=2) or hupdate (k>2) for analysis k-1; if k=1, this is not used and may be NULL
#' @param theta natural parameter used for lower bound only spending;
#' represents average drift at each time of analysis at least up to analysis k;
#' upper bound spending is always set under null hypothesis (theta = 0)
#' @param info statistical information at all analyses, at least up to analysis k
#' @param efficacy TRUE (default) for efficacy bound, FALSE otherwise
#' @param test_bound a logical vector of the same length as \code{info} should indicate which analyses will have a bound
#' @param r  Integer, at least 2; default of 18 recommended by Jennison and Turnbull
#' @param tol Tolerance parameter for convergence (on Z-scale)
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Set the spending time at analysis.
#'    \item Compute the cumulative spending at analysis.
#'    \item Compute the incremental spend at each analysis.
#'    \item Set test_bound a vector of length k > 1 if input as a single value.
#'    \item Compute spending for current bound.
#'    \item Iterate to convergence as in gsbound.c from gsDesign.
#'    \item Compute subdensity for final analysis in rejection region.
#'    \item Validate the output and return an error message in case of failure.
#'    \item Return a numeric bound (possibly infinite).
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @return returns a numeric bound (possibly infinite) or, upon failure, generates an error message.
#' @author Keaven Anderson \email{keaven_anderson@@merck.com}
#' @references Jennison C and Turnbull BW (2000), \emph{Group Sequential
#' Methods with Applications to Clinical Trials}. Boca Raton: Chapman and Hall.
#' @export
gs_spending_bound <- function(k = 1,
                              par = list(sf = gsDesign::sfLDOF,
                                         total_spend = 0.025,
                                         param = NULL,
                                         timing = NULL,
                                         max_info = NULL),
                              hgm1 = NULL,
                              theta = .1,
                              info = 1:3,
                              efficacy = TRUE,
                              test_bound = TRUE,
                              r = 18,
                              tol = 1e-6){
  # ---------------------------------- #
  #  check and initialize inputs       #
  # ---------------------------------- #
  # Make test_bound a vector of length k > 1 if input as a single value
  if(length(test_bound) == 1 && k > 1){test_bound <- rep(test_bound, k)}
  
  # ---------------------------------- #
  #  set spending time at analyses     #
  # ---------------------------------- #
  if(!is.null(par$timing)){
    timing <- par$timing
  }else{
    if(is.null(par$max_info)){
      timing <- info / max(info)
    }else{
      timing <- info / par$max_info
    } 
  }
  
  # ---------------------------------- #
  #   compute cumulative spending      #
  #         at each analyses           #
  # ---------------------------------- #
  spend <- par$sf(alpha = par$total_spend, t = timing, param = par$param)$spend
  
  # ---------------------------------- #
  #   compute incremental spending     #
  #         at each analyses           #
  # ---------------------------------- #
  old_spend <- 0

  for(i in 1:k){
    if (test_bound[i]){          # Check if spending is taken at analysis i
      xx <- spend[i] - old_spend # Cumulative spending minus previous spending
      old_spend <- spend[i]      # Reset previous spending
      spend[i] <- xx             # Incremental spend at analysis i
    }else{
      spend[i] <- 0              # 0 incremental spend if no testing at analysis i
    } 
  }
  
  
  # Now just get spending for current bound
  spend <- spend[k]
  
  # ---------------------------------- #
  #   compute lower bound              #
  #      at each analyses              #
  # ---------------------------------- #
  # lower bound
  if (!efficacy){
    # If no spending, return -Inf for bound
    if(spend <= 0){return(-Inf)} 
    
    # if theta not a vector, make it one
    # theta is for lower bound only
    if(length(theta) == 1) theta <- rep(theta, length(info))
    
    # set starting value
    a <- qnorm(spend) + sqrt(info[k]) * theta[k]
    
    # if it is the first analysis: no need for iteration
    if(k == 1){return(a)} 
    
    # Extremes for numerical integration
    mu <- theta[k] * sqrt(info[k])
    EXTREMElow <- mu - 3 - 4 * log(r)
    EXTREMEhi <- mu + 3 + 4 * log(r)
    
    # iterate to convergence as in gsbound.c from gsDesign
    adelta <- 1
    j <- 0
    
    # ---------------------------------------------------------------- # 
    # FOLLOWING UPDATE ALGORITHM FROM GSDESIGN::GSBOUND.C              #
    # use 1st order Taylor's series to update boundaries               #
    # maximum allowed change is 1                                      #
    # maximum value allowed is z1[m1]*rtIk to keep within grid points  #
    # ---------------------------------------------------------------- # 
    while(abs(adelta) > tol){ 
      
      # get grid for rejection region
      hg <- hupdate(theta = theta[k], I =  info[k], a = -Inf, b = a, thetam1 = theta[k-1], Im1 = info[k-1], gm1 = hgm1, r = r)
      i <- length(hg$h)
      
      # compute lower bound crossing (pik)
      pik <- sum(hg$h)  
      adelta <- spend - pik
      dplo <- hg$h[i] / hg$w[i]
      
      if(adelta > dplo){
        adelta <- 1
      }else if(adelta < -dplo){
        adelta <- -1
      }else{
        adelta <- adelta / dplo
      }
      
      a <- a + adelta
      
      if(a > EXTREMEhi){
        a <- EXTREMEhi
      }else if(a < EXTREMElow){
        a <- EXTREMElow
      }
      
      if (abs(adelta) < tol){return(a)}
      
      j <- j + 1
      if (j > 20){
        stop(paste("gs_spending_bound(): bound_update did not converge for lower bound calculation, analysis", k, " !"))
      }
    }
  }else{
  # ---------------------------------- #
  #   compute upper bound              #
  #      at each analyses              #
  # ---------------------------------- #
    if(spend <= 0){return(Inf)}
    
    # set starting value
    b <- qnorm(spend, lower.tail = FALSE)
    
    # if it is the first analysis: no iteration needed
    if(k == 1){return(b)} 
    
    # if it is not the first analysis
    for(iter in 0:20){
      
      # sub-density for final analysis in rejection region
      hg <- hupdate(theta = 0, I =  info[k], a = b, b = Inf, thetam1 = 0, Im1 = info[k-1], gm1 = hgm1, r = r)
      
      # compute probability of crossing bound
      pik <- sum(hg$h) 
      
      # compute the derivative of bound crossing at b[k]
      dpikdb <- hg$h[1] / hg$w[1] 
      
      # update upper boundary by Newton-Raphson method
      b_old <- b
      b <- b - (spend - pik) / dpikdb 
      if(abs(b - b_old) < tol){return(b)}
    }
    stop(paste("gs_spending_bound(): bound_update did not converge for upper bound calculation, analysis", k, " !"))
  }
}