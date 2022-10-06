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

#' Lower and Upper Bound of Group Sequential Design
#' 
#' @importFrom mvtnorm GenzBretz
#' 
#' @param alpha a numeric vector of cumulative allocated alpha in each interim analysis
#' @param beta  a numeric vector of cumulative allocated beta in each interim analysis
#' @param theta a numeric vector of effect size under alternative.
#' @param corr  a matrix of correlation matrix
#' @param analysis a numeric vector of interim analysis indicator. Default is 1:length(alpha).
#' @param theta0 a numeric vector of effect size under null hypothesis. Default is 0.
#' @param binding_lower_bound a logical value to indicate binding lower bound.
#' @param alpha_bound logical value to indicate if alpha is Type I error or upper bound. Default is FALSE.
#' @param beta_bound logical value to indicate if beta is Type II error or lower bound. Default is FALSE.
#' @inheritParams pmvnorm_combo
#' 
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Create a vector of allocated alpha in each interim analysis from the cumulative allocated alpha.
#'    \item Create a vector of allocated beta in each interim analysis from the cumulative allocated beta.
#'    \item Extract the number of analysis.
#'    \item Find the upper and lower bound by solving multivariate normal distribution using \code{pmvnorm_combo}
#'    \item
#'    \item Return a data frame of upper and lower boundaries of group sequential design.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @examples
#' library(gsDesign)
#'
#' x <- gsDesign::gsSurv( k = 3 , test.type = 4 , alpha = 0.025 ,
#'                        beta = 0.2 , astar = 0 , timing = c( 1 ) ,
#'                        sfu = sfLDOF , sfupar = c( 0 ) , sfl = sfLDOF ,
#'                        sflpar = c( 0 ) , lambdaC = c( 0.1 ) ,
#'                        hr = 0.6 , hr0 = 1 , eta = 0.01 ,
#'                        gamma = c( 10 ) ,
#'                        R = c( 12 ) , S = NULL ,
#'                        T = 36 , minfup = 24 , ratio = 1 )
#'
#' cbind(x$lower$bound, x$upper$bound)
#'
#' gsdmvn:::gs_bound(alpha = sfLDOF(0.025, 1:3/3)$spend,
#'          beta = sfLDOF(0.2, 1:3/3)$spend,
#'          analysis = 1:3,
#'          theta = x$theta[2] * sqrt(x$n.I),
#'          corr = outer(1:3, 1:3, function(x,y) pmin(x,y) / pmax(x,y)))
#'
#' @noRd
#'
gs_bound <- function(alpha,
                     beta,
                     theta,
                     corr,
                     analysis = 1:length(alpha),
                     theta0 = rep(0, length(analysis)),
                     binding_lower_bound = FALSE,
                     algorithm = GenzBretz(maxpts= 1e5, abseps= 1e-5),
                     alpha_bound = FALSE,
                     beta_bound = FALSE,
                     ...){
  
  
  alpha <- c(alpha[1], diff(alpha))
  beta  <- c(beta[1],  diff(beta))
  
  lower <- NULL
  upper <- NULL
  .lower <- -Inf
  
  n_analysis <- length(unique(analysis))
  
  for(k in 1:n_analysis){
    k_ind <- analysis <= k
    
    bound_fun <- function(.lower, .upper, .prob, .theta, binding_lower_bound = FALSE){
      
      if(binding_lower_bound){
        lower_bound <- c(lower, .lower)
      }else{
        lower_bound <- c(rep(-Inf, k-1), .lower)
      }
      upper_bound <- c(upper, .upper)
      
      p <- pmvnorm_combo(lower_bound,
                         upper_bound,
                         group = analysis[k_ind],
                         mean = .theta[k_ind],
                         corr = corr[k_ind, k_ind], ...)
      
      p - .prob
    }
    
    
    # change .lower for different type of test (gsDesign test.type)
    if(beta_bound){
      .lower <- sum(beta[1:k])
    }else{
      .lower <- uniroot(bound_fun, .lower = -Inf, .prob = beta[k], .theta = theta,
                        binding_lower_bound = TRUE,
                        interval = c(-20, 20), extendInt = "yes")$root
    }
    
    if(alpha_bound){
      .upper <- sum(alpha[1:k])
    }else{
      .upper <- uniroot(bound_fun, .upper = Inf, .prob = alpha[k], .theta = theta0,
                        binding_lower_bound = FALSE,
                        interval = c(-20, 20), extendInt = "yes")$root
    }
    
    lower <- c(lower, .lower)
    upper <- c(upper, .upper)
  }
  
  # Ensure final analysis bound are the same
  lower[n_analysis] <- upper[n_analysis]
  
  data.frame(upper = upper, lower = lower)
  
}
