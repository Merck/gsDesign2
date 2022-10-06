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

#' @importFrom mvtnorm GenzBretz
#' 
#' @param enrollRates enrollment rates
#' @param failRates failure and dropout rates
#' @param ratio Experimental:Control randomization ratio (not yet implemented)
#' @param fh_test weighting tests
#' @param algorithm numerical algorithms
#' 
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Define the analysis time from input fh_test.
#'    \item Compute arm0 and arm1 using \code{gs_create_arm()}.
#'    \item Set a unique test.
#'    \item Compute the information fraction using \code{gs_info_combo()}.
#'    \item Compute the correlation between tests.
#'    \item Compute the correlation between analysis.
#'    \item Compute the overall correlation.
#'    \item Extract the sample size from  info.
#'    \item Compute information restricted to actual analysis.
#'    \item Compute the effect size.
#'    \item Return a list of info_all = info, info = info_fh, theta = theta_fh, corr = corr_fh.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#' 
#' @noRd
gs_utility_combo <- function(enrollRates,
                             failRates,
                             fh_test,
                             ratio = 1,
                             algorithm = GenzBretz(maxpts = 1e5, abseps = 1e-5),
                             ...){
  
  # Define analysis time
  analysisTimes <- sort(unique(fh_test$analysisTimes))
  
  # Define Arm
  gs_arm <- gs_create_arm(enrollRates, failRates,
                          ratio = ratio,                   # Randomization ratio
                          total_time = max(analysisTimes)) # Total study duration
  
  arm0 <- gs_arm[["arm0"]]
  arm1 <- gs_arm[["arm1"]]
  
  # Unique test
  u_fh_test <- unique(fh_test[, c("test","rho", "gamma", "tau")] )
  
  # Information Fraction
  info <- gs_info_combo(enrollRates, failRates, ratio,
                        analysisTimes = analysisTimes,
                        rho = u_fh_test$rho,
                        gamma = u_fh_test$gamma)
  
  # Correlation between test
  corr_test <- with(u_fh_test,
                    lapply(analysisTimes, function(tmax){
                      cov2cor(gs_sigma2_combo(arm0, arm1, tmax = tmax,
                                              rho = rho, gamma = gamma, tau = tau))
                    })
  )
  
  # Correlation between analysis
  info_split <- split(info, info$test)
  corr_time <- lapply(info_split, function(x){
    corr <- with(x, outer(sqrt(info), sqrt(info), function(x,y) pmin(x,y) / pmax(x,y)))
    rownames(corr) <- analysisTimes
    colnames(corr) <- analysisTimes
    corr
  })
  
  # Overall Correlation
  corr_combo <- diag(1, nrow = nrow(info))
  for(i in 1:nrow(info)){
    for(j in 1:nrow(info)){
      t1 <- as.numeric(info$Analysis[i])
      t2 <- as.numeric(info$Analysis[j])
      if(t1 <= t2){
        test1 <- as.numeric(info$test[i])
        test2 <- as.numeric(info$test[j])
        corr_combo[i,j] <- corr_test[[t1]][test1,test2] * corr_time[[test2]][t1, t2]
        corr_combo[j,i] <- corr_combo[i,j]
      }
    }
  }
  
  # Sample size
  n <- max(info$N)
  
  # Restricted to actual analysis
  info_fh <- merge(info, fh_test, all = TRUE)
  corr_fh <- corr_combo[! is.na(info_fh$gamma), ! is.na(info_fh$gamma)]
  info_fh <- subset(info_fh, ! is.na(gamma))
  
  # Effect size
  theta_fh <- (- info_fh$delta) / sqrt(info_fh$sigma2)
  
  list(info_all = info, info = info_fh, theta = theta_fh, corr = corr_fh)
  
}


#' Multivariate Normal Distribution for Multivariate Maximum Statistics
#'
#' Computes the distribution function of the multivariate normal distribution
#' with maximum statistics for arbitrary limits and correlation matrices
#' @importFrom mvtnorm GenzBretz
#' @inheritParams mvtnorm::pmvnorm
#' 
#' @param group the vector of test statistics group.
#' @param ... additional parameters transfer to `mvtnorm::pmvnorm`
#' 
#' @details
#' Let $Z = {Z_ij}$ be a multivariate normal distribution.
#' Here i is a group indicator and j is a within group statistics indicator.
#' Let G_i = max({Z_ij}) for all test within one group.
#' This program are calculating the probability
#'
#'   $$Pr( lower < max(G) < upper )$$
#'
#' @export
pmvnorm_combo <- function(lower,
                          upper,
                          group,
                          mean,
                          corr,
                          algorithm = GenzBretz(maxpts= 1e5, abseps= 1e-5),
                          ...){
  
  # Number of test in each group
  n_test <- as.numeric(table(group))
  
  
  # Ensure positive definitive of the correlation matrix
  if(! corpcor::is.positive.definite(corr)){
    corr <- corpcor::make.positive.definite(corr)
    corr <- stats::cov2cor(corr)
  }
  
  # One dimension test
  if(length(mean) == 1){
    p <- pnorm(mean, lower) - pnorm(mean, upper)
    return(p)
  }
  
  # One test for all group or lower bound is -Inf.
  if(all(n_test == 1) | all(lower == -Inf) ){
    p <- mvtnorm::pmvnorm(lower = rep(lower, n_test),
                          upper = rep(upper, n_test),
                          mean = mean,
                          corr = corr,
                          sigma = NULL,
                          algorithm = algorithm,
                          ...)
    
    return(p)
    
    # General Algorithm
  }else{
    
    # Re-arrange test based on the order for number of test
    group <- as.numeric(factor(group, order(n_test)))
    
    mean <- mean[order(group)]
    corr <- corr[order(group), order(group)]
    group <- group[order(group)]
    
    n_test <- as.numeric(table(group))
    
    
    
    # Split by number of test == 1
    lower1 <- lower[n_test == 1]
    upper1 <- upper[n_test == 1]
    
    lower2 <- lower[n_test > 1]
    upper2 <- upper[n_test > 1]
    
    # Combination of all possible test
    k <- length(lower2)
    test_ind <- split(matrix(c(1,-1), nrow = k, ncol = 2, byrow = TRUE), 1:k)
    test_ind <- expand.grid(test_ind)
    test <- split(test_ind, 1:nrow(test_ind))
    
    p <- sapply(test, function(x){
      lower_bound <- rep(c(lower1, rep(-Inf, k)), n_test)
      upper_bound <- rep(c(upper1, ifelse(x == 1, upper2, lower2)), n_test)
      
      p_bound <- mvtnorm::pmvnorm(lower = lower_bound,
                                  upper = upper_bound,
                                  mean = mean,
                                  corr = corr,
                                  sigma = NULL,
                                  algorithm = algorithm,
                                  ...)
      
      prod(x) * p_bound
      
    })
    
    return(sum(p))
    
  }
  
}


#' Create "npsurvSS" arm object
#'
#' @param total_time total analysis time
#' @inheritParams gs_info_ahr
#' 
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if there is only one stratum.
#'    \item Calculate the accrual duration.
#'    \item calculate the accrual intervals.
#'    \item Calculate the accrual parameter as the proportion of enrollment rate*duration.
#'    \item Set cure proportion to zero.
#'    \item set survival intervals and shape.
#'    \item Set fail rate in failRates to the Weibull scale parameter for the survival distribution in the arm 0.
#'    \item Set the multiplication of hazard ratio and fail rate to the Weibull scale parameter
#'    for the survival distribution in the arm 1.
#'    \item Set the shape parameter to one as the exponential distribution for
#'    shape parameter for the loss to follow-up distribution
#'    \item Set the scale parameter to one as the scale parameter for the loss to follow-up
#'     distribution since the exponential distribution is supported only
#'    \item Create arm 0 using \code{npsurvSS::create_arm()} using the parameters for arm 0.
#'    \item Create arm 1 using \code{npsurvSS::create_arm()} using the parameters for arm 1.
#'    \item Set the class of the two arms.
#'    \item Return a list of the two arms.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @noRd
gs_create_arm <- function(enrollRates,
                          failRates,
                          ratio,
                          total_time = 1e6){
  
  n_stratum <- length(unique(enrollRates$Stratum))
  if(n_stratum > 1){
    stop("Only one stratum is supported")
  }
  
  accr_time     <- sum(enrollRates$duration)
  accr_interval <- cumsum(enrollRates$duration)
  accr_param    <- enrollRates$rate * enrollRates$duration / sum(enrollRates$rate * enrollRates$duration)
  
  surv_cure     <- 0                    # No cure proportion
  surv_interval <- c(0, c(utils::head(failRates$duration, -1), Inf))
  surv_shape    <- 1                    # Exponential Distribution
  surv_scale0   <- failRates$failRate
  surv_scale1   <- failRates$hr * failRates$failRate
  
  loss_shape    <- 1                         # Exponential Distribution
  loss_scale    <- failRates$dropoutRate[1]  # Only Exponential Distribution is supported
  
  # Control Group
  arm0 <- npsurvSS::create_arm(size = 1,
                               
                               accr_time = accr_time,
                               accr_dist = "pieceuni",
                               accr_interval = accr_interval,
                               accr_param = accr_param,
                               
                               surv_cure = surv_cure,
                               surv_interval = surv_interval,
                               surv_shape = surv_shape,
                               surv_scale = surv_scale0,
                               
                               loss_shape = loss_shape,
                               loss_scale = loss_scale,
                               
                               total_time = total_time)
  
  
  # Control Group
  arm1 <- npsurvSS::create_arm(size = ratio,
                               
                               accr_time = accr_time,
                               accr_dist = "pieceuni",
                               accr_interval = accr_interval,
                               accr_param = accr_param,
                               
                               surv_cure = surv_cure,
                               surv_interval = surv_interval,
                               surv_shape = surv_shape,
                               surv_scale = surv_scale1,
                               
                               loss_shape = loss_shape,
                               loss_scale = loss_scale,
                               
                               total_time = total_time)
  
  class(arm0) <- c("list", "arm")
  class(arm1) <- c("list", "arm")
  
  list(arm0 = arm0,
       arm1 = arm1)
  
}

#' Create weight in max combo test
#' @param rho weighting parameter
#' @param gamma weighting parameter
#' @param tau weighting parameter
#' 
#' @noRd
get_combo_weight <- function(rho, gamma, tau){
  
  stopifnot(length(rho) == length(gamma))
  stopifnot(length(rho) == length(tau))
  
  weight <- list()
  for(i in 1:length(rho)){
    
    if(tau[i] == -1){
      tmp_tau <- NULL
    }else{
      tmp_tau <- tau[i]
    }
    
    text <- paste0("weight <- function(x, arm0, arm1){
                            wlr_weight_fh(x, arm0, arm1
                                ,rho =", rho[i],
                   ", gamma =", gamma[i],
                   ", tau =", tmp_tau,  ")}")
    
    weight[[i]] <- text
    
  }
  
  weight
}

#' Compute delta in max combo test
#' @noRd
gs_delta_combo <- function(arm0,
                           arm1,
                           tmax = NULL,
                           rho,
                           gamma,
                           tau = rep(-1, length(rho)),
                           approx="asymptotic",
                           normalization = FALSE) {
  
  stopifnot(length(tmax) == 1)
  
  weight <- get_combo_weight(rho, gamma, tau)
  delta <- sapply(weight, function(x){
    x <- eval(parse(text = x))
    gs_delta_wlr(arm0, arm1, tmax = tmax, weight = x,
                 approx = approx, normalization = normalization)
  })
  
  delta
  
}

#' Compute delta in max combo test
#' @noRd
gs_sigma2_combo <- function(arm0,
                            arm1,
                            tmax = NULL,
                            rho,
                            gamma,
                            tau = rep(-1, length(rho)),
                            approx="asymptotic"){
  
  stopifnot(length(tmax) == 1)
  stopifnot(length(rho) == length(gamma))
  stopifnot(length(rho) == length(tau))
  
  rho1   <- outer(rho, rho, function(x,y) (x+y)/2 )
  gamma1 <- outer(gamma, gamma, function(x,y) (x+y)/2 )
  
  sigma2 <- rho1
  for(i in 1:length(rho)){
    
    weight <- get_combo_weight(rho1[i,], gamma1[i,],tau)
    
    sigma2[i,] <- sapply(weight, function(x){
      x <- eval(parse(text = x))
      gs_sigma2_wlr(arm0, arm1, tmax = tmax, weight = x,
                    approx = approx)
      
    })
  }
  
  sigma2
  
}

#' MaxCombo Group sequential boundary crossing probabilities
#'
#' @inheritParams pmvnorm_combo
#' @param upper_bound a numeric vector of upper bound
#' @param lower_bound a numeric vector of lower bound
#' @param analysis an integer vector of the interim analysis index
#' @param theta a numeric vector of effect size under alternative hypothesis
#' @param corr a matrix of correlation matrix
#'
#' @importFrom mvtnorm GenzBretz
#'
#' @noRd
gs_prob_combo <- function(upper_bound,
                          lower_bound,
                          analysis,
                          theta,
                          corr,
                          algorithm = GenzBretz(maxpts= 1e5, abseps= 1e-5),
                          ...){
  
  n_analysis <- length(unique(analysis))
  
  p <- c()
  q <- c()
  for(k in 1:n_analysis){
    k_ind <- analysis <= k
    
    
    # Efficacy Bound
    if(k == 1){
      lower <- upper_bound[1]
      upper <- Inf
    }else{
      lower <- c(lower_bound[1:(k-1)], upper_bound[k])
      upper <- c(upper_bound[1:(k-1)], Inf)
    }
    
    
    p[k] <- pmvnorm_combo(lower,
                          upper,
                          group = analysis[k_ind],
                          mean = theta[k_ind],
                          corr = corr[k_ind, k_ind])
    
    # Futility Bound
    if(k == 1){
      lower <- -Inf
      upper <- lower_bound[k]
    }else{
      lower <- c(lower_bound[1:(k-1)], -Inf)
      upper <- c(upper_bound[1:(k-1)], lower_bound[k])
    }
    
    q[k] <- pmvnorm_combo(lower,
                          upper,
                          group = analysis[k_ind],
                          mean  = theta[k_ind],
                          corr  = corr[k_ind, k_ind])
    
  }
  
  data.frame(Bound = rep(c("Upper", "Lower"), each = n_analysis),
             Probability = c(cumsum(p),cumsum(q)))
  
}