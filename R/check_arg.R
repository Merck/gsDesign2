#' A function to check the arguments \code{enrollRates} used in gsDesign2
#'
#' @param enrollRates enrollment rates
#'
#' @return TURE or FALSE
#'
#' @examples
#' 
#' enrollRates <- tibble::tibble(Stratum = "All", duration = c(2, 2, 10), rate = c(3, 6, 9))
#' check_enrollRates(enrollRates)
#' 
#' @noRd
#' 
check_enrollRates <- function(enrollRates){
  
  # --------------------------- #
  #   check the Stratum column  #
  # --------------------------- #
  # if("Stratum" %in% colnames(enrollRates)){
  #   stop("The enrollRates is a tibble which contains a column called `Stratum`!")
  # }
  
  # --------------------------- #
  #   check the duration column #
  # --------------------------- #
  if(!"duration" %in% colnames(enrollRates)){
    stop("The enrollRates is a tibble which contains a column called `duration`!")
  }
  # the duration is numerical values
  if(!is.numeric(enrollRates$duration)){
    stop("The `duration`column in enrollRates should be numeric!")
  }
  
  # the duration is positive numbers
  if(sum(!enrollRates$duration > 0) != 0){
    stop("The `duration` column in enrollRates should be positive numbers!")
  }
  
  # --------------------------- #
  #   check the rate column     #
  # --------------------------- #
  if(!"rate" %in% colnames(enrollRates)){
    stop("The enrollRates is a tibble which contains a column called `rate`!")
  }
  
  # the rate is numerical values
  if(!is.numeric(enrollRates$rate)){
    stop("The `rate`column in enrollRates should be numeric!")
  }
  
  # the rate is positive numbers
  if(sum(!enrollRates$rate >= 0) != 0){
    stop("The `rate` column in enrollRates should be positive numbers!")
  }
}



#' A function to check the arguments \code{failRates} used in gsDesign2
#'
#' @param failRates failure rates
#'
#' @return TURE or FALSE
#'
#' @examples
#' 
#' failRates <- tibble::tibble(Stratum = "All", duration = c(3, 100), 
#'                             failRate = log(2) / c(9, 18), hr = c(.9, .6), 
#'                             dropoutRate = rep(.001, 2))
#' check_failRates(failRates)
#' 
#' @noRd
check_failRates <- function(failRates){
  
  # --------------------------- #
  #   check the Stratum column  #
  # --------------------------- #
  # if(!"Stratum" %in% colnames(enrollRates)){
  #   stop("The enrollRates is a tibble which contains a column called `Stratum`!")
  # }
  
  # --------------------------- #
  #   check the duration column #
  # --------------------------- #
  if(!"duration" %in% colnames(failRates)){
    stop("The failRates is a tibble which contains a column called `duration`!")
  }
  # the duration is numerical values
  if(!is.numeric(failRates$duration)){
    stop("The `duration`column in failRates should be numeric!")
  }
  
  # the duration is positive numbers
  if(sum(!failRates$duration > 0) != 0){
    stop("The `duration` column in failRates should be positive numbers!")
  }
  
  # --------------------------- #
  #   check the failRate column #
  # --------------------------- #
  if(!"failRate" %in% colnames(failRates)){
    stop("The failRates is a tibble which contains a column called `failRate`!")
  }
  
  # the rate is failRates values
  if(!is.numeric(failRates$failRate)){
    stop("The `failRate`column in failRates should be numeric!")
  }
  
  # the rate is positive numbers
  if(sum(!failRates$failRate > 0) != 0){
    stop("The `failRate` column in failRates should be positive numbers!")
  }
  
  # --------------------------- #
  #   check the hr column       #
  # --------------------------- #
  if("hr" %in% colnames(failRates)){
    
    if(!is.numeric(failRates$hr)){
      stop("The `hr`column in failRates should be numeric!")
    }
    
    if(sum(!failRates$hr > 0) != 0){
      stop("The `hr` column in failRates should be positive numbers!")
    }
  }
  
  # --------------------------- #
  # check the dropoutRate column#
  # --------------------------- #
  if(!"dropoutRate" %in% colnames(failRates)){
    stop("The failRates is a tibble which contains a column called `dropoutRate`!")
  }
  
  # the rate is numerical values
  if(!is.numeric(failRates$dropoutRate)){
    stop("The `dropoutRate`column in failRates should be numeric!")
  }
  
  # the rate is positive numbers
  if(sum(!failRates$dropoutRate >= 0) != 0){
    stop("The `dropoutRate` column in failRates should be positive numbers!")
  }
}



#' A function to check the arguments \code{enrollRates} and \code{failRates} used in gsDesign2
#'
#' @param enrollRates enrollment rates
#' @param failRates failure rates
#' @return TURE or FALSE
#'
#' @examples
#' 
#' enrollRates <- tibble::tibble(Stratum = "All", duration = c(2, 2, 10), rate = c(3, 6, 9))
#' failRates <- tibble::tibble(Stratum = "All", duration = c(3, 100), 
#'                             failRate = log(2) / c(9, 18), hr = c(.9, .6), 
#'                             dropoutRate = rep(.001, 2))
#' check_enrollRates(enrollRates, failRates)
#' 
#' @noRd
#' 
check_enrollRates_failRates <- function(enrollRates, failRates){
  
  if("Stratum" %in% colnames(enrollRates) && "Stratum" %in% colnames(failRates)){
    strata_enroll <- unique(enrollRates$Stratum)
    strata_fail   <- unique(failRates$Stratum)
    strata_common <- dplyr::intersect(strata_enroll, strata_fail)
    
    if(sum(strata_common %in% strata_enroll) != length(strata_enroll)){
      stop("The `Strata` column in the input argument `enrollRates` and `failRates` must be the same!")
    }
  }
}


#' A function to check the arguments \code{analysisTimes} used in gsDesign2
#'
#' @param analysisTimes analysis time
#'
#' @return TURE or FALSE
#'
#' @examples
#' analysisTimes <- 20
#' check_analysisTimes(analysisTimes)
#' 
#' analysisTimes <- c(20, 30)
#' check_analysisTimes(analysisTimes)
#' 
#' @noRd
check_analysisTimes <- function(analysisTimes){
  cond1 <- !is.numeric(analysisTimes)
  cond2 <- !is.vector(analysisTimes)
  cond3 <- min(analysisTimes - dplyr::lag(analysisTimes, def=0))<=0
  if ( cond1 || cond2 || cond3 ){
    stop("The input argument `analysisTimes` must be NULL a numeric vector with positive increasing values!")
  }
}


#' A function to check the arguments \code{events} used in gsDesign2
#'
#' @param events number of events  
#'
#' @return TURE or FALSE
#' 
#' @examples
#' events <- 20
#' check_events(events)
#' 
#' events <- c(20, 30)
#' check_events(events)
#' 
#' @noRd
check_events <- function(events){
  cond1 <- !is.numeric(events)
  cond2 <- !is.vector(events)
  cond3 <- min(events - dplyr::lag(events, default=0))<=0
  if ( cond1 || cond2 || cond3 ){
    stop("The input argument `events` must be NULL or a numeric vector with positive increasing values!")
  }
}

#' A function to check the arguments \code{totalDuration} used in gsDesign2
#'
#' @param totalDuration total duration
#'
#' @return TURE or FALSE
#' 
#' @examples
#' totalDuration <- 36
#' check_totalDuration(totalDuration)
#' 
#' totalDuration <- c(36, 48)
#' check_totalDuration(totalDuration)
#' 
#' @noRd
check_totalDuration <- function(totalDuration){
  if(!is.numeric(totalDuration)){
    stop("The input argument `totalDuration` must be a non-empty vector of positive numbers!")
  }
  
  if(sum(!totalDuration > 0) != 0){
    stop("The input argument `totalDuration` must be a non-empty vector of positive numbers!")
  }
}

#' A function to check the arguments \code{ratio} used in gsDesign2
#'
#' @param ratio  randomization ratio
#'
#' @return TURE or FALSE
#' 
#' @examples
#' ratio <- 1
#' check_ratio(ratio)
#' 
#' @noRd
check_ratio <- function(ratio){
  if(!is.numeric(ratio)){
    stop("The input argument `ratio` must be a numerical number!")
  }
  
  if(ratio <= 0){
    stop("The input argument `ratio` must be a positive number!")
  }
}

#' A function to check the arguments \code{info} used in `gs_power_npe` or `gs_design_npe` in gsDesign2
#'
#' @param info statistical information
#'
#' @return TURE or FALSE
#' 
#' @examples
#' info <- 1:3
#' check_info(info)
#' 
#' @noRd
check_info <- function(info){
  if(!is.vector(info, mode = "numeric")){
    stop("gs_design_npe() or gs_power_npe(): info must be specified numeric vector!")
  } 
  if (min(info - lag(info, default = 0)) <= 0){
    stop("gs_design_npe() or gs_power_npe(): info much be strictly increasing and positive!")
  } 
}

#' A function to check the arguments \code{theta} used in `gs_power_npe` or `gs_design_npe` in gsDesign2
#'
#' @param theta  treatment effect
#' @param K number of total analysis
#'
#' @return TURE or FALSE
#' 
#' @examples
#' theta <- 0.5
#' check_theta(theta)
#' 
#' @noRd
check_theta <- function(theta, K){
  if(!is.vector(theta, mode = "numeric")){
    stop("gs_design_npe() or gs_power_npe(): theta must be a real vector!")
  } 
  
  if(length(theta) != K){
    stop("gs_design_npe() or gs_power_npe(): if length(theta) > 1, must be same as info!")
  }
  
  if(theta[K] < 0){
    stop("gs_design_npe() or gs_power_npe(): final effect size must be > 0!")
  } 
}

#' A function to check the arguments \code{test_upper} used in `gs_power_npe` or `gs_design_npe` in gsDesign2
#'
#' @param test_upper  test upper or lower
#' @param K number of total analysis
#'
#' @return TURE or FALSE
#' 
#' @examples
#' test_upper <- TRUE
#' check_test_upper(test_upper)
#' 
#' @noRd
check_test_upper <- function(test_upper, K){
  ## Check test_upper and test_lower are logical and correct length
  if(!is.vector(test_upper, mode = "logical")){
    stop("gs_design_npe() or gs_power_npe(): test_upper must be logical!")
  }
    
  if(!(length(test_upper) == 1 || length(test_upper) == K)){
    stop("gs_design_npe() or gs_power_npe(): test_upper must be length 1 or same length as info!")
  }
  
  # check that final test_upper value is TRUE
  if(!dplyr::last(test_upper)){
    stop("gs_design_npe(): last value of test_upper must be TRUE!")
  }
  
}

#' A function to check the arguments \code{text_lower} used in `gs_power_npe` or `gs_design_npe` in gsDesign2
#'
#' @param test_lower  test upper or lower
#' @param K number of total analysis
#'
#' @return TURE or FALSE
#' 
#' @examples
#' test_lower <- TRUE
#' check_test_lower(test_lower)
#' 
#' @noRd
check_test_lower <- function(test_lower, K){
  ## Check test_upper and test_lower are logical and correct length
  if (!is.vector(test_lower, mode = "logical")){
    stop("gs_design_npe() or gs_power_npe(): test_lower must be logical!")
  }
  
  if (!(length(test_lower) == 1 || length(test_lower) == K)){
    stop("gs_design_npe() or gs_power_npe(): test_lower must be length 1 or same length as info!")
  }
}

#' A function to check the arguments \code{alpha} and \code{beta} in gsDesign2
#'
#' @param alpha type I error
#' @param beta  type II error
#'
#' @return TURE or FALSE
#' 
#' @examples
#' alpha <- 0.025
#' beta <- 0.2
#' check_alpha_beta(alpha, beta)
#' 
#' @noRd
check_alpha_beta <- function(alpha, beta){
  if(!is.numeric(alpha)) stop("alpha must be numeric!")
  if(!is.numeric(beta)) stop("beta must be numeric!")
  if(length(alpha) != 1 || length(beta) != 1) stop("alpha and beta must be length 1!")
  if(alpha <= 0 || 1 - beta <= alpha || beta <= 0) stop("must have 0 < alpha < 1 - beta < 1!")
}

#' A function to check the arguments \code{IF} in gsDesign2
#'
#' @param IF statistical informational fraction
#'
#' @return TURE or FALSE
#' 
#' @examples
#' IF <- 1:3/3
#' check_IF(IF)
#' 
#' @noRd
check_IF <- function(IF){
  msg <- "gs_design_ahr(): IF must be a positive number or positive increasing sequence on (0, 1] with final value of 1"
  if(!is.vector(IF, mode = "numeric")) stop(msg)
  if(min(IF - dplyr::lag(IF, def = 0)) <= 0) stop(msg)
  if(max(IF) != 1) stop(msg)
}

