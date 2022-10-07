#' Sample Size Calculation based on RMST method 
#' 
#' @param enrollRates enrollment rates
#' @param failRates failure and dropout rates
#' @param analysisTimes Minimum time of analysis
#' @param ratio Experimental:Control randomization ratio
#' @param alpha One-sided Type I error (strictly between 0 and 1)
#' @param beta  Power (`NULL` to compute power or strictly between 0 and `1 - alpha` otherwise)
#' @param test A string specifies the type of statistical test.
#'             Default is \code{"survival difference"} (a Kaplan-Meier based test). 
#'             One can also set it as \code{"rmst difference"} (another Kaplan-Meier based test)
#' @param tau desired milestone for \code{test = "survival difference"} or \code{test = "rmst difference"}
#' @return a list with \code{enrollRates}, \code{failRates}, \code{bounds}, \code{analysis} and \code{design}
#'
#' @examples 
#' # set enrollment rates
#' enrollRates <- tibble::tibble(Stratum = "All", duration = 12, rate = 500/12)
#' 
#' # set failure rates
#' failRates <- tibble::tibble(
#'   Stratum = "All",
#'   duration = c(4, 100),
#'   failRate = log(2) / 15,  # median survival 15 month
#'   hr = c(1, .6),
#'   dropoutRate = 0.001)
#'
#' fixed_design_size_rmst(enrollRates, failRates, analysisTimes = 36)
#' fixed_design_size_rmst(enrollRates, failRates, analysisTimes = 36, beta = 1 - 0.887)
#' fixed_design_size_rmst(enrollRates, failRates, analysisTimes = 36, tau = 18)
#' 
#' @noRd
fixed_design_size_rmst <- function(enrollRates, 
                                   failRates, 
                                   analysisTimes,
                                   ratio = 1,
                                   alpha = 0.025, 
                                   beta = 0.1,
                                   test = "rmst difference",
                                   tau = NULL){
  
  gs_arm <- gs_create_arm(enrollRates, failRates, ratio = ratio,  total_time = analysisTimes) 
  arm0 <- gs_arm[["arm0"]]
  arm1 <- gs_arm[["arm1"]]
  
  n <- sum(enrollRates$duration * enrollRates$rate)
  
  # Sample size for RMST at cut point 
  npsurv <- npsurvSS::size_two_arm(arm0, arm1, 
                                   alpha = alpha, power = 1 - beta,  
                                   test = list(test = test, milestone = if(is.null(tau)){arm0$total_time}else{tau})) 
  bounds <- tibble::tibble(
    Analysis = 1,
    Bound = "Upper",
    Probability = 1 - beta,
    Probability0 = alpha,
    Z = - qnorm(alpha)
  )
  
  analysis <- tibble::tibble(
    Analysis = 1, 
    Time = analysisTimes,
    N = npsurv[["n"]], 
    Events = npsurv[["d"]]
  )
  
  
  res <- list(enrollRates = enrollRates %>% mutate(rate = rate * npsurv[["n"]] / n), 
              failRates = failRates, 
              bounds = bounds,
              analysis = analysis)
  
  res
}


#' Power calculation based on RMST method 
#' 
#' @param enrollRates enrollment rates
#' @param failRates failure and dropout rates
#' @param analysisTimes Minimum time of analysis
#' @param ratio Experimental:Control randomization ratio
#' @param alpha One-sided Type I error (strictly between 0 and 1)
#' @param test A string specifies the type of statistical test.
#'             Default is \code{"survival difference"} (a Kaplan-Meier based test). 
#'             One can also set it as \code{"rmst difference"} (another Kaplan-Meier based test)
#' @param tau desired milestone for \code{test = "survival difference"} or \code{test = "rmst difference"}
#' 
#' @examples 
#' # set enrollment rates
#' enrollRates <- tibble::tibble(Stratum = "All", duration = 12, rate = 500/12)
#' 
#' # set failure rates
#' failRates <- tibble::tibble(
#'   Stratum = "All",
#'   duration = c(4, 100),
#'   failRate = log(2) / 15,  # median survival 15 month
#'   hr = c(1, .6),
#'   dropoutRate = 0.001)
#'
#' fixed_design_power_rmst(enrollRates, failRates, analysisTimes = 36)
#' fixed_design_power_rmst(enrollRates, failRates, analysisTimes = 36, tau = 18)
#' 
#' @noRd
fixed_design_power_rmst <- function(enrollRates, 
                                    failRates, 
                                    analysisTimes,
                                    ratio = 1,
                                    alpha = 0.025,
                                    test = "rmst difference",
                                    tau = NULL){
  
  gs_arm <- gs_create_arm(enrollRates, failRates, ratio = ratio,  total_time = analysisTimes) 
  arm0 <- gs_arm[["arm0"]]
  arm1 <- gs_arm[["arm1"]]
  
  n <- sum(enrollRates$duration * enrollRates$rate)
  n0 <- n / (ratio + 1)
  n1 <- n - n0
  arm0$size <- n0 
  arm1$size <- n1
  
  d <- prob_event.arm(arm0, tmax =  arm0$total_time) * n0 + prob_event.arm(arm1, tmax =  arm0$total_time) * n1
  
  # Sample size for RMST at cut point 
  npsurv <- npsurvSS::power_two_arm(arm0, arm1, 
                                    alpha = alpha, 
                                    test = list(test = test, milestone = if(is.null(tau)){arm0$total_time}else{tau})) 
  
  bounds <- tibble::tibble(
    Analysis = 1,
    Bound = "Upper",
    Probability = npsurv,
    Probability0 = alpha,
    Z = - qnorm(alpha))
  
  analysis <- tibble::tibble(
    Analysis = 1, 
    Time = analysisTimes,
    N = n, 
    Events = d)
  
  res <- list(enrollRates = enrollRates, 
              failRates = failRates, 
              bounds = bounds,
              analysis = analysis)
  
  res
}