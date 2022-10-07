#' Fixed design sample size
#'
#' Computes fixed design sample size for many sample size methods.
#' Returns a `tibble` with a basic summary
#' @param x Sample size method; default is \code{"AHR"}; 
#'          other options include \code{"FH"}, \code{"MB"}, \code{"LF"}, \code{"RD"}, \code{"MaxCombo"}, \code{"Milestone"}.
#' @param alpha One-sided Type I error (strictly between 0 and 1)
#' @param power Power (`NULL` to compute power or strictly between 0 and `1 - alpha` otherwise)
#' @param ratio Experimental:Control randomization ratio
#' @param studyDuration study duration
#' @param ... additional arguments like \code{enrollRates}, \code{failRates}, \code{rho}, \code{gamma}, \code{tau}
#' 
#' @return a table
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' # Average hazard ratio
#' x <- fixed_design("AHR", 
#'                   alpha = .025, power = .9, 
#'                   enrollRates = tibble::tibble(Stratum = "All",  duration = 18, rate = 1),
#'                   failRates = tibble::tibble(Stratum = "All", duration = c(4, 100), failRate = log(2) / 12, hr = c(1, .6), dropoutRate = .001),
#'                   studyDuration = 36)
#' x %>% summary()            
#' 
#' # Lachin and Foulkes (uses gsDesign::nSurv())
#' x <- fixed_design("LF", 
#'                   alpha = .025, power = .9, 
#'                   enrollRates = tibble::tibble(Stratum = "All",  duration = 18, rate = 1),
#'                   failRates = tibble::tibble(Stratum = "All", duration = 100, failRate = log(2) / 12, hr = .7, dropoutRate = .001),
#'                   studyDuration = 36)
#' x %>% summary()
#' 
#' # RMST
#' x <- fixed_design("RMST", alpha = .025, power = .9, 
#'                   enrollRates = tibble::tibble(Stratum = "All",  duration = 18, rate = 1),
#'                   failRates = tibble::tibble(Stratum = "All", duration = 100, failRate = log(2) / 12, hr = .7, dropoutRate = .001),
#'                   studyDuration = 36,
#'                   tau = 18)
#' x %>% summary()
#' 
#' # Milestone 
#' x <- fixed_design("Milestone", alpha = .025, power = .9, 
#'                   enrollRates = tibble::tibble(Stratum = "All",  duration = 18, rate = 1),
#'                   failRates = tibble::tibble(Stratum = "All", duration = 100, failRate = log(2) / 12, hr = .7, dropoutRate = .001),
#'                   studyDuration = 36,
#'                   tau = 18)
#' x %>% summary()
#' 
fixed_design <- function(x = c("AHR", "FH", "MB", "LF", "RD", "MaxCombo", "RMST", "Milestone"), 
                         alpha = 0.025, power = NULL, ratio = 1, studyDuration = 36, ...){
  # --------------------------------------------- #
  #     check inputs                              #
  # --------------------------------------------- #
  x <- match.arg(x)
  args <- list(...)
  
  has_weight <- "weight" %in% names(args)
  has_rho <- "rho" %in% names(args)
  has_gamma <- "gamma" %in% names(args)
  has_tau <- "tau" %in% names(args)
  has_enrollRates <- "enrollRates" %in% names(args)
  has_failRates <- "failRates" %in% names(args)
  has_N <- "N" %in% names(args)
  
  # ------------------------- #
  #     check inputs          #
  # ------------------------- #
  
  # check enrollment rate (not expected for RD)
  if(!has_enrollRates && x != "RD"){
    stop("fixed_design: please input enrollRates!")
  }else{
    enrollRates <- args$enrollRates
  }
  
  # check failure rate (not expected for RD)
  if(!has_failRates && x != "RD"){
    stop("fixed_design: please input failRates!")
  }else{
    failRates <- args$failRates
  }
  
  # check test parameters, like rho, gamma, tau 
  if(has_rho & length(args$rho) > 1 & x %in% c("FH", "MB")){
    stop("fixed_design: multiple rho can not be used in Fleming-Harrington or Magirr-Burman method!")
  }
  if(has_gamma & length(args$gamma) > 1 & x %in% c("FH", "MB")){
    stop("fixed_design: multiple gamma can not be used in Fleming-Harrington or Magirr-Burman method!")
  }
  if(has_tau & length(args$tau) > 1 & x %in% c("FH", "MB")){
    stop("fixed_design: multiple tau can not be used in Fleming-Harrington or Magirr-Burman method!")
  }
  if(has_tau & x == "FH"){
    stop("fixed_design: tau is not needed for Fleming-Harrington (FH) method!")
  }
  if(has_rho & has_gamma & x == "MB"){
    stop("fixed_design: rho and gamma are not needed for Magirr-Burman (MB) method!")
  }
  
  # check inputs necessary for RD
  if(x == "RD"){
    if(!"p_c" %in% names(args)){stop("fixed_design: p_c is needed for RD!")}
    if(!"p_e" %in% names(args)){stop("fixed_design: p_e is needed for RD!")}
    if(!"rd0" %in% names(args)){stop("fixed_design: rd0 is needed for RD!")}
    if(is.null(power) && !has_N){stop("fixed_design: sample size N = ... is needed for RD!")}
  }
  
  # ------------------------- #
  #     generate design       #
  # ------------------------- #
  y <- switch(x, 
              "AHR" = {
                if (!is.null(power)){
                  d <- gs_design_ahr(alpha = alpha, beta = 1 - power,
                                     upar = qnorm(1 - alpha), lpar = -Inf,
                                     enrollRates = enrollRates,
                                     failRates = failRates,
                                     ratio  = ratio, 
                                     analysisTimes = studyDuration)
                }else{
                  d <- gs_power_ahr(upar = qnorm(1 - alpha), lpar = -Inf,
                                    enrollRates = enrollRates,
                                    failRates = failRates,
                                    ratio  = ratio, 
                                    analysisTimes = studyDuration,
                                    events = NULL)
                }
                ans <- tibble::tibble(Design = "AHR",
                                      N = d$analysis$N,
                                      Events = d$analysis$Events,
                                      Time = d$analysis$Time,
                                      Bound = (d$bounds %>% filter(Bound == "Upper"))$Z,
                                      alpha = alpha,
                                      Power = (d$bounds %>% filter(Bound == "Upper"))$Probability)
                
                list(enrollRates = d$enrollRates, failRates = d$failRates, analysis = ans, design = "AHR")
              },
              
              "FH" = {
                
                if(has_weight + has_rho + has_gamma == 0){
                  weight <- function(x, arm0, arm1){wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 0.5)}
                }
                if(has_weight == 0 & has_rho + has_gamma >= 1){
                  weight <- function(x, arm0, arm1){wlr_weight_fh(x, arm0, arm1, 
                                                                  rho = ifelse(has_rho, args$rho, 0), 
                                                                  gamma = ifelse(has_gamma, args$gamma, 0.5))}
                }
                if (!is.null(power)){
                  d <- gs_design_wlr(alpha = alpha, beta = 1 - power,
                                     upar = qnorm(1 - alpha), lpar = -Inf,
                                     enrollRates = enrollRates, 
                                     failRates = failRates,
                                     ratio = ratio, 
                                     weight = weight,
                                     analysisTimes = studyDuration)
                }else{
                  d <- gs_power_wlr(upar = qnorm(1 - alpha), lpar = -Inf,
                                    enrollRates = enrollRates, 
                                    failRates = failRates,
                                    ratio = ratio, 
                                    weight = weight,
                                    analysisTimes = studyDuration,
                                    events = NULL)
                }
                ans <- tibble::tibble(Design = "FH",
                                      N = d$analysis$N,
                                      Events = d$analysis$Events,
                                      Time = d$analysis$Time,
                                      Bound = (d$bounds %>% filter(Bound == "Upper"))$Z,
                                      alpha = alpha,
                                      Power = (d$bounds %>% filter(Bound == "Upper"))$Probability)
                
                list(enrollRates = d$enrollRates, failRates = d$failRates, analysis = ans, 
                     design = "FH", design_par = list(rho = if(has_rho){args$rho}else{0}, 
                                                      gamma = if(has_gamma){args$gamma}else{0.5})
                )
              },
              
              
              "MB" = {
                # check if power is NULL or not
                if(!is.null(power)){
                  d <- gs_design_wlr(alpha = alpha,
                                     beta = 1 - power,
                                     enrollRates = enrollRates, 
                                     failRates = failRates,
                                     ratio = 1, 
                                     weight = function(x, arm0, arm1){wlr_weight_fh(x, arm0, arm1, rho = -1, gamma = 0,
                                                                                    tau = ifelse(has_tau, args$tau, 6))},
                                     upper = gs_b,
                                     upar = qnorm(1 - alpha),
                                     lower = gs_b,
                                     lpar = -Inf,
                                     analysisTimes = studyDuration) 
                }else{
                  d <- gs_power_wlr(enrollRates = enrollRates, 
                                    failRates = failRates,
                                    ratio = 1, 
                                    weight = function(x, arm0, arm1){wlr_weight_fh(x, arm0, arm1, rho = -1, gamma = 0,
                                                                                   tau = ifelse(has_tau, args$tau, 6))},
                                    upper = gs_b,
                                    upar = qnorm(1 - alpha),
                                    lower = gs_b,
                                    lpar = -Inf,
                                    analysisTimes = studyDuration,
                                    events = NULL) 
                }
                
                # get the output of MB
                ans <- tibble::tibble(Design = "MB",
                                      N = d$analysis$N,
                                      Events = d$analysis$Events,
                                      Time = d$analysis$Time,
                                      Bound = (d$bounds %>% filter(Bound == "Upper"))$Z,
                                      alpha = alpha,
                                      Power = (d$bounds %>% filter(Bound == "Upper"))$Probability)
                
                list(enrollRates = d$enrollRates, failRates = d$failRates, analysis = ans, 
                     design = "MB", design_par = list(tau = ifelse(has_tau, args$tau, 6)))
                
                
              },
              
              
              "LF" = { 
                # check if it is stratum
                if(length(unique(enrollRates$Stratum)) != 1 | length(unique(failRates$Stratum)) != 1){
                  warning("Lachin-Foulkes is not recommended for stratified designs!")
                }
                
                # calculate the S: duration of piecewise constant event rates 
                m <- length(failRates$failRate)
                if (m == 1){S <- NULL}else{S <- failRates$duration[1:(m-1)]}
                
                # calculate the ahr as the hr in nSurv
                dd <- gsDesign2::AHR(enrollRates = enrollRates, failRates = failRates, totalDuration = studyDuration, ratio = ratio)
                
                # use nSuve to develop the design
                d <- gsDesign::nSurv(alpha = alpha, beta = if(is.null(power)){NULL}else{1 - power}, 
                                     ratio = ratio, hr = dd$AHR,
                                     # failRates
                                     lambdaC = failRates$failRate,
                                     S = S, eta = failRates$dropoutRate,  
                                     # enrollRates
                                     gamma = enrollRates$rate, R = enrollRates$duration,
                                     T = studyDuration, minfup = studyDuration - sum(enrollRates$duration))
                
                ans <- tibble::tibble(Design = "LF",
                                      N = d$n,
                                      Events = d$d,
                                      Time = d$T,
                                      Bound = qnorm(1 - alpha),
                                      alpha = d$alpha,
                                      Power = d$power)
                
                list(enrollRates = enrollRates %>% mutate(rate = rate * d$n/sum(enrollRates$duration * enrollRates$rate)), 
                     failRates = failRates, 
                     analysis = ans, 
                     design = "LF")
              },
              
              
              "MaxCombo" = {
                # organize the tests in max combo
                max_combo_test <- data.frame(rho = if(has_rho){args$rho}else{c(0, 0)},
                                             gamma = if(has_gamma){args$gamma}else{c(0, 0.5)},
                                             tau = if(has_tau){args$tau}else{-1}) %>% 
                  mutate(test = seq(1, length(rho)), Analysis = 1, analysisTimes = studyDuration)
                
                # check if power is NULL or not
                if(!is.null(power)){
                  d <- gs_design_combo(alpha = alpha, beta = 1 - power, ratio = ratio, 
                                       enrollRates = enrollRates, 
                                       failRates = failRates,
                                       fh_test = max_combo_test, 
                                       upper = gs_b, upar = qnorm(1 - alpha),
                                       lower = gs_b, lpar = -Inf) 
                }else{
                  d <- gs_power_combo(ratio = ratio,
                                      enrollRates = enrollRates,  
                                      failRates = failRates,  
                                      fh_test = max_combo_test, 
                                      upper = gs_b, upar = qnorm(1 - alpha),
                                      lower = gs_b, lpar = -Inf) 
                }
                
                # get the output of max combo
                ans <- tibble::tibble(Design = "MaxCombo",
                                      N = d$analysis$N,
                                      Events = d$analysis$Events,
                                      Time = d$analysis$Time,
                                      Bound = (d$bounds %>% filter(Bound == "Upper"))$Z,
                                      alpha = alpha,
                                      Power = (d$bounds %>% filter(Bound == "Upper"))$Probability)
                
                list(enrollRates = d$enrollRates, failRates = d$failRates, analysis = ans, 
                     design = "MaxCombo", design_par = list(rho = if(has_rho){args$rho}else{c(0, 0)},
                                                            gamma = if(has_gamma){args$gamma}else{c(0, 0.5)},
                                                            tau = if(has_tau){args$tau}else{c(-1, -1)}))
              },
              
              "RD" = {
                if(!is.null(power)){
                  d <- gs_design_rd(p_c = tibble::tibble(Stratum = "All", Rate = args$p_c),
                                    p_e = tibble::tibble(Stratum = "All", Rate = args$p_e),
                                    alpha = alpha, beta = 1 - power, ratio = ratio,
                                    upper = gs_b, upar = qnorm(1 - alpha),
                                    lower = gs_b, lpar = -Inf,
                                    rd0 = args$rd0, weight = "un-stratified") 
                }else{
                  d <- gs_power_rd(p_c = tibble::tibble(Stratum = "All", Rate = args$p_c),
                                   p_e = tibble::tibble(Stratum = "All", Rate = args$p_e),
                                   ratio = ratio,
                                   upper = gs_b, upar = qnorm(1 - alpha),
                                   lower = gs_b, lpar = -Inf,
                                   N = tibble::tibble(Stratum = "All", N = args$N, Analysis = 1),
                                   rd0 = args$rd0,  weight = "un-stratified") 
                }
                
                # get the output of max combo
                ans <- tibble::tibble(Design = "RD",
                                      N = d$analysis$N,
                                      Bound = (d$bounds %>% filter(Bound == "Upper"))$Z,
                                      alpha = alpha,
                                      Power = (d$bounds %>% filter(Bound == "Upper"))$Probability)
                
                list(enrollRates = d$enrollRates, failRates = d$failRates, analysis = ans, design = "RD")
              },
              
              
              "RMST" = {
                if(!is.null(power)){
                  d <- fixed_design_size_rmst(alpha = alpha, beta = 1 - power, ratio = ratio, 
                                              enrollRates = enrollRates, failRates = failRates,
                                              analysisTimes = studyDuration, 
                                              test = "rmst difference",
                                              tau = ifelse(has_tau, args$tau, studyDuration)) 
                }else{
                  d <- fixed_design_power_rmst(alpha = alpha, ratio = ratio,
                                               enrollRates = enrollRates, failRates = failRates,  
                                               analysisTimes = studyDuration, 
                                               test = "rmst difference",
                                               tau = ifelse(has_tau, args$tau, studyDuration)) 
                }
                
                # get the output 
                ans <- tibble::tibble(Design = "RMST",
                                      N = d$analysis$N,
                                      Events = d$analysis$Events,
                                      Time = d$analysis$Time,
                                      Bound = (d$bounds %>% filter(Bound == "Upper"))$Z,
                                      alpha = alpha,
                                      Power = (d$bounds %>% filter(Bound == "Upper"))$Probability)
                
                list(enrollRates = d$enrollRates, failRates = d$failRates, analysis = ans, 
                     design = "RMST", design_par = list(tau = ifelse(has_tau, args$tau, studyDuration)))
              },
              
              "Milestone" = {
                if(!is.null(power)){
                  d <- fixed_design_size_rmst(alpha = alpha, beta = 1 - power, ratio = ratio, 
                                              enrollRates = enrollRates, failRates = failRates,
                                              analysisTimes = studyDuration, 
                                              test = "survival difference",
                                              tau = ifelse(has_tau, args$tau, studyDuration)) 
                }else{
                  d <- fixed_design_power_rmst(alpha = alpha, ratio = ratio,
                                               enrollRates = enrollRates, failRates = failRates,
                                               analysisTimes = studyDuration, 
                                               test = "survival difference",
                                               tau = ifelse(has_tau, args$tau, studyDuration)) 
                }
                
                # get the output of max combo
                ans <- tibble::tibble(Design = "Milestone",
                                      N = d$analysis$N,
                                      Events = d$analysis$Events,
                                      Time = d$analysis$Time,
                                      Bound = (d$bounds %>% filter(Bound == "Upper"))$Z,
                                      alpha = alpha,
                                      Power = (d$bounds %>% filter(Bound == "Upper"))$Probability)
                
                list(enrollRates = d$enrollRates, failRates = d$failRates, analysis = ans, 
                     design = "Milestone", design_par = list(tau = ifelse(has_tau, args$tau, studyDuration)))
              }
  )
  
  class(y) <- c("fixed_design", class(y))
  return(y)    
}

