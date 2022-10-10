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


#' For a subject in the provided arm, calculate the probability he or
#' she is observed to be at risk at time=teval after enrollment.
#' @noRd
prob_risk <- function(arm, teval, tmax) {
  if(is.null(tmax)){
    tmax <- arm$total_time
  }
  
  npsurvSS::psurv(teval, arm, lower.tail = F) *
    npsurvSS::ploss(teval, arm, lower.tail = F) *
    npsurvSS::paccr(pmin(arm$accr_time, tmax - teval), arm)
}

#' For a subject in the provided arm, calculate the density of event
#' at time=teval after enrollment.
#' @noRd
dens_event <- function(arm, teval, tmax = NULL) {
  
  if(is.null(tmax)){
    tmax <- arm$total_time
  }
  
  npsurvSS::dsurv(teval, arm) *
    npsurvSS::ploss(teval, arm, lower.tail = F) *
    npsurvSS::paccr(pmin(arm$accr_time, tmax - teval), arm)
}

#' For a subject in the provided arm, calculate the probability he or
#' she is observed to have experienced an event by time=teval after enrollment.
#' @noRd
prob_event <- function(arm, tmin = 0, tmax = arm$total_time) {
  UseMethod("prob_event", arm)
}

#' prob_event for arm of class "arm"
#' @noRd
prob_event.arm <- function(arm, tmin = 0, tmax = arm$total_time) {
  l = length(tmax)
  if (l == 1) {
    return(stats::integrate(function(x) dens_event(arm, x, tmax = tmax), lower = tmin, upper = tmax)$value)
  } else {
    if (length(tmin) == 1) {
      tmin = rep(tmin, l)
    }
    return(sapply(seq(l), function(i) prob_event(arm, tmin[i], tmax[i])))
  }
}

#' @noRd
gs_delta_wlr <- function(arm0,
                         arm1,
                         tmax = NULL,
                         weight= wlr_weight_fh,
                         approx="asymptotic",
                         normalization = FALSE) {
  
  if(is.null(tmax)){
    tmax <- arm0$total_time
  }
  
  p1 <- arm1$size / (arm0$size + arm1$size)
  p0 <- 1 - p1
  
  if (approx == "event driven") {
    
    if (sum(arm0$surv_shape != arm1$surv_shape) > 0 |
        length( unique(arm1$surv_scale / arm0$surv_scale) ) > 1) {
      
      stop("gs_delta_wlr(): Hazard is not proportional over time.", call. = F)
      
    } else if (wlr_weight_fh(seq(0,tmax,length.out = 10), arm0, arm1) != "1") {
      
      stop("gs_delta_wlr(): Weight must equal `1`.", call. = F)
    }
    
    theta <- c(arm0$surv_shape * log( arm1$surv_scale / arm0$surv_scale ))[1]        # log hazard ratio
    nu    <- p0 * prob_event(arm0, tmax = tmax) + p1 * prob_event(arm1, tmax = tmax) # probability of event
    delta <- theta * p0 * p1 * nu
    
  } else if (approx == "asymptotic") {
    
    delta <- stats::integrate(function(x){
      
      term0 <- p0 * prob_risk(arm0, x, tmax)
      term1 <- p1 * prob_risk(arm1, x, tmax)
      term  <- (term0 * term1) / (term0 + term1)
      term  <- ifelse(is.na(term), 0, term)
      weight(x, arm0, arm1) *  term * ( npsurvSS::hsurv(x, arm1) - npsurvSS::hsurv(x, arm0) )},
      lower = 0,
      upper = tmax, rel.tol = 1e-5)$value
    
    
  } else if (approx == "generalized schoenfeld") {
    
    delta <- stats::integrate(function(x){
      
      if(normalization){
        log_hr_ratio <- 1
      }else{
        log_hr_ratio <- log( npsurvSS::hsurv(x, arm1) / npsurvSS::hsurv(x, arm0) )
      }
      
      weight(x, arm0, arm1) *
        log_hr_ratio *
        p0 * prob_risk(arm0, x, tmax) * p1 * prob_risk(arm1, x, tmax) /
        ( p0 * prob_risk(arm0, x, tmax) + p1 * prob_risk(arm1, x, tmax) )^2 *
        ( p0 * dens_event(arm0, x, tmax) + p1 * dens_event(arm1, x, tmax))},
      lower = 0,
      upper = tmax)$value
  } else {
    
    stop("gs_delta_wlr(): Please specify a valid approximation for the mean.", call. = F)
    
  }
  
  return(delta)
  
}

#' @noRd
gs_sigma2_wlr <- function(arm0,
                          arm1,
                          tmax = NULL,
                          weight= wlr_weight_fh,
                          approx="asymptotic") {
  
  if(is.null(tmax)){
    tmax <- arm0$total_time
  }
  
  p1 <- arm1$size / (arm0$size + arm1$size)
  p0 <- 1 - p1
  
  if (approx == "event driven") {
    
    nu      <- p0 * prob_event(arm0, tmax = tmax) + p1 * prob_event(arm1, tmax = tmax)
    sigma2  <- p0 * p1 * nu
    
  } else if (approx %in% c("asymptotic", "generalized schoenfeld")) {
    
    sigma2  <- stats::integrate(function(x) weight(x, arm0, arm1)^2 *
                                  p0 * prob_risk(arm0, x, tmax) * p1 * prob_risk(arm1, x, tmax) /
                                  ( p0 * prob_risk(arm0, x, tmax) + p1 * prob_risk(arm1, x, tmax) )^2 *
                                  ( p0 * dens_event(arm0, x, tmax) + p1 * dens_event(arm1, x, tmax)),
                                lower = 0,
                                upper= tmax)$value
    
  } else {
    stop("gs_sigma2_wlr(): Please specify a valid approximation for the mean.", call. = F)
  }
  
  return(sigma2)
  
}
