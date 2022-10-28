#  Copyright (c) 2022 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co., Inc., Rahway, NJ, USA.
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


#' Information and effect size for Weighted Log-rank test
#'
#' Based on piecewise enrollment rate, failure rate, and dropout rates computes
#' approximate information and effect size using an average hazard ratio model.
#' @param enrollRates enrollment rates
#' @param failRates failure and dropout rates
#' @param ratio Experimental:Control randomization ratio
#' @param events Targeted minimum events at each analysis
#' @param analysisTimes Targeted minimum study duration at each analysis
#' @param weight weight of weighted log rank test
#' - `"1"`= unweighted,
#' - `"n"`= Gehan-Breslow,
#' - `"sqrtN"`= Tarone-Ware,
#' - `"FH_p[a]_q[b]"`= Fleming-Harrington with p=a and q=b
#' @param approx approximate estimation method for Z statistics
#' - `"event driven"` = only work under proportional hazard model with log rank test
#' - `"asymptotic"`
#'
#' @return a \code{tibble} with columns \code{Analysis, Time, N, Events, AHR, delta, sigma2, theta, info, info0.}
#' \code{info, info0} contains statistical information under H1, H0, respectively.
#' For analysis \code{k}, \code{Time[k]} is the maximum of \code{analysisTimes[k]} and the expected time
#' required to accrue the targeted \code{events[k]}.
#' \code{AHR} is expected average hazard ratio at each analysis.
#' 
#' @details The \code{AHR()} function computes statistical information at targeted event times.
#' The \code{tEvents()} function is used to get events and average HR at targeted \code{analysisTimes}.
#' 
#' @export
#' 
gs_info_wlr <- function(enrollRates=tibble::tibble(Stratum="All",
                                                   duration=c(2,2,10),
                                                   rate=c(3,6,9)),
                        failRates=tibble::tibble(Stratum="All",
                                                 duration=c(3,100),
                                                 failRate=log(2)/c(9,18),
                                                 hr=c(.9,.6),
                                                 dropoutRate=rep(.001,2)),
                        ratio=1,                # Experimental:Control randomization ratio
                        events = NULL, # Events at analyses
                        analysisTimes = NULL,   # Times of analyses
                        weight = wlr_weight_fh,
                        approx = "asymptotic"
){
  
  if (is.null(analysisTimes) && is.null(events)){
    stop("gs_info_wlr(): One of events and analysisTimes must be a numeric value or vector with increasing values!")
  }
  
  # Obtain Analysis time
  avehr <- NULL
  if(!is.null(analysisTimes)){
    avehr <- AHR(enrollRates = enrollRates, failRates = failRates, ratio = ratio,
                 totalDuration = analysisTimes)
    for(i in seq_along(events)){
      if (avehr$Events[i] < events[i]){
        avehr[i,] <- tEvents(enrollRates = enrollRates, failRates = failRates, ratio = ratio,
                             targetEvents = events[i])
      }
    }
  }else{
    for(i in seq_along(events)){
      avehr <- rbind(avehr,
                     tEvents(enrollRates = enrollRates, failRates = failRates, ratio = ratio,
                             targetEvents = events[i]))
    }
  }
  
  time <- avehr$Time
  
  # Create Arm object
  gs_arm <- gs_create_arm(enrollRates, failRates, ratio)
  
  arm0 <- gs_arm$arm0
  arm1 <- gs_arm$arm1
  
  
  
  # Randomization ratio
  p0 <- arm0$size/(arm0$size + arm1$size)
  p1 <- 1 - p0
  
  # Null arm
  arm_null <- arm0
  arm_null$surv_scale <- p0* arm0$surv_scale + p1 * arm1$surv_scale
  
  arm_null1 <- arm_null
  arm_null1$size <- arm1$size
  
  delta <- c()     # delta of effect size in each analysis
  sigma2_h1 <- c()    # sigma square of effect size in each analysis under null
  sigma2_h0 <- c()    # sigma square of effect size in each analysis under alternative
  p_event <- c()   # probability of events in each analysis
  p_subject <- c() # probability of subjects enrolled
  num_log_ahr <- c()
  dem_log_ahr <- c()
  
  # Used to calculate average hazard ratio
  arm01 <- arm0; arm01$size <- 1
  arm11 <- arm1; arm11$size <- 1
  
  for(i in seq_along(time)){
    t <- time[i]
    p_event[i]      <- p0 * prob_event.arm(arm0, tmax = t) + p1 * prob_event.arm(arm1, tmax = t)
    p_subject[i]    <- p0 * npsurvSS::paccr(t, arm0) + p1 * npsurvSS::paccr(t, arm1)
    delta[i]        <- gs_delta_wlr(arm0, arm1, tmax = t, weight = weight, approx = approx)
    num_log_ahr[i] <- gs_delta_wlr(arm01, arm11, tmax = t, weight = weight, approx = approx)
    dem_log_ahr[i] <- gs_delta_wlr(arm01, arm11, tmax = t, weight = weight,
                                   approx = "generalized schoenfeld", normalization = TRUE)
    
    sigma2_h1[i]    <- gs_sigma2_wlr(arm0, arm1, tmax = t, weight = weight, approx = approx)
    sigma2_h0[i]    <- gs_sigma2_wlr(arm_null, arm_null1, tmax = t, weight = weight, approx = approx)
  }
  
  N <- tail(avehr$Events / p_event,1) * p_subject
  theta <- (- delta) / sigma2_h1
  data.frame(Analysis = 1:length(time),
             Time = time,
             N = N,
             Events = avehr$Events,
             AHR = exp(num_log_ahr/dem_log_ahr),
             delta = delta,
             sigma2 = sigma2_h1,
             theta = theta,
             info =  sigma2_h1 * N,
             info0 = sigma2_h0 * N)
  
}
