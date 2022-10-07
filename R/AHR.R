#  Copyright (c) 2021 Merck Sharp & Dohme Corp., a subsidiary of
#  Merck & Co., Inc., Kenilworth, NJ, USA.
#
#  This file is part of the gsDesign2 program.
#
#  gsDesign2 is free software: you can redistribute it and/or modify
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

#' @importFrom dplyr filter mutate transmute full_join group_by summarize ungroup first "%>%"
#' @importFrom tibble tibble
NULL

#' Average hazard ratio under non-proportional hazards (test version)
#'
#' \code{AHR()} provides a geometric average hazard ratio under
#' various non-proportional hazards assumptions for either single or multiple strata studies.
#' The piecewise exponential distribution allows a simple method to specify a distribution
#' and enrollment pattern where the enrollment, failure and dropout rates changes over time.
#' @param enrollRates Piecewise constant enrollment rates by stratum and time period.
#' @param failRates Piecewise constant control group failure rates, duration for each piecewise constant period,
#' hazard ratio for experimental vs control, and dropout rates by stratum and time period.
#' @param totalDuration Total follow-up from start of enrollment to data cutoff;
#' this can be a single value or a vector of positive numbers.
#' @param ratio ratio of experimental to control randomization.
#' @param simple logical; if TRUE (default), for each value in input totalDuration overall event count,
#' statistical information and average hazard ratio are given;
#' if FALSE, hazard ratio, expected events and statistical information are produced by stratum and underlying hazard ratio.
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input enrollment rate contains stratum column.
#'    \item Validate if input enrollment rate contains total duration column.
#'    \item Validate if input enrollment rate contains rate column.
#'    \item Validate if input failure rate contains stratum column.
#'    \item Validate if input failure rate contains duration column.
#'    \item Validate if input failure rate contains failure rate column.
#'    \item Validate if input failure rate contains hazard ratio column.
#'    \item Validate if input failure rate contains dropout rate column.
#'    \item Validate if input trial total follow-up (total duration) is a non-empty vector of positive integers.
#'    \item Validate if strata is the same in enrollment rate and failure rate.
#'    \item Validate if input simple is logical.
#'    \item Compute the proportion in each group.
#'    \item Compute the expected events by treatment groups, stratum and time period.
#'    \item Calculate the expected number of events for all time points in the total
#'     duration and for all stratification variables.
#'    \itemize{
#'      \item Compute the expected events in for each strata.
#'        \itemize{
#'          \item Combine the expected number of events of all stratification variables.
#'          \item Recompute events, hazard ratio and information under the given scenario of the combined data for each strata.
#'          }
#'        \item Combine the results for all time points by summarizing the results by adding up the number of events,
#'       information under the null and the given scenarios.
#'       }
#'    \item Return a tibble of overall event count, statistical information and average hazard ratio
#'    of each value in totalDuration if the input simple is true, or a tibble of hazard ratio,
#'    expected events and statistical information  produced by stratum and
#'    underlying hazard ratio if the input simple is false.
#'    \item Calculation of \code{AHR} for different design scenarios, and the comparison to the
#'    simulation studies are defined in vignette/AHRVignette.Rmd.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @return A `tibble` with `Time` (from `totalDuration`), `AHR` (average hazard ratio),
#' `Events` (expected number of events), info (information under given scenarios),
#' and info0 (information under related null hypothesis) for each value of `totalDuration` input;
#' if `simple=FALSE`, `Stratum` and `t` (beginning of each constant HR period) are also returned
#' and `HR` is returned instead of `AHR`
#' 
#' @examples
#' # Example: default
#' AHR()
#' 
#' # Example: default with multiple analysis times (varying totalDuration)
#' 
#' AHR(totalDuration = c(15, 30))
#' 
#' # Stratified population
#' enrollRates <- tibble::tibble(Stratum = c(rep("Low", 2), rep("High", 3)),
#'                               duration = c(2, 10, 4, 4, 8),
#'                               rate = c(5, 10, 0, 3, 6))
#' failRates <- tibble::tibble(Stratum = c(rep("Low", 2), rep("High", 2)),
#'                             duration = 1,
#'                             failRate = c(.1, .2, .3, .4),
#'                             hr = c(.9, .75, .8, .6),
#'                             dropoutRate = .001)
#' AHR(enrollRates = enrollRates, failRates = failRates, totalDuration = c(15, 30))
#' 
#' # Same example, give results by strata and time period
#' AHR(enrollRates = enrollRates, failRates = failRates, totalDuration = c(15, 30), simple = FALSE)
#' 
#' @export
#'
AHR <- function(enrollRates = tibble::tibble(Stratum = "All",
                                             duration = c(2, 2, 10),
                                             rate = c(3, 6, 9)),
                failRates = tibble::tibble(Stratum = "All",
                                           duration = c(3, 100),
                                           failRate = log(2) / c(9, 18),
                                           hr = c(.9, .6),
                                           dropoutRate = rep(.001, 2)),
                totalDuration = 30,
                ratio = 1,
                simple = TRUE
){
  # ----------------------------#
  #    check input values       #
  # ----------------------------#
  check_enrollRates(enrollRates)
  check_failRates(failRates)
  check_enrollRates_failRates(enrollRates, failRates)
  check_totalDuration(totalDuration)
  check_ratio(ratio)
  if(!is.logical(simple)){stop("gsDesign2: simple in `AHR()` must be logical")}
  
  # compute proportion in each group
  Qe <- ratio / (1 + ratio)
  Qc <- 1 - Qe
  
  # compute expected events by treatment group, stratum and time period
  ans <- NULL
  strata <- unique(enrollRates$Stratum)
  
  for(td in totalDuration){
    
    events <- NULL
    
    for(s in strata){
      # subset to stratum
      enroll <- enrollRates %>% filter(Stratum == s)
      fail <- failRates %>% filter(Stratum == s)
      
      # update enrollment rates
      enroll_c <- enroll %>% mutate(rate = rate * Qc)
      enroll_e <- enroll %>% mutate(rate = rate * Qe)
      
      # update failure rates
      fail_c <- fail
      fail_e <- fail %>% mutate(failRate = failRate * hr)
      
      # compute expected number of events
      events_c <- eEvents_df(enrollRates = enroll_c, failRates = fail_c, totalDuration = td, simple = FALSE)
      events_e <- eEvents_df(enrollRates = enroll_e, failRates = fail_e, totalDuration = td, simple = FALSE)
      
      # Combine control and experimental; by period recompute HR, events, information
      events <- rbind(events_c %>% mutate(Treatment = "Control"),
                      events_e %>% mutate(Treatment = "Experimental")) %>%
                arrange(t, Treatment) %>% 
                ungroup() %>%
                # recompute HR, events, info by period
                group_by(t) %>%
                summarize(Stratum = s, 
                          info = (sum(1 / Events))^(-1),
                          Events = sum(Events), 
                          HR = last(failRate) / first(failRate)) %>%
                rbind(events)
    }
    
    # summarize events in one stratum
    ans_new <- events %>%
               mutate(Time = td, 
                      lnhr = log(HR), 
                      info0 = Events * Qc * Qe) %>%
               ungroup() %>% 
               # pool strata together for each time period
               group_by(Time, Stratum, HR) %>%
               summarize(t = min(t), 
                         Events = sum(Events),
                         info0 = sum(info0), 
                         info = sum(info))
    ans <- rbind(ans, ans_new)
  }
  
  # output the results
  if(!simple){
    ans <- ans %>% 
           select(Time, Stratum, t, HR, Events, info, info0) %>%
           group_by(Time, Stratum) %>% 
           arrange(t, .by_group = TRUE) %>% 
           ungroup()
  }else{
    ans <- ans %>%
           group_by(Time) %>%
           summarize(AHR = exp(sum(log(HR) * Events) / sum(Events)),
                     Events = sum(Events),
                     info = sum(info),
                     info0 = sum(info0)) %>%
           ungroup()
      
  }
  return(ans)
}