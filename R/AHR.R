#  Copyright (c) 2022 Merck Sharp & Dohme Corp., a subsidiary of
#  Merck & Co., Inc., Rahway, NJ, USA.
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
#' @param enroll_rate Piecewise constant enrollment rates by stratum and time period.
#' @param fail_rate Piecewise constant control group failure rates, duration for each piecewise constant period,
#' hazard ratio for experimental vs control, and dropout rates by stratum and time period.
#' @param total_duration Total follow-up from start of enrollment to data cutoff;
#' this can be a single value or a vector of positive numbers.
#' @param ratio ratio of experimental to control randomization.
#' @param simple logical; if TRUE (default), for each value in input total_duration overall event count,
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
#'    of each value in total_duration if the input simple is true, or a tibble of hazard ratio,
#'    expected events and statistical information  produced by stratum and
#'    underlying hazard ratio if the input simple is false.
#'    \item Calculation of \code{AHR} for different design scenarios, and the comparison to the
#'    simulation studies are defined in vignette/AHRVignette.Rmd.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @return A `tibble` with `Time` (from `total_duration`), `AHR` (average hazard ratio),
#' `Events` (expected number of events), info (information under given scenarios),
#' and info0 (information under related null hypothesis) for each value of `total_duration` input;
#' if `simple=FALSE`, `Stratum` and `t` (beginning of each constant HR period) are also returned
#' and `HR` is returned instead of `AHR`
#' 
#' @examples
#' # Example: default
#' AHR()
#' 
#' # Example: default with multiple analysis times (varying total_duration)
#' 
#' AHR(total_duration = c(15, 30))
#' 
#' # Stratified population
#' enroll_rate <- tibble::tibble(Stratum = c(rep("Low", 2), rep("High", 3)),
#'                               duration = c(2, 10, 4, 4, 8),
#'                               rate = c(5, 10, 0, 3, 6))
#' fail_rate <- tibble::tibble(Stratum = c(rep("Low", 2), rep("High", 2)),
#'                             duration = 1,
#'                             fail_rate = c(.1, .2, .3, .4),
#'                             hr = c(.9, .75, .8, .6),
#'                             dropout_rate = .001)
#' AHR(enroll_rate = enroll_rate, fail_rate = fail_rate, total_duration = c(15, 30))
#' 
#' # Same example, give results by strata and time period
#' AHR(enroll_rate = enroll_rate, fail_rate = fail_rate, total_duration = c(15, 30), simple = FALSE)
#' 
#' @export
#'
AHR <- function(enroll_rate = tibble::tibble(Stratum = "All",
                                             duration = c(2, 2, 10),
                                             rate = c(3, 6, 9)),
                fail_rate = tibble::tibble(Stratum = "All",
                                           duration = c(3, 100),
                                           fail_rate = log(2) / c(9, 18),
                                           hr = c(.9, .6),
                                           dropout_rate = rep(.001, 2)),
                total_duration = 30,
                ratio = 1,
                simple = TRUE
){
  # ----------------------------#
  #    check input values       #
  # ----------------------------#
  check_enroll_rate(enroll_rate)
  check_fail_rate(fail_rate)
  check_enroll_rate_fail_rate(enroll_rate, fail_rate)
  check_total_duration(total_duration)
  check_ratio(ratio)
  if(!is.logical(simple)){stop("gsDesign2: simple in `AHR()` must be logical")}
  
  # compute proportion in each group
  Qe <- ratio / (1 + ratio)
  Qc <- 1 - Qe
  
  # compute expected events by treatment group, stratum and time period
  ans <- NULL
  strata <- unique(enroll_rate$Stratum)
  
  for(td in total_duration){
    
    events <- NULL
    
    for(s in strata){
      # subset to stratum
      enroll <- enroll_rate %>% filter(Stratum == s)
      fail <- fail_rate %>% filter(Stratum == s)
      
      # update enrollment rates
      enroll_c <- enroll %>% mutate(rate = rate * Qc)
      enroll_e <- enroll %>% mutate(rate = rate * Qe)
      
      # update failure rates
      fail_c <- fail
      fail_e <- fail %>% mutate(fail_rate = fail_rate * hr)
      
      # compute expected number of events
      events_c <- expected_event(enroll_rate = enroll_c, fail_rate = fail_c, total_duration = td, simple = FALSE)
      events_e <- expected_event(enroll_rate = enroll_e, fail_rate = fail_e, total_duration = td, simple = FALSE)
      
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