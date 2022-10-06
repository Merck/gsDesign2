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

#' @importFrom dplyr filter select full_join mutate transmute group_by ungroup summarize arrange desc lag last lead "%>%"
#' @importFrom tibble tibble
#' @importFrom stats stepfun
NULL

#' Expected events observed under piecewise exponential model
#'
#' \code{eEvents_df} computes expected events over time and by strata
#' under the assumption of piecewise constant enrollment rates and piecewise
#' exponential failure and censoring rates.
#' The piecewise exponential distribution allows a simple method to specify a distribtuion
#' and enrollment pattern
#' where the enrollment, failure and dropout rates changes over time.
#' While the main purpose may be to generate a trial that can be analyzed at a single point in time or
#' using group sequential methods, the routine can also be used to simulate an adaptive trial design.
#' The intent is to enable sample size calculations under non-proportional hazards assumptions
#' for stratified populations.
#'
#' @param enrollRates Enrollment rates; see details and examples
#' @param failRates Failure rates and dropout rates by period
#' @param totalDuration Total follow-up from start of enrollment to data cutoff
#' @param simple If default (TRUE), return numeric expected number of events, otherwise
#' a \code{tibble} as described below.
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input enrollment rate contains total duration column.
#'    \item Validate if input enrollment rate contains rate column.
#'    \item Validate if input failure rate contains duration column.
#'    \item Validate if input failure rate contains failure rate column.
#'    \item Validate if input failure rate contains dropout rate column.
#'    \item Validate if input trial total follow-up (total duration) is a non-empty vector of positive integers.
#'    \item Validate if input simple is logical.
#'    \item Define a tibble with the start opening for enrollment at zero and cumulative duration.
#'    Add the event (or failure) time corresponding to the start of the enrollment. Finally, add the enrollment rate to the tibble
#'    corresponding to the start and end (failure) time. This will be recursively used to calculate the expected
#'    number of events later. For details, see vignette/eEventsTheory.Rmd
#'    \item Define a tibble including the cumulative duration of failure rates, the corresponding start time of
#'    the enrollment, failure rate and dropout rates.  For details, see vignette/eEventsTheory.Rmd
#'    \item Only consider the failure rates in the interval of the end failure rate and total duration.
#'    \item Compute the failure rates over time using \code{stepfun} which is used
#'     to group rows by periods defined by failRates.
#'    \item Compute the dropout rate over time using \code{stepfun}.
#'    \item Compute the enrollment rate over time using \code{stepfun}. Details are
#'    available in vignette/eEventsTheory.Rmd.
#'    \item Compute expected events by interval at risk using the notations and descriptions in
#'    vignette/eEventsTheory.Rmd.
#'    \item Return \code{eEvents_df}
#'  }
#' }
#' @return
#' The default when \code{simple=TRUE} is to return the total expected number of events as a real number.
#' Otherwise, when \code{simple=FALSE} a \code{tibble} is returned with the following variables for each period specified in 'failRates':
#' \code{t} start of period,
#' \code{failRate} failure rate during the period
#' \code{Events} expected events during the period,
#'
#' The records in the returned \code{tibble} correspond to the input \code{tibble} \code{failRates}.
#' 
#' @details
#' More periods will generally be supplied in output than those that are input.
#' The intent is to enable expected event calculations in a tidy format to
#' maximize flexibility for a variety of purposes.
#' 
#' @examples
#' library(tibble)
#' library(gsDesign2)
#' 
#' # Default arguments, simple output (total event count only)
#' eEvents_df()
#' 
#' # Event count by time period
#' eEvents_df(simple = FALSE)
#' 
#' # Early cutoff
#' eEvents_df(totalDuration = .5)
#' 
#' # Single time period example
#' eEvents_df(enrollRates = tibble(duration = 10,rate = 10),
#'            failRates = tibble(duration=100, failRate = log(2) / 6 ,dropoutRate = .01),
#'            totalDuration = 22,
#'            simple = FALSE)
#' 
#' # Single time period example, multiple enrollment periods
#' eEvents_df(enrollRates = tibble(duration = c(5,5), rate = c(10, 20)),
#'            failRates = tibble(duration = 100, failRate = log(2)/6, dropoutRate = .01),
#'            totalDuration = 22, simple = FALSE)
#' @export
eEvents_df <- function(enrollRates = tibble::tibble(duration = c(2, 2, 10),
                                                    rate = c(3, 6, 9)),
                       failRates = tibble::tibble(duration = c(3, 100),
                                                  failRate = log(2) / c(9, 18),
                                                  dropoutRate = rep(.001, 2)),
                       totalDuration = 25,
                       simple = TRUE
){
  # ----------------------------#
  #    check input values       #
  # ----------------------------#
  check_enrollRates(enrollRates)
  check_failRates(failRates)
  check_enrollRates_failRates(enrollRates, failRates)
  check_totalDuration(totalDuration)
  if(length(totalDuration) > 1){stop("gsDesign2: totalDuration in `events_df()` must be a numeric number!")}
  if(!is.logical(simple)){stop("gsDesign2: simple in `eEvents_df()` must be logical")}
  
  # ----------------------------#
  #    divide the time line     #
  #     into sub-intervals      #
  # ----------------------------#
  ## by piecewise enrollment rates 
  df_1 <- tibble::tibble(startEnroll = c(0, cumsum(enrollRates$duration)),
                         endFail = totalDuration - startEnroll
                         #rate = c(enrollRates$rate, 0)
                         ) %>% subset(endFail > 0)
  ## by piecewise failure & dropout rates 
  df_2 <- tibble::tibble(endFail = cumsum(failRates$duration),
                         startEnroll = totalDuration - endFail,
                         failRate = failRates$failRate,
                         dropoutRate = failRates$dropoutRate)
  temp <- cumsum(failRates$duration)
  if(temp[length(temp)] < totalDuration){
    df_2 <- df_2[-nrow(df_2), ]
  }else{
    df_2 <- df_2[df_2$startEnroll > 0, ]
  }

  # ----------------------------#
  # create 3 step functions (sf)#
  # ----------------------------#
  # Step function to define enrollment rates over time
  sf.enrollRate <- stepfun(c(0, cumsum(enrollRates$duration)),
                           c(0, enrollRates$rate,0),
                           right = FALSE)
  # step function to define failure rates over time
  startFail <- c(0, cumsum(failRates$duration))
  sf.failRate <- stepfun(startFail,
                         c(0, failRates$failRate, last(failRates$failRate)),
                         right = FALSE)
  # step function to define dropout rates over time
  sf.dropoutRate <- stepfun(startFail,
                            c(0, failRates$dropoutRate, last(failRates$dropoutRate)),
                            right = FALSE)

  # ----------------------------#
  #    combine sub-intervals    #
  #          from               #
  #  enroll + failure + dropout #
  # ----------------------------#
  # impute the NA by step functions
  df <- full_join(df_1, df_2, by = c("startEnroll", "endFail")) %>%
    arrange(endFail) %>%
    mutate(endEnroll = lag(startEnroll, default = as.numeric(totalDuration)),
           startFail = lag(endFail, default = 0),
           duration = endEnroll - startEnroll,
           failRate = sf.failRate(startFail),
           dropoutRate = sf.dropoutRate(startFail),
           enrollRate = sf.enrollRate(startEnroll)) %>% 
    # create 2 auxiliary variable for failure & dropout rate
    # q: number of expected events in a sub-interval
    # Q: cumulative product of q (pool all sub-intervals)
    mutate(q = exp(-duration * (failRate + dropoutRate)),
           Q = lag(cumprod(q), default = 1)) %>%
    arrange(desc(startFail)) %>%
    # create another 2 auxiliary variable for enroll rate
    # g: number of expected subjects in a sub-interval
    # G: cumulative sum of g (pool all sub-intervals)
    mutate(g = enrollRate * duration,
           G = lag(cumsum(g), default = 0)) %>%
    arrange(startFail) %>%
    # compute expected events as nbar in a sub-interval
    mutate(d = ifelse(failRate == 0, 0, Q * (1 - q) * failRate / (failRate + dropoutRate)),
           nbar = ifelse(failRate == 0, 0, G * d + (failRate * Q * enrollRate) / (failRate + dropoutRate) * (duration - (1 - q) / (failRate + dropoutRate))))
  
  # ----------------------------#
  #       output results        #
  # ----------------------------#
  if(simple){
    ans <- as.numeric(sum(df$nbar))
  }else{
    sf.startFail <- stepfun(startFail, c(0, startFail), right = FALSE)
    ans <- df %>% 
    transmute(t = endFail, failRate = failRate, Events = nbar, startFail = sf.startFail(startFail)) %>%  
    group_by(startFail) %>%
    summarize(failRate = first(failRate), Events = sum(Events)) %>%
    mutate(t = startFail) %>% 
    select("t", "failRate", "Events")
  }
  return(ans)
}