#  Copyright (c) 2021 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co., Inc., Kenilworth, NJ, USA.
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

#' Information and effect size for max combo test
#' @importFrom mvtnorm GenzBretz
#' @importFrom tibble tibble
#' @importFrom tibble tibble
#' 
#' @param enrollRates enrollment rates
#' @param failRates failure and dropout rates
#' @param ratio Experimental:Control randomization ratio (not yet implemented)
#' @param events Targeted events at each analysis
#' @param analysisTimes Minimum time of analysis
#' @param rho Weighting parameters
#' @param gamma Weighting parameters
#' @param tau Weighting parameters
#' @param approx Approximation method
#' 
#' @export
gs_info_combo <- function(enrollRates = tibble(Stratum = "All", 
                                               duration = c(2, 2, 10), 
                                               rate = c(3, 6, 9)),
                          failRates = tibble(Stratum = "All",
                                             duration = c(3,100),
                                             failRate = log(2) / c(9, 18),
                                             hr = c(.9, .6),
                                             dropoutRate = rep(.001, 2)),
                          ratio = 1,                
                          events = NULL, 
                          analysisTimes = NULL,   
                          rho,
                          gamma,
                          tau =  rep(-1, length(rho)),
                          approx = "asymptotic"){
  
  weight <- get_combo_weight(rho, gamma, tau)
  
  info <- lapply(weight, function(x){
    x <- eval(parse(text = x))
    gs_info_wlr(enrollRates, failRates, ratio, events = events, analysisTimes = analysisTimes, weight = x)
  })
  
  info <- dplyr::bind_rows(info, .id = "test")
  info$test <- as.numeric(info$test)
  
  return(info)
}
