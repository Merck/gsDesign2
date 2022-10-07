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

#' @importFrom tibble tibble
#' @importFrom gsDesign gsDesign sfLDOF
#' @importFrom stats qnorm
#' @importFrom dplyr select arrange desc right_join
NULL
#' Group sequential design power using average hazard ratio under non-proportional hazards
#'
#' @param enrollRates enrollment rates
#' @param failRates failure and dropout rates
#' @param ratio Experimental:Control randomization ratio (not yet implemented)
#' @param events Targeted events at each analysis
#' @param analysisTimes Minimum time of analysis
#' @param binding indicator of whether futility bound is binding; default of FALSE is recommended
#' @param upper Function to compute upper bound
#' @param upar Parameter passed to \code{upper()}
#' @param lower Function to compute lower bound
#' @param lpar Parameter passed to \code{lower()}
#' @param test_upper indicator of which analyses should include an upper (efficacy) bound; single value of TRUE (default) indicates all analyses;
#' otherwise, a logical vector of the same length as \code{info} should indicate which analyses will have an efficacy bound
#' @param test_lower indicator of which analyses should include an lower bound; single value of TRUE (default) indicates all analyses;
#' single value FALSE indicated no lower bound; otherwise, a logical vector of the same length as \code{info} should indicate which analyses will have a
#' lower bound
#' @param r  Integer, at least 2; default of 18 recommended by Jennison and Turnbull
#' @param tol Tolerance parameter for boundary convergence (on Z-scale)
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Calculate information and effect size based on AHR approximation using \code{gs_info_ahr()}.
#'    \item Return a tibble of with columns Analysis, Bound, Z, Probability, theta,
#'     Time, AHR, Events and  contains a row for each analysis and each bound.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @return a \code{tibble} with columns \code{Analysis, Bound, Z, Probability, theta, Time, AHR, Events}.
#' Contains a row for each analysis and each bound.
#' @details
#' Bound satisfy input upper bound specification in \code{upper, upar} and lower bound specification in \code{lower, lpar}.
#' The \code{AHR()} function computes statistical information at targeted event times.
#' The \code{tEvents()} function is used to get events and average HR at targeted \code{analysisTimes}.
#'
#' @noRd
#'
#' @examples
#' library(gsDesign2)
#' library(dplyr)
#' 
#' gs_power_ahr() %>% filter(abs(Z) < Inf)
#'
#' # 2-sided symmetric O'Brien-Fleming spending bound
#' # NOT CURRENTLY WORKING
#' gs_power_ahr(analysisTimes = c(12, 24, 36),
#'               binding = TRUE,
#'               upper = gs_spending_bound,
#'               upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'               lower = gs_spending_bound,
#'               lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL))
#'
gs_power_ahr_ <- function(enrollRates=tibble::tibble(Stratum="All",
                                                    duration=c(2,2,10),
                                                    rate=c(3,6,9)),
                         failRates=tibble::tibble(Stratum="All",
                                                  duration=c(3,100),
                                                  failRate=log(2)/c(9,18),
                                                  hr=c(.9,.6),
                                                  dropoutRate=rep(.001,2)),
                         ratio=1,                 # Experimental:Control randomization ratio
                         events = c(30, 40, 50),  # Targeted events of analysis
                         analysisTimes = NULL,    # Targeted times of analysis
                         binding = FALSE,
                         upper = gs_b,
                         # Default is Lan-DeMets approximation of
                         upar = gsDesign(k=length(events), test.type=1,
                                         n.I=events, maxn.IPlan = max(events),
                                         sfu=sfLDOF, sfupar = NULL)$upper$bound,
                         lower = gs_b,
                         lpar = c(qnorm(.1), rep(-Inf, length(events) - 1)), # Futility only at IA1
                         test_upper = TRUE,
                         test_lower = TRUE,
                         r = 18,
                         tol = 1e-6
){
  x <- gs_info_ahr(enrollRates = enrollRates,
                   failRates = failRates,
                   ratio = ratio,
                   events = events,
                   analysisTimes = analysisTimes
  )
  return(gs_power_npe_(theta = x$theta, info = x$info, info0 = x$info0, binding = binding,
                      upper=upper, lower=lower, upar = upar, lpar= lpar,
                      test_upper = test_upper, test_lower = test_lower,
                      r = r, tol = tol) %>%
           right_join(x %>% select(-c(info, info0, theta)), by = "Analysis") %>%
           select(c(Analysis, Bound, Time, Events, Z, Probability, AHR, theta, info, info0)) %>%
           arrange(desc(Bound), Analysis)
  )
}