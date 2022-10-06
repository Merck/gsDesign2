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

#' Group sequential design power under risk difference
#'
#' @param p_c rate at the control group
#' @param p_e rate at the experimental group 
#' @param N sample size 
#' @param rd0 treatment effect under super-superiority designs, the default is 0
#' @param ratio experimental:control randomization ratio
#' @param upper function to compute upper bound
#' @param upar parameter to pass to upper
#' @param lower function to compare lower bound
#' @param lpar parameter to pass to lower
#' @param info_scale the information scale for calculation
#' @param weight weigting method, either "un-stratified" or "ss" or "invar"
#' @param binding indicator of whether futility bound is binding; default of FALSE is recommended
#' @param test_upper indicator of which analyses should include an upper (efficacy) bound;
#' single value of TRUE (default)  indicates all analyses; otherwise,
#' a logical vector of the same length as \code{info} should indicate which analyses will have an efficacy bound
#' @param test_lower indicator of which analyses should include a lower bound;
#' single value of TRUE (default) indicates all analyses;
#' single value FALSE indicated no lower bound; otherwise,
#' a logical vector of the same length as \code{info} should indicate which analyses will have a lower bound
#' @param r Integer, at least 2; default of 18 recommended by Jennison and Turnbull
#' @param tol Tolerance parameter for boundary convergence (on Z-scale)
#' 
#' @return a \code{tibble} with columns Analysis, Bound, Z, Probability, theta, Time, AHR, Events
#' 
#' @export
#'
#' @examples
#' # --------------------- #
#' #      example 1        #
#' # --------------------- #
#' library(gsDesign)
#' 
#' # un-stratified case with H0: rd0 = 0
#' gs_power_rd(
#'   p_c = tibble::tibble(Stratum = "All",
#'                        Rate = .2),
#'   p_e = tibble::tibble(Stratum = "All",
#'                        Rate = .15),
#'   N = tibble::tibble(Stratum = "All",
#'                      N = c(20, 40, 60),
#'                      Analysis = 1:3),
#'   rd0 = 0,
#'   ratio = 1,
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' )
#' 
#' # --------------------- #
#' #      example 2        #
#' # --------------------- #
#' # un-stratified case with H0: rd0 != 0
#' gs_power_rd(
#'   p_c = tibble::tibble(Stratum = "All",
#'                        Rate = .2),
#'   p_e = tibble::tibble(Stratum = "All",
#'                        Rate = .15),
#'   N = tibble::tibble(Stratum = "All",
#'                      N = c(20, 40, 60),
#'                      Analysis = 1:3),
#'   rd0 = 0.005,
#'   ratio = 1,
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' )
#' 
#' # use spending function
#' gs_power_rd(
#'   p_c = tibble::tibble(Stratum = "All",
#'                        Rate = .2),
#'   p_e = tibble::tibble(Stratum = "All",
#'                        Rate = .15),
#'   N = tibble::tibble(Stratum = "All",
#'                      N = c(20, 40, 60),
#'                      Analysis = 1:3),
#'   rd0 = 0.005,
#'   ratio = 1,
#'   upper = gs_spending_bound,
#'   lower = gs_b,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' )
#' 
#' # --------------------- #
#' #      example 3        #
#' # --------------------- #
#' # stratified case under sample size weighting and H0: rd0 = 0
#' gs_power_rd(
#'   p_c = tibble::tibble(Stratum = c("S1", "S2", "S3"),
#'                        Rate = c(.15, .2, .25)),
#'   p_e = tibble::tibble(Stratum = c("S1", "S2", "S3"),
#'                        Rate = c(.1, .16, .19)),
#'   N = tibble::tibble(Stratum = rep(c("S1", "S2", "S3"), each = 3),
#'                      Analysis = rep(1:3, 3),
#'                      N = c(10, 20, 24, 18, 26, 30, 10, 20, 24)),
#'   rd0 = 0,
#'   ratio = 1,
#'   weight = "ss",
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2)))
#' 
#' # --------------------- #
#' #      example 4        #
#' # --------------------- #
#' # stratified case under inverse variance weighting and H0: rd0 = 0
#' gs_power_rd(
#'   p_c = tibble::tibble(Stratum = c("S1", "S2", "S3"),
#'                        Rate = c(.15, .2, .25)),
#'   p_e = tibble::tibble(Stratum = c("S1", "S2", "S3"),
#'                        Rate = c(.1, .16, .19)),
#'   N = tibble::tibble(Stratum = rep(c("S1", "S2", "S3"), each = 3),
#'                      Analysis = rep(1:3, 3),
#'                      N = c(10, 20, 24, 18, 26, 30, 10, 20, 24)),
#'   rd0 = 0,
#'   ratio = 1,
#'   weight = "invar",
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2)))
#' 
#' # --------------------- #
#' #      example 5        #
#' # --------------------- #
#' # stratified case under sample size weighting and H0: rd0 != 0
#' gs_power_rd(
#'   p_c = tibble::tibble(Stratum = c("S1", "S2", "S3"),
#'                        Rate = c(.15, .2, .25)),
#'   p_e = tibble::tibble(Stratum = c("S1", "S2", "S3"),
#'                        Rate = c(.1, .16, .19)),
#'   N = tibble::tibble(Stratum = rep(c("S1", "S2", "S3"), each = 3),
#'                      Analysis = rep(1:3, 3),
#'                      N = c(10, 20, 24, 18, 26, 30, 10, 20, 24)),
#'   rd0 = 0.02,
#'   ratio = 1,
#'   weight = "ss",
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2)))
#' 
#' # --------------------- #
#' #      example 6        #
#' # --------------------- #
#' # stratified case under inverse variance weighting and H0: rd0 != 0
#' gs_power_rd(
#'   p_c = tibble::tibble(Stratum = c("S1", "S2", "S3"),
#'                        Rate = c(.15, .2, .25)),
#'   p_e = tibble::tibble(Stratum = c("S1", "S2", "S3"),
#'                        Rate = c(.1, .16, .19)),
#'   N = tibble::tibble(Stratum = rep(c("S1", "S2", "S3"), each = 3),
#'                      Analysis = rep(1:3, 3),
#'                      N = c(10, 20, 24, 18, 26, 30, 10, 20, 24)),
#'   rd0 = 0.03,
#'   ratio = 1,
#'   weight = "invar",
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2)))
#'   
gs_power_rd <- function(
    p_c = tibble::tibble(Stratum = "All", 
                         Rate = .2),
    p_e = tibble::tibble(Stratum = "All",
                         Rate = .15),
    N = tibble::tibble(Stratum = "All",  
                       N = c(40, 50, 60),
                       Analysis = 1:3),
    rd0 = 0, 
    ratio = 1,
    weight = c("un-stratified", "ss", "invar"),
    upper = gs_b,
    lower = gs_b,
    upar = list(par = gsDesign(k = length(N), test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound),
    lpar = list(par = c(qnorm(.1), rep(-Inf, length(N) - 1))),
    info_scale = c(0, 1, 2),
    binding = FALSE,
    test_upper = TRUE,
    test_lower = TRUE,
    r = 18,
    tol = 1e-6
){
  
  # get the number of analysis
  K <- max(N$Analysis)
  # get the info_scale
  info_scale <- if(methods::missingArg(info_scale)){2}else{match.arg(as.character(info_scale), choices = 0:2)}
  # get the weighting scheme
  weight <- if(methods::missingArg(weight)){"un-stratified"}else{match.arg(weight)}
  
  # ---------------------------------------- #
  #    calculate the asymptotic variance     #
  #       and statistical information        #
  # ---------------------------------------- #
  x <- gs_info_rd(
    p_c = p_c,
    p_e = p_e,
    N = N,
    rd0 = rd0,
    ratio = ratio,
    weight = weight)
  
  # ---------------------------------------- #
  #  given the above statistical information #
  #         calculate the power              #
  # ---------------------------------------- #
  y_H1 <- gs_power_npe(
    theta = x$rd, 
    info = x$info1, 
    info0 = x$info0,
    info1 = x$info1,
    info_scale = info_scale,
    binding = binding,
    upper = upper, 
    lower = lower, 
    upar = upar,
    lpar = lpar,
    test_upper = test_upper,
    test_lower = test_lower,
    r = r, 
    tol = tol) 
  
  y_H0 <- gs_power_npe(
    theta = x$rd0, 
    info = x$info0, 
    info0 = x$info0,
    info1 = x$info1,
    info_scale = info_scale,
    binding = binding,
    upper = upper, 
    upar = upar,
    test_upper = test_upper,
    lower = lower, 
    lpar = lpar,
    test_lower = test_lower,
    r = r, 
    tol = tol)
  
  # ---------------------------------------- #
  #         organize the outputs             #
  # ---------------------------------------- #
  # summarize the bounds
  suppressMessages(
    bounds <- y_H1 %>% 
      mutate(`~Risk difference at bound` = Z / sqrt(info) / theta * (x$rd[1] - x$rd0[1])  + x$rd0[1],  `Nominal p` = pnorm(-Z)) %>% 
      left_join(y_H0 %>% select(Analysis, Bound, Probability) %>% dplyr::rename(Probability0 = Probability)) %>% 
      select(Analysis, Bound, Probability, Probability0, Z, `~Risk difference at bound`, `Nominal p`)
  )
  # summarize the analysis
  suppressMessages(
    analysis <- x %>% 
      select(Analysis, N, rd, rd0, theta1, theta0) %>% 
      left_join(y_H1 %>% select(Analysis, info, IF) %>% unique()) %>%
      left_join(y_H0 %>% select(Analysis, info, IF) %>% dplyr::rename(info0 = info, IF0 = IF) %>% unique()) %>%
      select(Analysis, N, rd, rd0, theta1, theta0, info, info0, IF, IF0)
  )
  
  ans <- list(
    bounds = bounds,
    analysis = analysis)
  
  class(ans) <- c("rd", "gs_design", class(ans))
  
  return(ans)
}