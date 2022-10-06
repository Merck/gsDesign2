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
#' @importFrom dplyr lag
NULL
#' Information and effect size under risk difference 
#' 
#' @param p_c rate at the control group
#' @param p_e rate at the experimental group 
#' @param N sample size 
#' @param rd0 the risk difference under H0 
#' @param ratio Experimental:Control randomization ratio
#' @param weight weigting method, either "un-stratified" or "ss" or "invar"
#' @export
#' @examples
#' library(tibble)
#' # --------------------- #
#' #      example 1        #
#' # --------------------- #
#' # un-stratified case with H0: rd0 = 0
#' gs_info_rd(
#'   p_c = tibble(Stratum = "All", Rate = .15),
#'   p_e = tibble(Stratum = "All", Rate = .1),
#'   N = tibble(Stratum = "All", N = c(100, 200, 300), Analysis = 1:3),
#'   rd0 = 0,
#'   ratio = 1
#' )
#' 
#' # --------------------- #
#' #      example 2        #
#' # --------------------- #
#' # un-stratified case with H0: rd0 != 0
#' gs_info_rd(
#'   p_c = tibble(Stratum = "All", Rate = .2),
#'   p_e = tibble(Stratum = "All", Rate = .15),
#'   N = tibble(Stratum = "All", N = c(100, 200, 300), Analysis = 1:3),
#'   rd0 = 0.005,
#'   ratio = 1
#' )
#' 
#' # --------------------- #
#' #      example 3        #
#' # --------------------- #
#' # stratified case under sample size weighting and H0: rd0 = 0
#' gs_info_rd(
#'   p_c = tibble(Stratum = c("S1", "S2", "S3"), Rate = c(.15, .2, .25)),
#'   p_e = tibble(Stratum = c("S1", "S2", "S3"), Rate = c(.1, .16, .19)),
#'   N = tibble(Stratum = rep(c("S1", "S2", "S3"), each = 3),
#'              Analysis = rep(1:3, 3),
#'              N = c(50, 100, 200, 40, 80, 160, 60, 120, 240)),
#'   rd0 = 0,
#'   ratio = 1,
#'   weight = "ss")
#' 
#' # --------------------- #
#' #      example 4        #
#' # --------------------- #
#' # stratified case under inverse variance weighting and H0: rd0 = 0
#' gs_info_rd(
#'   p_c = tibble(Stratum = c("S1", "S2", "S3"),
#'                Rate = c(.15, .2, .25)),
#'   p_e = tibble(Stratum = c("S1", "S2", "S3"),
#'                Rate = c(.1, .16, .19)),
#'   N = tibble(Stratum = rep(c("S1", "S2", "S3"), each = 3),
#'              Analysis = rep(1:3, 3),
#'              N = c(50, 100, 200, 40, 80, 160, 60, 120, 240)),
#'   rd0 = 0,
#'   ratio = 1,
#'   weight = "invar")
#' 
#' # --------------------- #
#' #      example 5        #
#' # --------------------- #
#' # stratified case under sample size weighting and H0: rd0 != 0
#' gs_info_rd(
#'   p_c = tibble(Stratum = c("S1", "S2", "S3"),
#'                Rate = c(.15, .2, .25)),
#'   p_e = tibble(Stratum = c("S1", "S2", "S3"),
#'                Rate = c(.1, .16, .19)),
#'   N = tibble(Stratum = rep(c("S1", "S2", "S3"), each = 3),
#'              Analysis = rep(1:3, 3),
#'              N = c(50, 100, 200, 40, 80, 160, 60, 120, 240)),
#'   rd0 = 0.02,
#'   ratio = 1,
#'   weight = "ss")
#' 
#' # --------------------- #
#' #      example 6        #
#' # --------------------- #
#' # stratified case under inverse variance weighting and H0: rd0 != 0
#' gs_info_rd(
#'   p_c = tibble(Stratum = c("S1", "S2", "S3"),
#'                Rate = c(.15, .2, .25)),
#'   p_e = tibble(Stratum = c("S1", "S2", "S3"),
#'                Rate = c(.1, .16, .19)),
#'   N = tibble(Stratum = rep(c("S1", "S2", "S3"), each = 3),
#'              Analysis = rep(1:3, 3),
#'              N = c(50, 100, 200, 40, 80, 160, 60, 120, 240)),
#'   rd0 = 0.02,
#'   ratio = 1,
#'   weight = "invar")
#' 
#' # --------------------- #
#' #      example 7        #
#' # --------------------- #
#' # stratified case under inverse variance weighting and H0: rd0 != 0 and 
#' # rd0 difference for different statum
#' gs_info_rd(
#'   p_c = tibble(Stratum = c("S1", "S2", "S3"),
#'                Rate = c(.15, .2, .25)),
#'   p_e = tibble(Stratum = c("S1", "S2", "S3"),
#'                Rate = c(.1, .16, .19)),
#'   N = tibble(Stratum = rep(c("S1", "S2", "S3"), each = 3),
#'              Analysis = rep(1:3, 3),
#'              N = c(50, 100, 200, 40, 80, 160, 60, 120, 240)),
#'   rd0 = tibble(Stratum = c("S1", "S2", "S3"),
#'                rd0 = c(0.01, 0.02, 0.03)),
#'   ratio = 1,
#'   weight = "invar")
#' 
gs_info_rd <- function(
    p_c = tibble::tibble(Stratum = "All",
                         Rate = .2),
    p_e = tibble::tibble(Stratum = "All",
                         Rate = .15),
    N = tibble::tibble(Stratum = "All",
                       N = c(100, 200, 300),
                       Analysis = 1:3),
    rd0 = 0, 
    ratio = 1,
    weight = c("un-stratified", "ss", "invar")
){
  
  K <- max(N$Analysis)
  weight <- match.arg(weight)
  
  # -------------------------------------------------#
  #       pool the input arguments together          #
  # -------------------------------------------------#
  suppressMessages(
    tbl <- N %>% 
      left_join(p_c) %>%
      dplyr::rename(p_c = Rate) %>% 
      left_join(p_e) %>% 
      dplyr::rename(p_e = Rate) %>% 
      left_join(if("data.frame" %in% class(rd0)){rd0}else{tibble::tibble(Analysis = 1:K, rd0 = rd0)}) %>% 
      mutate(
        N_e = N / (1 + ratio), 
        N_c = N * ratio / (1 + ratio),
        d = ifelse(p_c > p_e, 1, -1),
        p_pool_per_k_per_s = (N_c * p_c + N_e * p_e) / N,
        p_e0 = (p_c + ratio * p_e - d * rd0) / (ratio + 1),
        p_c0 = p_e0 + d * rd0)
  )
  
  
  
  # -------------------------------------------------#
  #   calculate the variance of the risk difference  #
  # -------------------------------------------------#
  if(is.numeric(rd0) && rd0 == 0){
    tbl <- tbl %>% mutate(sigma2_H0_per_k_per_s = p_pool_per_k_per_s * (1 - p_pool_per_k_per_s) * (1 / N_c + 1 / N_e),
                          sigma2_H1_per_k_per_s = p_c * (1 - p_c) / N_c  + p_e * (1 - p_e) / N_e)
  }else if("data.frame" %in% class(rd0) || rd0 != 0){
    tbl <- tbl %>% mutate(sigma2_H0_per_k_per_s = p_c0 * (1 - p_c0) / N_c + p_e0 * (1 - p_e0) / N_e,
                          sigma2_H1_per_k_per_s = p_c * (1 - p_c) / N_c  + p_e * (1 - p_e) / N_e)
  }
  
  # -------------------------------------------------#
  #               assign weights                     #
  # -------------------------------------------------#
  if(weight == "un-stratified"){
    tbl <- tbl %>% mutate(weight_per_k_per_s = 1) 
  }else if(weight == "ss"){
    suppressMessages(
      tbl <- tbl %>% 
        left_join(tbl %>% dplyr::group_by(Analysis) %>% summarize(sum_ss = sum(N_c * N_e / (N_c + N_e)))) %>%
        mutate(weight_per_k_per_s = N_c * N_e / (N_c + N_e) / sum_ss ) %>% 
        select(-sum_ss)
    )
  }else if(weight == "invar"){
    suppressMessages(
      tbl <- tbl %>% 
        left_join(tbl %>% dplyr::group_by(Analysis) %>% summarize(sum_inv_var_per_s = sum(1/sigma2_H0_per_k_per_s))) %>% 
        mutate(weight_per_k_per_s = 1/sigma2_H0_per_k_per_s / sum_inv_var_per_s) %>% 
        select(-sum_inv_var_per_s)
    )
  }
  
  # -------------------------------------------------#
  #           pool the strata together               #
  # -------------------------------------------------#
  ans <- tbl %>% 
    dplyr::group_by(Analysis) %>%
    summarize(N = sum(N),
              rd = sum((p_c - p_e) * d * weight_per_k_per_s),
              rd0 = sum(rd0 * weight_per_k_per_s),
              sigma2_H0 = sum((weight_per_k_per_s^2 * p_c0 * (1 - p_c0) +  
                                 weight_per_k_per_s^2 * p_e0 * (1 - p_e0) / ratio ) * (1 + ratio)),
              sigma2_H0 = sum(if(sum(rd0 == 0) == 0){
                weight_per_k_per_s^2 * p_pool_per_k_per_s * (1 - p_pool_per_k_per_s) * (1/N_c + 1/N_e)
              }else{
                weight_per_k_per_s^2 * p_c0 * (1 - p_c0) / N_c +  weight_per_k_per_s^2 * p_e0 * (1 - p_e0) / N_e
              }),
              sigma2_H1 = sum(weight_per_k_per_s^2 * p_c * (1 - p_c) / N_c + weight_per_k_per_s^2 * p_e * (1 - p_e) / N_e)) %>% 
    mutate(theta1 = rd / sqrt(sigma2_H1),
           theta0 = rd0 / sqrt(sigma2_H0),
           info1 = 1 / sigma2_H1,
           info0 = 1 / sigma2_H0) %>%
    dplyr::ungroup() %>% 
    select(Analysis, N, rd, rd0, theta1, theta0, info1, info0)
  
  return(ans)
}
