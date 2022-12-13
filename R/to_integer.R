#  Copyright (c) 2022 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co., Inc., Rahway, NJ, USA.
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

#' S3 class method to round sample size to an even number for equal design.
#'
#' @param x an object returned from \code{gs_design_ahr}, \code{gs_design_wlr}, or \code{gs_design_combo}
#' @param ... additional arguments
#'
#' @return a list similar to the output of \code{gs_design_ahr}, \code{gs_design_wlr}, or \code{gs_design_combo}, 
#' but the sample size is an integer
#' @export
#'
to_integer <- function(x, ...) {
  UseMethod("to_integer", x)
}

#' This is the function to format the fixed design into integer sample size.
#' @rdname to_integer.fixed_design
#' @param x an object returned from \code{fixed_design}
#' @param sample_size \code{TRUE} or \code{FALSE}, indicting if to ceiling sample size to an even integer
#' @param ... additional arguments
#' 
#' @return a list similar to the output of \code{fixed_design}, but the sample size is an integer
#' 
#' @export to_integer
#' @exportS3Method
#'  
#' @method to_integer fixed_design
#' 
#' @examples
#' library(dplyr)
#' library(tibble)
#' library(gsDesign2)
#' 
#' # Average hazard ratio
#' x <- fixed_design("ahr", 
#'                   alpha = .025, power = .9, 
#'                   enroll_rate = tibble(stratum = "All",  duration = 18, rate = 1),
#'                   fail_rate = tibble(stratum = "All", duration = c(4, 100), 
#'                                      fail_rate = log(2) / 12, hr = c(1, .6), 
#'                                      dropout_rate = .001),
#'                   study_duration = 36)
#' x %>% to_integer()    
#' 
#' # FH
#' x <- fixed_design("fh", alpha = 0.025, power = 0.9, 
#'                   enroll_rate = tibble(stratum = "All", duration = 18, rate = 20), 
#'                   fail_rate = tibble(stratum = "All", duration = c(4, 100), 
#'                                      fail_rate = log(2) / 12, hr = c(1, .6), 
#'                                      dropout_rate = .001), 
#'                   rho = 0.5, gamma = 0.5,
#'                   study_duration = 36, ratio = 1)         
#' x %>% to_integer() 
#' 
#' # MB
#' x <- fixed_design("mb", alpha = 0.025, power = 0.9, 
#'                   enroll_rate = tibble(stratum = "All", duration = 18, rate = 20), 
#'                   fail_rate = tibble(stratum = "All", duration = c(4, 100), 
#'                                      fail_rate = log(2) / 12, hr = c(1, .6), 
#'                                      dropout_rate = .001), 
#'                   tau = 4,
#'                   study_duration = 36, ratio = 1)         
#' x %>% to_integer()                   
to_integer.fixed_design <- function(x, sample_size = TRUE, ...){
  
  enroll_rate_new <- x$enroll_rate %>% mutate(rate = rate * ceiling(x$analysis$N / 2) * 2 / x$analysis$N) 
  
  input_N <- expected_accrual(time = x$analysis$Time, enroll_rate = x$input$enroll_rate)
  output_N <- x$analysis$N
  
  if(x$design == "ahr" & input_N != output_N){
    x_new <- gs_power_ahr(enroll_rate = enroll_rate_new,
                          fail_rate = x$input$fail_rate,
                          event = as.numeric(ceiling(x$analysis$Events)),
                          analysis_time = NULL,
                          ratio = x$input$ratio,
                          upar = qnorm(1 - x$input$alpha), lpar = -Inf)
    
    ans <- tibble::tibble(Design = "ahr",
                          N = x_new$analysis$N,
                          Events = x_new$analysis$Events,
                          Time = x_new$analysis$Time,
                          Bound = (x_new$bounds %>% filter(Bound == "Upper"))$Z,
                          alpha = x$input$alpha,
                          Power = (x_new$bounds %>% filter(Bound == "Upper"))$Probability)
    
    list(input = x$input, enroll_rate = x_new$enroll_rate, fail_rate = x_new$fail_rate, 
         analysis = ans, design = "ahr")
    
  }else if(x$design == "fh" & input_N != output_N){
    x_new <- gs_power_wlr(enroll_rate = enroll_rate_new,
                          fail_rate = x$input$fail_rate,
                          event = as.numeric(ceiling(x$analysis$Events)),
                          analysis_time = NULL,
                          ratio = x$input$ratio,
                          upar = qnorm(1 - x$input$alpha), lpar = -Inf,
                          weight = function(s, arm0, arm1){wlr_weight_fh(s, arm0, arm1, 
                                                                         rho = x$design_par$rho, 
                                                                         gamma = x$design_par$gamma)})
    
    ans <- tibble::tibble(Design = "fh",
                          N = x_new$analysis$N,
                          Events = x_new$analysis$Events,
                          Time = x_new$analysis$Time,
                          Bound = (x_new$bounds %>% filter(Bound == "Upper"))$Z,
                          alpha = x$input$alpha,
                          Power = (x_new$bounds %>% filter(Bound == "Upper"))$Probability)
    
    list(input = x$input, enroll_rate = x_new$enroll_rate, fail_rate = x_new$fail_rate, 
         analysis = ans, design = "fh", 
         design_par = x$design_par)
    
    
  }else if(x$design == "mb" & input_N != output_N){
    x_new <- gs_power_wlr(enroll_rate = enroll_rate_new,
                          fail_rate = x$input$fail_rate,
                          event = as.numeric(ceiling(x$analysis$Events)),
                          analysis_time = NULL,
                          ratio = x$input$ratio,
                          weight = function(s, arm0, arm1){wlr_weight_fh(s, arm0, arm1, rho = -1, gamma = 0,
                                                                         tau = x$design_par$tau)},
                          upar = qnorm(1 - x$input$alpha), lpar = -Inf)
    
    ans <- tibble::tibble(Design = "mb",
                          N = x_new$analysis$N,
                          Events = x_new$analysis$Events,
                          Time = x_new$analysis$Time,
                          Bound = (x_new$bounds %>% filter(Bound == "Upper"))$Z,
                          alpha = x$input$alpha,
                          Power = (x_new$bounds %>% filter(Bound == "Upper"))$Probability)
    
    list(input = x$input, enroll_rate = d$enroll_rate, fail_rate = d$fail_rate, 
         analysis = ans, design = "mb", 
         design_par = x$design_par)
    
  }else{
    message("The input object is not applicatable to get an integer sample size.")
    x_new <- x
  }
  
  return(x_new)
}


#' This is the function to format group sequential design into interger sample size.
#' @rdname to_integer.gs_design
#' @param x an object returned from \code{gs_design_ahr}, \code{gs_design_wlr}, or \code{gs_design_combo}
#' @param sample_size \code{TRUE} or \code{FALSE}, indicting if to ceiling sample size to an even integer
#' @param ... additional arguments
#' 
#' @return a list similar to the output of \code{gs_design_ahr}, \code{gs_design_wlr}, or \code{gs_design_combo}, 
#' but the sample size is an integer
#' 
#' @export to_integer
#' @exportS3Method
#'  
#' @method to_integer gs_design
#' 
#' @examples
#' library(dplyr)
#' gs_design_ahr() %>% to_integer()
#' gs_design_wlr() %>% to_integer()
#' 
to_integer.gs_design <- function(x, sample_size = TRUE, ...){
  enroll_rate <- x$enroll_rate
  enroll_rate_new <- enroll_rate %>% mutate(rate = rate * ceiling(x$analysis$N / 2) * 2 / x$analysis$N) 
  
  if("ahr" %in% class(x)){
    x_new <- gs_power_ahr(enroll_rate = enroll_rate_new,
                          fail_rate = x$input$fail_rate,
                          event = as.numeric(ceiling(x$analysis$Events)),
                          analysis_time = NULL,
                          ratio = x$input$ratio,
                          upper = x$input$upper, upar = x$input$upar,
                          lower = x$input$lower, lpar = x$input$lpar,
                          test_upper = x$input$test_upper,
                          test_lower = x$input$test_lower,
                          binding = x$input$binding, 
                          info_scale = x$input$info_scale, r = x$input$r, tol = x$input$tol)
    
  }else if("wlr" %in% class(x)){
    x_new <- gs_power_wlr(enroll_rate = enroll_rate_new,
                          fail_rate = x$input$fail_rate,
                          event = as.numeric(ceiling(x$analysis$Events)),
                          analysis_time = NULL,
                          ratio = x$input$ratio,
                          upper = x$input$upper, upar = x$input$upar,
                          lower = x$input$lower, lpar = x$input$lpar,
                          test_upper = x$input$test_upper,
                          test_lower = x$input$test_lower,
                          binding = x$input$binding, 
                          info_scale = x$input$info_scale, r = x$input$r, tol = x$input$tol,
                          weight = x$input$weight,
                          approx = x$input$approx)
  }else{
    message("The input object is not applicatable to get an integer sample size.")
    x_new <- x
  }
  
  return(x_new)
}