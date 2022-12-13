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

#' This is the function to format the bounds summary table of fixed design into gt style.
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
  }
  
  return(x_new)
}