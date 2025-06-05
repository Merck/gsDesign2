#  Copyright (c) 2025 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
#  All rights reserved.
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

#' Generates a textual summary of a group sequential design using the AHR method.
#' @param x A design object created by [gs_design_ahr()] with or without [to_integer()].
#' @param information A logical value indicating whether to include statistical information in the textual summary. Default is FALSE.
#' @param time_unit A character string specifying the time unit used in the design. Options include "days", "weeks", "months" (default), and "years".
#' @return A character string containing a paragraph that summarizes the design.
#'
#' @export
#' @examples
#' library(gsDesign)
#'
#' # Text summary of a 1-sided design
#' x <- gs_design_ahr(info_frac = 1:3/3, test_lower = FALSE) %>% to_integer()
#' x %>% text_summary()
#'
#' # Text summary of a 2-sided symmetric design
#' x <- gs_design_ahr(info_frac = 1:3/3,
#'                    upper = gs_spending_bound, lower = gs_spending_bound,
#'                    upar = list(sf = sfLDOF, total_spend = 0.025),
#'                    lpar = list(sf = sfLDOF, total_spend = 0.025),
#'                    binding = TRUE, h1_spending = FALSE) %>% to_integer()
#' x %>% text_summary()
#'
#' # Text summary of a asymmetric 2-sided design with beta-spending and non-binding futility bound
#' x <- gs_design_ahr(info_frac = 1:3/3, alpha = 0.025, beta = 0.1,
#'                    upper = gs_spending_bound, lower = gs_spending_bound,
#'                    upar = list(sf = sfLDOF, total_spend = 0.025),
#'                    lpar = list(sf = sfHSD, total_spend = 0.1, param = -4),
#'                    binding = FALSE, h1_spending = TRUE) %>% to_integer()
#' x %>% text_summary()
#'
#' # Text summary of a asymmetric 2-sided design with fixed non-binding futility bound
#' x <- gs_design_ahr(info_frac = 1:3/3, alpha = 0.025, beta = 0.1,
#'                    upper = gs_spending_bound, lower = gs_b,
#'                    upar = list(sf = sfLDOF, total_spend = 0.025),
#'                    test_upper = c(FALSE, TRUE, TRUE),
#'                    lpar = c(-1, -Inf, -Inf),
#'                    test_lower = c(TRUE, FALSE, FALSE),
#'                    binding = FALSE, h1_spending = TRUE) %>% to_integer()
#' x %>% text_summary()
#'
#' # If there are >5 pieces of HRs, we provide a brief summary of HR.
#' gs_design_ahr(
#'   fail_rate = define_fail_rate(duration = c(rep(3, 5), Inf),
#'                                hr = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4),
#'                                fail_rate = log(2) / 10, dropout_rate = 0.001),
#'   info_frac = 1:3/3, test_lower = FALSE) %>%
#'   text_summary()
text_summary <- function(x, information = FALSE, time_unit = "months") {

  n_analysis <- nrow(x$analysis)
  is_gs_design_ahr <- attributes(x)$uninteger_is_from == "gs_design_ahr"
  is_gs_power_ahr <- attributes(x)$uninteger_is_from == "gs_power_ahr"
  is_gs_design_wlr <- attributes(x)$uninteger_is_from == "gs_design_wlr"
  is_gs_power_wlr <- attributes(x)$uninteger_is_from == "gs_power_wlr"
  is_gs_design_rd <- attributes(x)$uninteger_is_from == "gs_design_rd"
  is_gs_power_rd <- attributes(x)$uninteger_is_from == "gs_power_rd"

  # ---------------------------------------- #
  # Check if it is two-sided design or not
  # ---------------------------------------- #
  if ((identical(x$input$lower, gs_b) && (!is.list(x$input$lpar))) || all(!x$input$test_lower)) {
    if (all(x$input$test_lower == FALSE)) {
      two_sided <- FALSE
    } else {
      two_sided <- ifelse(identical(x$input$lpar, rep(-Inf, n_analysis)), FALSE, TRUE)
    }
  } else {
    two_sided <- TRUE
  }

  # ---------------------------------------- #
  # Initialize the output
  # ---------------------------------------- #
  out <- NULL

  # ---------------------------------------- #
  # Add the test type
  # ---------------------------------------- #
  ## test_type = 1
  if (!two_sided) {
    out <- paste(out, "One-sided group sequential design with ", sep = "")
  } else {
  # test_type = 2
    if (identical(x$input$upper, gs_spending_bound) && identical(x$input$lower, gs_spending_bound) &&
        identical(x$input$upar, x$input$lpar) && x$input$binding && !x$input$h1_spending) {
      out <- paste(out, "Symmetric two-sided group sequential design with ", sep = "")
  # test_type = 3, 4, 5, 6
    } else {
      out <- paste(out, "Asymmetric two-sided group sequential design with ", sep = "")
      if (x$input$binding) {
        out <- paste(out, "binding futility bound, ", sep = "")
      } else {
        out <- paste(out, "non-binding futility bound, ", sep = "")
      }
    }
  }

  # ---------------------------------------- #
  # Add the number of analyses, sample size, events
  # ---------------------------------------- #
  out <- paste(out, n_analysis, " analyses, ", sep = "")

  out <- paste(out,
               "time-to-event outcome with sample size ",
               ifelse(is_wholenumber(max(x$analysis$n)), max(x$analysis$n), max(x$analysis$n) %>% round(1)),
               " and ",
               ifelse(is_wholenumber(max(x$analysis$event)), max(x$analysis$event), max(x$analysis$event) %>% round(1)),
               " events, ",
               sep = "")

  # ---------------------------------------- #
  # Add information, power and type I error
  # ---------------------------------------- #
  if (information) {
    out <- paste(out, " total information ", round(x$analysis$info[n_analysis], 2), ", ", sep = "")
  }

  # ---------------------------------------- #
  # Add power and type I error
  # ---------------------------------------- #
  # if it is a gs_design_ahr object...
  if (is_gs_design_ahr) {
    out <- paste(out, 100 * round(x$bound$probability[x$bound$bound == "upper" & x$bound$analysis == n_analysis], 2), " percent power, ", 100 * x$input$alpha, " percent (1-sided) Type I error", sep = "")
  } else if (is_gs_power_ahr) {
    out <- paste(out, 100 * x$input$alpha, " percent (1-sided) Type I error", sep = "")
  }

  # ---------------------------------------- #
  # Add HR assumption
  # ---------------------------------------- #
  if (nrow(x$fail_rate) == 1) {
    temp <- paste("a hazard ratio of ", round(x$fail_rate$hr, 2), sep = "")
  } else if (nrow(x$fail_rate) == 2) {
    temp <- paste("hazard ratio of ",
                  round(x$fail_rate$hr[1], 2), " during the first ", round(x$fail_rate$duration[1], 2), " ", time_unit,
                  " and ", round(x$fail_rate$hr[2], 2), " thereafter", sep = "")
  } else if (nrow(x$fail_rate) <= 5) {
    temp <- paste(x$fail_rate$hr[1:(nrow(x$fail_rate) - 1)] %>% round(2),
                  c(" during the first ", rep(" during the next ", nrow(x$fail_rate) - 2)),
                  c(x$fail_rate$duration[1:(nrow(x$fail_rate) - 1)] %>% round(2)), time_unit) %>%
      paste(collapse = ", ") %>%
      paste(" and ", x$fail_rate$hr[nrow(x$fail_rate)] %>% round(2), " thereafter")
    temp <- paste("hazard ratio of ", temp, sep = "")
  } else {
    temp <- "piecewise hazard ratio"
  }

  if (is_gs_design_ahr) {
    out <- paste(out, " to detect ", temp, sep = "")
  } else if (is_gs_power_ahr) {
    out_end <- paste(" With ", temp,
                     ", the power is ",
                     100 * round(x$bound$probability[x$bound$analysis == n_analysis & x$bound$bound == "upper"], 2),
                     " percent.", sep = "")
  }
  # ---------------------------------------- #
  # Add enrollment and study duration
  # ---------------------------------------- #
  out <- paste(out, ". Enrollment and total study durations are assumed to be ", round(sum(x$enroll_rate$duration), 1),
               " and ", round(max(x$analysis$time), 1), " ", time_unit, ", respectively",
               sep = "")

  # ---------------------------------------- #
  # Add upper bounds derivation
  # ---------------------------------------- #
  if (identical(x$input$upper, gs_spending_bound) && identical(x$input$lower, gs_spending_bound) &&
      identical(x$input$upar, x$input$lpar) && x$input$binding && !x$input$h1_spending) {
    out <- paste(out, ". Bounds derived using a ", sep = "")
  } else {
    out <- paste(out, ". Efficacy bounds derived using a", sep = "")
  }

  analysis_seq <- c(paste("IA", 1:(n_analysis - 1), sep = ""), "FA")
  upper_text <- x$input$upar$sf(alpha = x$input$upar$total_spend, t = x$analysis$info_frac, param = x$input$upar$param)
  upper_tested <- if (!all(x$input$test_upper)) {
    paste(", tested at", paste("tested at", paste(analysis_seq[x$input$test_upper], collapse = ", ")))
  }
  out <- paste(out, " ", summary(upper_text), upper_tested, ".", sep = "")

  # ---------------------------------------- #
  # Add lower bounds derivation
  # ---------------------------------------- #
  if (any(x$bound$bound == "lower") &&
      !(identical(x$input$upper, gs_spending_bound) && identical(x$input$lower, gs_spending_bound) &&
        identical(x$input$upar, x$input$lpar) && x$input$binding && !x$input$h1_spending)) {
    lower_tested <- if (!all(x$input$test_lower)) {
      paste(", tested at", paste("tested at", paste(analysis_seq[x$input$test_lower], collapse = ", ")))
    }

    if (identical(x$input$lower, gs_spending_bound)) {
      lower_text <- x$input$lpar$sf(alpha = x$input$lpar$total_spend, t = x$analysis$info_frac, param = x$input$lpar$param)
      out <- paste(out, " Futility bounds derived using a ", summary(lower_text), lower_tested, ".", sep = "")
    } else if (identical(x$input$lower, gs_b)) {
      out <- paste(out, " Futility bounds is fixed as ", paste0(x$input$lpar, collapse = ", ") , lower_tested, ".", sep = "")
    }
  }

  # ---------------------------------------- #
  # Add power for gs_power_ahr object
  # ---------------------------------------- #
  if (is_gs_power_ahr) {
    out <- paste(out, out_end, sep = "")
  }

  return(out)
}
