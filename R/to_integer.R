#  Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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

#' Rounds sample size to an even number for equal design
#'
#' @param x An object returned by [fixed_design()], [gs_design_ahr()],
#'   [gs_design_wlr()], or [gs_design_combo()].
#' @param ... Additional parameters (not used).
#'
#' @return A list similar to the output of [fixed_design()],
#'   [gs_design_ahr()], [gs_design_wlr()], or [gs_design_combo()],
#'   except the sample size is an integer.
#'
#' @export to_integer
to_integer <- function(x, ...) {
  UseMethod("to_integer", x)
}

#' @rdname to_integer
#'
#' @param sample_size Logical, indicting if ceiling
#'   sample size to an even integer.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#' library(gsDesign2)
#'
#' # Average hazard ratio
#' \donttest{
#' x <- fixed_design("ahr",
#'   alpha = .025, power = .9,
#'   enroll_rate = tibble(stratum = "All", duration = 18, rate = 1),
#'   fail_rate = tibble(
#'     stratum = "All", duration = c(4, 100),
#'     fail_rate = log(2) / 12, hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36
#' )
#' x %>% to_integer()
#'
#' # FH
#' x <- fixed_design("fh",
#'   alpha = 0.025, power = 0.9,
#'   enroll_rate = tibble(stratum = "All", duration = 18, rate = 20),
#'   fail_rate = tibble(
#'     stratum = "All", duration = c(4, 100),
#'     fail_rate = log(2) / 12, hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   rho = 0.5, gamma = 0.5,
#'   study_duration = 36, ratio = 1
#' )
#' x %>% to_integer()
#'
#' # MB
#' x <- fixed_design("mb",
#'   alpha = 0.025, power = 0.9,
#'   enroll_rate = tibble(stratum = "All", duration = 18, rate = 20),
#'   fail_rate = tibble(
#'     stratum = "All", duration = c(4, 100),
#'     fail_rate = log(2) / 12, hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   tau = 4,
#'   study_duration = 36, ratio = 1
#' )
#' x %>% to_integer()
#' }
to_integer.fixed_design <- function(x, sample_size = TRUE, ...) {
  output_n <- x$analysis$n
  input_n <- expected_accrual(time = x$analysis$time, enroll_rate = x$input$enroll_rate)

  multiply_factor <- x$input$ratio + 1
  enroll_rate_new <- x$enroll_rate %>%
    mutate(rate = rate * ceiling(output_n / multiply_factor) * multiply_factor / output_n)

  if ((x$design == "ahr") && (input_n != output_n)) {
    x_new <- gs_power_ahr(
      enroll_rate = enroll_rate_new,
      fail_rate = x$input$fail_rate,
      event = as.numeric(ceiling(x$analysis$event)),
      analysis_time = NULL,
      ratio = x$input$ratio,
      upar = qnorm(1 - x$input$alpha), lpar = -Inf
    )

    analysis <- tibble::tibble(
      Design = "ahr",
      N = x_new$analysis$n,
      Events = x_new$analysis$event,
      Time = x_new$analysis$time,
      Bound = (x_new$bound %>% filter(bound == "upper"))$z,
      alpha = x$input$alpha,
      Power = (x_new$bound %>% filter(bound == "upper"))$probability
    )

    ans <- list(
      input = x$input, enroll_rate = x_new$enroll_rate, fail_rate = x_new$fail_rate,
      analysis = analysis, design = "ahr"
    )

    class(ans) <- c("fixed_design", class(ans))
  } else if ((x$design == "fh") && (input_n != output_n)) {
    x_new <- gs_power_wlr(
      enroll_rate = enroll_rate_new,
      fail_rate = x$input$fail_rate,
      event = as.numeric(ceiling(x$analysis$event)),
      analysis_time = NULL,
      ratio = x$input$ratio,
      upar = qnorm(1 - x$input$alpha), lpar = -Inf,
      weight = function(s, arm0, arm1) {
        wlr_weight_fh(s, arm0, arm1,
          rho = x$design_par$rho,
          gamma = x$design_par$gamma
        )
      }
    )

    analysis <- tibble::tibble(
      Design = "fh",
      N = x_new$analysis$n,
      Events = x_new$analysis$event,
      Time = x_new$analysis$time,
      Bound = (x_new$bound %>% filter(bound == "upper"))$z,
      alpha = x$input$alpha,
      Power = (x_new$bound %>% filter(bound == "upper"))$probability
    )

    ans <- list(
      input = x$input, enroll_rate = x_new$enroll_rate, fail_rate = x_new$fail_rate,
      analysis = analysis, design = "fh", design_par = x$design_par
    )

    class(ans) <- c("fixed_design", class(ans))
  } else if ((x$design == "mb") && (input_n != output_n)) {
    x_new <- gs_power_wlr(
      enroll_rate = enroll_rate_new,
      fail_rate = x$input$fail_rate,
      event = as.numeric(ceiling(x$analysis$event)),
      analysis_time = NULL,
      ratio = x$input$ratio,
      weight = function(s, arm0, arm1) {
        wlr_weight_fh(s, arm0, arm1,
          rho = -1, gamma = 0,
          tau = x$design_par$tau
        )
      },
      upar = qnorm(1 - x$input$alpha), lpar = -Inf
    )

    analysis <- tibble::tibble(
      Design = "mb",
      N = x_new$analysis$n,
      Events = x_new$analysis$event,
      Time = x_new$analysis$time,
      Bound = (x_new$bound %>% filter(bound == "upper"))$z,
      alpha = x$input$alpha,
      Power = (x_new$bound %>% filter(bound == "upper"))$probability
    )

    ans <- list(
      input = x$input, enroll_rate = x_new$enroll_rate, fail_rate = x_new$fail_rate,
      analysis = analysis, design = "mb", design_par = x$design_par
    )

    class(ans) <- c("fixed_design", class(ans))
  } else {
    message("The input object is not applicatable to get an integer sample size.")
    ans <- x
  }

  return(ans)
}

#' @rdname to_integer
#'
#' @export
#'
#' @examples
#' \donttest{
#' gs_design_ahr() %>% to_integer()
#' }
#' \donttest{
#' gs_design_wlr() %>% to_integer()
#' }
to_integer.gs_design <- function(x, sample_size = TRUE, ...) {
  n_analysis <- length(x$analysis$analysis)
  multiply_factor <- x$input$ratio + 1

  if ("ahr" %in% class(x)) {
    event <- x$analysis$event
    event_new <- c(floor(event[1:(n_analysis - 1)]), ceiling(event[n_analysis])) %>% as.integer()

    sample_size_new <- (ceiling(x$analysis$n[n_analysis] / multiply_factor) * multiply_factor) %>% as.integer()

    if (identical(x$input$upper, gs_spending_bound)) {
      upar_new <- x$input$upar
      upar_new$timing <- event_new / max(event_new)
    } else {
      upar_new <- x$input$upar
    }

    enroll_rate <- x$enroll_rate
    enroll_rate_new <- enroll_rate %>%
      mutate(rate = rate * sample_size_new / x$analysis$n[n_analysis])

    x_new <- gs_power_ahr(
      enroll_rate = enroll_rate_new,
      fail_rate = x$input$fail_rate,
      event = event_new,
      analysis_time = NULL,
      ratio = x$input$ratio,
      upper = x$input$upper, upar = upar_new,
      lower = x$input$lower, lpar = x$input$lpar,
      test_upper = x$input$test_upper,
      test_lower = x$input$test_lower,
      binding = x$input$binding,
      info_scale = x$input$info_scale, r = x$input$r, tol = x$input$tol
    )
  } else if ("wlr" %in% class(x)) {
    event <- x$analysis$event
    event_new <- c(floor(event[1:(n_analysis - 1)]), ceiling(event[n_analysis])) %>% as.integer()

    sample_size_new <- (ceiling(x$analysis$n[n_analysis] / multiply_factor) * multiply_factor) %>% as.integer()

    if (identical(x$input$upper, gs_spending_bound)) {
      upar_new <- x$input$upar
      upar_new$timing <- event_new / max(event_new)
    } else {
      upar_new <- x$input$upar
    }

    enroll_rate <- x$enroll_rate
    enroll_rate_new <- enroll_rate %>%
      mutate(rate = rate * sample_size_new / x$analysis$n[n_analysis])

    x_new <- gs_power_wlr(
      enroll_rate = enroll_rate_new,
      fail_rate = x$input$fail_rate,
      event = event_new,
      analysis_time = NULL,
      ratio = x$input$ratio,
      upper = x$input$upper, upar = upar_new,
      lower = x$input$lower, lpar = x$input$lpar,
      test_upper = x$input$test_upper,
      test_lower = x$input$test_lower,
      binding = x$input$binding,
      info_scale = x$input$info_scale, r = x$input$r, tol = x$input$tol,
      weight = x$input$weight,
      approx = x$input$approx
    )
  } else if ("rd" %in% class(x)) {
    n_stratum <- length(x$input$p_c$stratum)

    sample_size_new <- tibble(
      analysis = 1:n_analysis,
      n = c(
        floor(x$analysis$n[1:(n_analysis - 1)] / multiply_factor),
        ceiling(x$analysis$n[n_analysis] / multiply_factor)
      ) * multiply_factor
    )

    if (identical(x$input$upper, gs_spending_bound)) {
      upar_new <- x$input$upar
      upar_new$timing <- sample_size_new$n / max(sample_size_new$n)
    } else {
      upar_new <- x$input$upar
    }

    if (n_stratum == 1) {
      suppressMessages(
        tbl_n <- tibble(
          analysis = rep(1:n_analysis, each = n_stratum),
          stratum = rep(x$input$p_c$stratum, n_analysis)
        ) %>%
          left_join(sample_size_new)
      )
    } else {
      suppressMessages(
        tbl_n <- tibble(
          analysis = rep(1:n_analysis, each = n_stratum),
          stratum = rep(x$input$p_c$stratum, n_analysis)
        ) %>%
          left_join(x$input$stratum_prev) %>%
          left_join(sample_size_new) %>%
          mutate(n_new = prevalence * n) %>%
          select(-c(n, prevalence)) %>%
          dplyr::rename(n = n_new)
      )
    }

    x_new <- gs_power_rd(
      p_c = x$input$p_c,
      p_e = x$input$p_e,
      n = tbl_n,
      rd0 = x$input$rd0,
      ratio = x$input$ratio,
      weight = x$input$weight,
      upper = x$input$upper,
      lower = x$input$lower,
      upar = upar_new,
      lpar = x$input$lpar,
      info_scale = x$input$info_scale,
      binding = x$input$binding,
      test_upper = x$input$test_upper,
      test_lower = x$input$test_lower,
      r = x$input$r,
      tol = x$input$tol
    )
  } else {
    message("The input object is not applicatable to get an integer sample size.")
    x_new <- x
  }

  return(x_new)
}
