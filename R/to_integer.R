#  Copyright (c) 2024 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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
#' @param x An object returned by fixed_design_xxx() and gs_design_xxx().
#' @param ... Additional parameters (not used).
#'
#' @return A list similar to the output of fixed_design_xxx() and gs_design_xxx(),
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
#' library(gsDesign2)
#'
#' # Average hazard ratio
#' \donttest{
#' x <- fixed_design_ahr(
#'   alpha = .025, power = .9,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 1),
#'   fail_rate = define_fail_rate(
#'     duration = c(4, 100),
#'     fail_rate = log(2) / 12, hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36
#' )
#' x |>
#'   to_integer() |>
#'   summary()
#'
#' # FH
#' x <- fixed_design_fh(
#'   alpha = 0.025, power = 0.9,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 20),
#'   fail_rate = define_fail_rate(
#'     duration = c(4, 100),
#'     fail_rate = log(2) / 12,
#'     hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   rho = 0.5, gamma = 0.5,
#'   study_duration = 36, ratio = 1
#' )
#' x |>
#'   to_integer() |>
#'   summary()
#'
#' # MB
#' x <- fixed_design_mb(
#'   alpha = 0.025, power = 0.9,
#'   enroll_rate = define_enroll_rate(duration = 18, rate = 20),
#'   fail_rate = define_fail_rate(
#'     duration = c(4, 100),
#'     fail_rate = log(2) / 12, hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   tau = 4,
#'   study_duration = 36, ratio = 1
#' )
#' x |>
#'   to_integer() |>
#'   summary()
#' }
to_integer.fixed_design <- function(x, sample_size = TRUE, ...) {
  output_n <- x$analysis$n
  input_n <- expected_accrual(time = x$analysis$time, enroll_rate = x$input$enroll_rate)

  multiply_factor <- x$input$ratio + 1
  enroll_rate_new <- x$enroll_rate %>%
    mutate(rate = rate * ceiling(output_n / multiply_factor) * multiply_factor / output_n)

  # Round up the FA events
  event_ceiling <- ceiling(x$analysis$event)

  if ((x$design == "ahr") && (input_n != output_n)) {
    x_new <- gs_power_ahr(
      enroll_rate = enroll_rate_new,
      fail_rate = x$input$fail_rate,
      event = event_ceiling,
      analysis_time = NULL,
      ratio = x$input$ratio,
      upper = gs_b, lower = gs_b,
      upar = qnorm(1 - x$input$alpha), lpar = -Inf
    )

    analysis <- tibble::tibble(
      design = "ahr",
      n = x_new$analysis$n,
      event = x_new$analysis$event,
      time = x_new$analysis$time,
      bound = (x_new$bound %>% filter(bound == "upper"))$z,
      alpha = x$input$alpha,
      power = (x_new$bound %>% filter(bound == "upper"))$probability
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
      event = event_ceiling,
      analysis_time = NULL,
      ratio = x$input$ratio,
      upper = gs_b, lower = gs_b,
      upar = qnorm(1 - x$input$alpha), lpar = -Inf,
      weight = function(s, arm0, arm1) {
        wlr_weight_fh(s, arm0, arm1,
          rho = x$design_par$rho,
          gamma = x$design_par$gamma
        )
      }
    )

    analysis <- tibble::tibble(
      design = "fh",
      n = x_new$analysis$n,
      event = x_new$analysis$event,
      time = x_new$analysis$time,
      bound = (x_new$bound %>% filter(bound == "upper"))$z,
      alpha = x$input$alpha,
      power = (x_new$bound %>% filter(bound == "upper"))$probability
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
      event = event_ceiling,
      analysis_time = NULL,
      ratio = x$input$ratio,
      weight = function(s, arm0, arm1) {
        wlr_weight_fh(s, arm0, arm1,
          rho = -1, gamma = 0,
          tau = x$design_par$tau
        )
      },
      upper = gs_b, lower = gs_b,
      upar = qnorm(1 - x$input$alpha), lpar = -Inf
    )

    analysis <- tibble::tibble(
      design = "mb",
      n = x_new$analysis$n,
      event = x_new$analysis$event,
      time = x_new$analysis$time,
      bound = (x_new$bound %>% filter(bound == "upper"))$z,
      alpha = x$input$alpha,
      power = (x_new$bound %>% filter(bound == "upper"))$probability
    )

    ans <- list(
      input = x$input, enroll_rate = x_new$enroll_rate, fail_rate = x_new$fail_rate,
      analysis = analysis, design = "mb", design_par = x$design_par
    )

    class(ans) <- c("fixed_design", class(ans))
  } else {
    message("The input object is not applicable to get an integer sample size.")
    ans <- x
  }

  # Make n and event of ans$analysis exactly integers
  if ("fixed_design" %in% class(ans)) {
    ans$analysis$n <- round(ans$analysis$n)
    ans$analysis$event <- round(ans$analysis$event)
  }

  return(ans)
}

#' @rdname to_integer
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Example 1: Information fraction based spending
#' gs_design_ahr(
#'   analysis_time = c(18, 30),
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL),
#'   lower = gs_b,
#'   lpar = c(-Inf, -Inf)
#' ) |>
#'   to_integer() |>
#'   summary()
#'
#' gs_design_wlr(
#'   analysis_time = c(18, 30),
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL),
#'   lower = gs_b,
#'   lpar = c(-Inf, -Inf)
#' ) |>
#'   to_integer() |>
#'   summary()
#'
#' gs_design_rd(
#'   p_c = tibble::tibble(stratum = c("A", "B"), rate = c(.2, .3)),
#'   p_e = tibble::tibble(stratum = c("A", "B"), rate = c(.15, .27)),
#'   weight = "ss",
#'   stratum_prev = tibble::tibble(stratum = c("A", "B"), prevalence = c(.4, .6)),
#'   info_frac = c(0.7, 1),
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL),
#'   lower = gs_b,
#'   lpar = c(-Inf, -Inf)
#' ) |>
#'   to_integer() |>
#'   summary()
#'
#' # Example 2: Calendar based spending
#' x <- gs_design_ahr(
#'   upper = gs_spending_bound,
#'   analysis_time = c(18, 30),
#'   upar = list(
#'     sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL,
#'     timing = c(18, 30) / 30
#'   ),
#'   lower = gs_b,
#'   lpar = c(-Inf, -Inf)
#' ) |> to_integer()
#'
#' # The IA nominal p-value is the same as the IA alpha spending
#' x$bound$`nominal p`[1]
#' gsDesign::sfLDOF(alpha = 0.025, t = 18 / 30)$spend
#' }
to_integer.gs_design <- function(x, sample_size = TRUE, ...) {
  is_ahr <- inherits(x, "ahr")
  is_wlr <- inherits(x, "wlr")
  is_rd  <- inherits(x, "rd")
  if (!(is_ahr || is_wlr || is_rd)) {
    message("The input object is not applicable to get an integer sample size.")
    return(x)
  }

  n_analysis <- length(x$analysis$analysis)
  multiply_factor <- x$input$ratio + 1

  if (!is_rd) {
    # Updated events to integer
    event <- x$analysis$event
    if (n_analysis == 1) {
      event_new <- ceiling(event)
    } else {
      event_new <- c(floor(event[1:(n_analysis - 1)]), ceiling(event[n_analysis]))
    }

    # Updated sample size to integer and enroll rates
    sample_size_new <- (ceiling(x$analysis$n[n_analysis] / multiply_factor) * multiply_factor) %>% as.integer()
    enroll_rate <- x$enroll_rate
    enroll_rate_new <- enroll_rate %>%
      mutate(rate = rate * sample_size_new / x$analysis$n[n_analysis])

    # Updated upar
    # If it is spending bounds
    # Scenario 1: information-based spending
    # Scenario 2: calendar-based spending
    if (identical(x$input$upper, gs_b)) {
      upar_new <- x$input$upar
    } else if (identical(x$input$upper, gs_spending_bound)) {
      upar_new <- x$input$upar
      if (!("timing" %in% names(x$input$upar))) {
        info_with_new_event <- gs_info_ahr(
          enroll_rate = enroll_rate_new,
          fail_rate = x$input$fail_rate,
          ratio = x$input$ratio,
          event = event_new,
          analysis_time = NULL
        )

        upar_new$timing <- info_with_new_event$info / max(info_with_new_event$info)
      }
    }

    # Updated lpar
    # If it is spending bounds
    # Scenario 1: information-based spending
    # Scenario 2: calendar-based spending
    if (identical(x$input$lower, gs_b)) {
      lpar_new <- x$input$lpar
    } else if (identical(x$input$lower, gs_spending_bound)) {
      lpar_new <- x$input$lpar
      if (!("timing" %in% names(x$input$lpar))) {
        lpar_new$timing <- upar_new$timing
      }
    }

    # Updated design with integer events and sample size
    power_args <- list(
      enroll_rate = enroll_rate_new,
      fail_rate = x$input$fail_rate,
      event = event_new,
      analysis_time = NULL,
      ratio = x$input$ratio,
      upper = x$input$upper, upar = upar_new,
      lower = x$input$lower, lpar = lpar_new,
      test_upper = x$input$test_upper,
      test_lower = x$input$test_lower,
      binding = x$input$binding,
      info_scale = x$input$info_scale, r = x$input$r, tol = x$input$tol,
      interval = c(0.01, max(x$analysis$time) + 100)
    )
    if (is_wlr) power_args[c("weight", "approx")] <- x$input[c("weight", "approx")]
    x_new <- do.call(if (is_wlr) gs_power_wlr else gs_power_ahr, power_args)
  } else {
    n_stratum <- length(x$input$p_c$stratum)

    # Update unstratified sample size to integer
    sample_size_new <- tibble::tibble(
      analysis = 1:n_analysis,
      n = c(
        floor(x$analysis$n[1:(n_analysis - 1)] / multiply_factor),
        ceiling(x$analysis$n[n_analysis] / multiply_factor)
      ) * multiply_factor
    )

    # Update sample size per stratum
    suppressMessages({
      tbl_n <- tibble::tibble(
        analysis = rep(1:n_analysis, each = n_stratum),
        stratum = rep(x$input$p_c$stratum, n_analysis)
      )
      tbl_n <- if (n_stratum == 1) {
        tbl_n %>%
          left_join(sample_size_new)
      } else {
        tbl_n %>%
          left_join(x$input$stratum_prev) %>%
          left_join(sample_size_new) %>%
          mutate(n_new = prevalence * n) %>%
          select(-c(n, prevalence)) %>%
          dplyr::rename(n = n_new)
      }
    })

    # If it is spending bounds
    # Scenario 1: information-based spending
    # Scenario 2: calendar-based spending
    if (identical(x$input$upper, gs_b)) {
      upar_new <- x$input$upar
    } else if (identical(x$input$upper, gs_spending_bound)) {
      upar_new <- x$input$upar
      if (!("timing" %in% names(x$input$upar))) {
        info_with_new_n <- gs_info_rd(
          p_c = x$input$p_c,
          p_e = x$input$p_e,
          n = tbl_n,
          rd0 = x$input$rd,
          ratio = x$input$ratio,
          weight = x$input$weight
        )

        upar_new$timing <- info_with_new_n$info1 / max(info_with_new_n$info1)
      }
    }

    # Updated lpar
    if (identical(x$input$lower, gs_b)) {
      lpar_new <- x$input$lpar
    } else if (identical(x$input$lower, gs_spending_bound)) {
      lpar_new <- x$input$lpar
      if (!("timing" %in% names(x$input$lpar))) {
        lpar_new$timing <- upar_new$timing
      }
    }

    # Updated design
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
  }

  # Make n and event of x_new$analysis exactly integers
  x_new$analysis$n <- round(x_new$analysis$n)
  if (!is_rd) x_new$analysis$event <- round(x_new$analysis$event)

  return(x_new)
}
