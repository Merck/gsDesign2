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

#' Round sample size and events
#'
#' @param x An object returned by fixed_design_xxx() and gs_design_xxx().
#' @param ... Additional parameters (not used).
#' @details
#' For the sample size of the fixed design:
#' - When `ratio` is a positive integer, the sample size is rounded up to a multiple of `ratio + 1`
#' if `round_up_final = TRUE`, and just rounded to a multiple of `ratio + 1` if `round_up_final = FALSE`.
#' - When `ratio` is a positive non-integer, the sample size is rounded up if `round_up_final = TRUE`,
#' (may not be a multiple of `ratio + 1`), and just rounded if `round_up_final = FALSE` (may not be a multiple of `ratio + 1`).
#' Note the default `ratio` is taken from `x$input$ratio`.
#'
#' For the number of events of the fixed design:
#' - If the continuous event is very close to an integer within 0.01 differences, say 100.001 or 99.999, then the integer events is 100.
#' - Otherwise, round up if `round_up_final = TRUE` and round if `round_up_final = FALSE`.
#'
#' For the sample size of group sequential designs:
#' - When `ratio` is a positive integer, the final sample size is rounded to a multiple of `ratio + 1`.
#'   + For 1:1 randomization (experimental:control), set `ratio = 1` to round to an even sample size.
#'   + For 2:1 randomization, set `ratio = 2` to round to a multiple of 3.
#'   + For 3:2 randomization, set `ratio = 4` to round to a multiple of 5.
#'   + Note that for the final analysis, the sample size is rounded up to the nearest multiple of `ratio + 1` if `round_up_final = TRUE`.
#'   If `round_up_final = FALSE`, the final sample size is rounded to the nearest multiple of `ratio + 1`.
#' - When `ratio` is positive non-integer, the final sample size MAY NOT be rounded to a multiple of `ratio + 1`.
#'   + The final sample size is rounded up if `round_up_final = TRUE`.
#'   + Otherwise, it is just rounded.
#'
#' For the events of group sequential designs:
#' - For events at interim analysis, it is rounded.
#' - For events at final analysis:
#'   + If the continuous event is very close to an integer within 0.01 differences, say 100.001 or 99.999, then the integer events is 100.
#'   + Otherwise, final events is rounded up if `round_up_final = TRUE` and rounded if `round_up_final = FALSE`.
#'
#' @return A list similar to the output of fixed_design_xxx() and gs_design_xxx(),
#'   except the sample size is an integer.
#'
#' @export to_integer
to_integer <- function(x, ...) {
  UseMethod("to_integer", x)
}

#' @rdname to_integer
#' @export
#'
#' @examples
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
#'   tau = Inf, w_max = 2,
#'   study_duration = 36, ratio = 1
#' )
#' x |>
#'   to_integer() |>
#'   summary()
#' }
to_integer.fixed_design <- function(x, round_up_final = TRUE, ratio = x$input$ratio, ...) {

  if (ratio < 0) {
    stop("The ratio must be non-negative.")
  }

  if (!is_wholenumber(ratio)) {
    message("The output sample size is just rounded, may not a multiple of (ratio + 1).")
  }

  output_n <- x$analysis$n
  input_n <- expected_accrual(time = x$analysis$time, enroll_rate = x$input$enroll_rate)

  multiply_factor <- ratio + 1
  ss <- output_n / multiply_factor
  if (is_wholenumber(ratio)) {
    if (round_up_final) {
      sample_size_new <- ceiling(ss) * multiply_factor
    } else {
      sample_size_new <- round(ss, 0) * multiply_factor
    }
  } else {
    if (round_up_final) {
      sample_size_new <- ceiling(output_n)
    } else {
      sample_size_new <- round(output_n, 0)
    }
  }

  enroll_rate_new <- x$enroll_rate |>
    mutate(rate = rate * sample_size_new / output_n)

  # Round events
  # if events is very close to an integer, set it as this integer
  if (abs(x$analysis$event - round(x$analysis$event)) < 0.01) {
    event_new <- round(x$analysis$event)
    # ceiling the FA events as default
  } else if (round_up_final) {
    event_new <- ceiling(x$analysis$event)
    # otherwise, round the FA events
  } else{
    event_new <- round(x$analysis$event, 0)
  }

  if ((x$design == "ahr") && (input_n != output_n)) {
    x_new <- gs_power_ahr(
      enroll_rate = enroll_rate_new,
      fail_rate = x$input$fail_rate,
      event = event_new,
      analysis_time = NULL,
      ratio = x$input$ratio,
      upper = gs_b, lower = gs_b,
      upar = qnorm(1 - x$input$alpha), lpar = -Inf
    )

    analysis <- tibble(
      design = "ahr",
      n = x_new$analysis$n,
      event = x_new$analysis$event,
      time = x_new$analysis$time,
      ahr = x_new$analysis$ahr,
      bound = (x_new$bound |> filter(bound == "upper"))$z,
      alpha = x$input$alpha,
      power = (x_new$bound |> filter(bound == "upper"))$probability
    )

    ans <- structure(
      list(
        input = x$input, enroll_rate = x_new$enroll_rate, fail_rate = x_new$fail_rate,
        analysis = analysis, design = "ahr"
      ),
      class = "fixed_design",
      design_display = attr(x, "design_display"),
      title = attr(x, "title"),
      footnote = attr(x, "footnote")
    )
  } else if ((x$design == "fh") && (input_n != output_n)) {
    x_new <- gs_power_wlr(
      enroll_rate = enroll_rate_new,
      fail_rate = x$input$fail_rate,
      event = event_new,
      analysis_time = NULL,
      ratio = x$input$ratio,
      upper = gs_b, lower = gs_b,
      upar = qnorm(1 - x$input$alpha), lpar = -Inf,
      weight = list(method = "fh", param = list(rho = x$design_par$rho, gamma = x$design_par$gamma))
    )

    analysis <- tibble(
      design = "fh",
      n = x_new$analysis$n,
      event = x_new$analysis$event,
      time = x_new$analysis$time,
      ahr = x_new$analysis$ahr,
      bound = (x_new$bound |> filter(bound == "upper"))$z,
      alpha = x$input$alpha,
      power = (x_new$bound |> filter(bound == "upper"))$probability
    )

    ans <- structure(
      list(
        input = x$input, enroll_rate = x_new$enroll_rate, fail_rate = x_new$fail_rate,
        analysis = analysis, design = "fh", design_par = x$design_par
      ),
      class = "fixed_design",
      design_display = attr(x, "design_display"),
      title = attr(x, "title"),
      footnote = attr(x, "footnote")
    )
  } else if ((x$design == "mb") && (input_n != output_n)) {
    x_new <- gs_power_wlr(
      enroll_rate = enroll_rate_new,
      fail_rate = x$input$fail_rate,
      event = event_new,
      analysis_time = NULL,
      ratio = x$input$ratio,
      weight = list(method = "mb", param = list(tau = x$design_par$tau, w_max = x$design_par$w_max)),
      upper = gs_b, lower = gs_b,
      upar = qnorm(1 - x$input$alpha), lpar = -Inf
    )

    analysis <- tibble(
      design = "mb",
      n = x_new$analysis$n,
      event = x_new$analysis$event,
      time = x_new$analysis$time,
      ahr = x_new$analysis$ahr,
      bound = (x_new$bound |> filter(bound == "upper"))$z,
      alpha = x$input$alpha,
      power = (x_new$bound |> filter(bound == "upper"))$probability
    )

    ans <- structure(
      list(
        input = x$input, enroll_rate = x_new$enroll_rate, fail_rate = x_new$fail_rate,
        analysis = analysis, design = "mb", design_par = x$design_par
      ),
      class = "fixed_design",
      design_display = attr(x, "design_display"),
      title = attr(x, "title"),
      footnote = attr(x, "footnote")
    )
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
#' @param round_up_final Events at final analysis is rounded up if `TRUE`;
#' otherwise, just rounded, unless it is very close to an integer.
#' @param ratio Positive integer for randomization ratio (experimental:control).
#' A positive integer will result in rounded sample size, which is a multiple of (ratio + 1).
#' A positive non-integer will result in round sample size, which may not be a multiple of (ratio + 1).
#' A negative number will result in an error.
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
to_integer.gs_design <- function(x, round_up_final = TRUE, ratio = x$input$ratio, ...) {
  is_ahr <- x$design == "ahr"
  is_wlr <- x$design == "wlr"
  is_rd  <- x$design == "rd"
  if (!(is_ahr || is_wlr || is_rd)) {
    message("The input object is not applicable to get an integer sample size.")
    return(x)
  }

  if (ratio < 0) {
    stop("The ratio must be non-negative.")
  }

  if (!is_wholenumber(ratio)) {
    message("The output sample size is just rounded, may not a multiple of (ratio + 1).")
  }

  n_analysis <- length(x$analysis$analysis)
  multiply_factor <- ratio + 1

  if (!is_rd) {
    # Updated events to integer
    event <- x$analysis$event
    if (n_analysis == 1) {
      event_new <- ceiling(event)
    } else {
      # round IA events to the closest integer
      event_ia_new <- round(event[1:(n_analysis - 1)])

      # if the FA events is very close to an integer, set it as this integer
      if (abs(event[n_analysis] - round(event[n_analysis])) < 0.01) {
        event_fa_new <- round(event[n_analysis])
      # ceiling the FA events as default
      } else if (round_up_final) {
        event_fa_new <- ceiling(event[n_analysis])
      # otherwise, round the FA events
      } else{
        event_fa_new <- round(event[n_analysis], 0)
      }

      event_new <- c(event_ia_new, event_fa_new)
    }

    # Updated sample size to integer and enroll rates
    # if the randomization ratio is a whole number, round the sample size as a multiplier of ratio + 1
    if(is_wholenumber(ratio)) {
      ss <- x$analysis$n[n_analysis] / multiply_factor

      if (round_up_final) {
        sample_size_new <- ceiling(ss) * multiply_factor
      } else {
        sample_size_new <- round(ss, 0) * multiply_factor
      }
    # if the randomization ratio is NOT a whole number, just round it
    } else {
      if (round_up_final) {
        sample_size_new <- ceiling(x$analysis$n[n_analysis])
      } else {
        sample_size_new <- round(x$analysis$n[n_analysis], 0)
      }
    }

    sample_size_new <- as.integer(sample_size_new)

    enroll_rate <- x$enroll_rate
    enroll_rate_new <- enroll_rate |>
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
        if (is_ahr) {
          info_with_new_event <- gs_info_ahr(
            enroll_rate = enroll_rate_new,
            fail_rate = x$input$fail_rate,
            ratio = ratio,
            event = event_new,
            analysis_time = NULL
          )
        } else if (is_wlr) {
          info_with_new_event <- gs_info_wlr(
            enroll_rate = enroll_rate_new,
            fail_rate = x$input$fail_rate,
            ratio = ratio,
            event = event_new,
            analysis_time = NULL,
            weight = x$input$weight
          )
        }

        # ensure info0 is based on integer sample size calculation
        # as as they become a slight different number due to the `enroll_rate`
        if (is_ahr) {
          q_e <- ratio / (1 + ratio)
          q_c <- 1 - q_e
          info_with_new_event$info0 <- event_new * q_e * q_c

          # ensure info is based on integer sample size calculation
          # as as they become a slight different number due to the `enroll_rate`
          q <- event_new / event
          info_with_new_event$info <- x$analysis$info * q
        }

        # update timing
        upar_new$timing <- info_with_new_event$info0 / max(info_with_new_event$info0)
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
      ratio = ratio,
      upper = x$input$upper, upar = upar_new,
      lower = x$input$lower, lpar = lpar_new,
      test_upper = x$input$test_upper,
      test_lower = x$input$test_lower,
      binding = x$input$binding,
      info_scale = x$input$info_scale, r = x$input$r, tol = x$input$tol,
      interval = c(0.01, max(x$analysis$time) + 100),
      integer = TRUE
    )
    if (is_ahr) power_args["h1_spending"] <- x$input["h1_spending"]
    if (is_wlr) power_args[c("weight", "approx")] <- x$input[c("weight", "approx")]
    x_new <- do.call(if (is_wlr) gs_power_wlr else gs_power_ahr, power_args)
  } else {
    n_stratum <- length(x$input$p_c$stratum)

    # Update unstratified sample size to integer
    sample_size_new_ia <- round(x$analysis$n[1:(n_analysis - 1)], 0)
    if (round_up_final) {
      if (is_wholenumber(ratio)) {
        sample_size_new_fa <- ceiling(x$analysis$n[n_analysis] / multiply_factor) * multiply_factor
      } else {
        sample_size_new_fa <- ceiling(x$analysis$n[n_analysis])
      }
    } else {
      if (is_wholenumber(ratio)) {
        sample_size_new_fa <- round(x$analysis$n[n_analysis] / multiply_factor, 0) * multiply_factor
      } else {
        sample_size_new_fa <- round(x$analysis$n[n_analysis], 0)
      }
    }

    sample_size_new <- tibble(
      analysis = 1:n_analysis,
      n = c(sample_size_new_ia, sample_size_new_fa)
    )

    # Update sample size per stratum
    suppressMessages({
      tbl_n <- tibble(
        analysis = rep(1:n_analysis, each = n_stratum),
        stratum = rep(x$input$p_c$stratum, n_analysis)
      )
      tbl_n <- if (n_stratum == 1) {
        tbl_n |>
          left_join(sample_size_new)
      } else {
        tbl_n |>
          left_join(x$input$stratum_prev) |>
          left_join(sample_size_new) |>
          mutate(n_new = prevalence * n) |>
          select(-c(n, prevalence)) |>
          rename(n = n_new)
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
          ratio = ratio,
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
      ratio = ratio,
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

  # Add attributes to x_new to identify whether it is a gs_design_ahr orbject or gs_power_ahr object
  if ("analysis_time" %in% names(x$input) && "info_frac" %in% names(x$input) && "ahr" %in% class(x)) {
    attr(x_new, 'uninteger_is_from') <- "gs_design_ahr"
  } else if ("analysis_time" %in% names(x$input) && "event" %in% names(x$input) && "ahr" %in% class(x)) {
    attr(x_new, 'uninteger_is_from') <- "gs_power_ahr"
  } else if ("analysis_time" %in% names(x$input) && "info_frac" %in% names(x$input) && "wlr" %in% class(x)) {
    attr(x_new, 'uninteger_is_from') <- "gs_design_wlr"
  } else if ("analysis_time" %in% names(x$input) && "event" %in% names(x$input) && "wlr" %in% class(x)) {
    attr(x_new, 'uninteger_is_from') <- "gs_power_wlr"
  } else if (!("n" %in% names(x$input)) && "rd" %in% class(x)) {
    attr(x_new, 'uninteger_is_from') <- "gs_design_rd"
  } else if ("n" %in% names(x$input) && "rd" %in% class(x)) {
    attr(x_new, 'uninteger_is_from') <- "gs_power_rd"
  }

  return(x_new)
}
