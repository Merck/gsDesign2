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

#' Fixed design sample size
#'
#' Computes fixed design sample size for many sample size methods.
#' Returns a tibble with a basic summary.
#'
#' @param method Sample size method. Default is `"ahr"`.
#'   Other options include `"fh"`, `"mb"`, `"lf"`, `"rd"`,
#'   `"maxcombo"`, `"milestone"`.
#' @param alpha One-sided Type I error (strictly between 0 and 1).
#' @param power Power (`NULL` to compute power or strictly between 0
#'   and `1 - alpha` otherwise).
#' @param ratio Experimental:Control randomization ratio.
#' @param study_duration Study duration.
#' @param ... Additional arguments like `enroll_rate`, `fail_rate`,
#'   `rho`, `gamma`, `tau`.
#'
#' @return A table.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Average hazard ratio
#' x <- fixed_design(
#'   "ahr",
#'   alpha = .025, power = .9,
#'   enroll_rate = tibble::tibble(stratum = "All", duration = 18, rate = 1),
#'   fail_rate = tibble::tibble(
#'     stratum = "All",
#'     duration = c(4, 100),
#'     fail_rate = log(2) / 12,
#'     hr = c(1, .6),
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36
#' )
#' x %>% summary()
#'
#' # Lachin and Foulkes (uses gsDesign::nSurv())
#' x <- fixed_design(
#'   "lf",
#'   alpha = .025, power = .9,
#'   enroll_rate = tibble::tibble(stratum = "All", duration = 18, rate = 1),
#'   fail_rate = tibble::tibble(
#'     stratum = "All",
#'     duration = 100,
#'     fail_rate = log(2) / 12,
#'     hr = .7,
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36
#' )
#' x %>% summary()
#'
#' # RMST
#' x <- fixed_design(
#'   "rmst",
#'   alpha = .025, power = .9,
#'   enroll_rate = tibble::tibble(stratum = "All", duration = 18, rate = 1),
#'   fail_rate = tibble::tibble(
#'     stratum = "All",
#'     duration = 100,
#'     fail_rate = log(2) / 12,
#'     hr = .7,
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36,
#'   tau = 18
#' )
#' x %>% summary()
#'
#' # Milestone
#' x <- fixed_design(
#'   "milestone",
#'   alpha = .025, power = .9,
#'   enroll_rate = tibble::tibble(stratum = "All", duration = 18, rate = 1),
#'   fail_rate = tibble::tibble(
#'     stratum = "All",
#'     duration = 100,
#'     fail_rate = log(2) / 12,
#'     hr = .7,
#'     dropout_rate = .001
#'   ),
#'   study_duration = 36,
#'   tau = 18
#' )
#' x %>% summary()
fixed_design <- function(method = c("ahr", "fh", "mb", "lf", "rd", "maxcombo", "rmst", "milestone"),
                         alpha = 0.025,
                         power = NULL,
                         ratio = 1,
                         study_duration = 36,
                         ...) {
  # --------------------------------------------- #
  #     check inputs                              #
  # --------------------------------------------- #
  x <- match.arg(method)
  args <- list(...)

  has_weight <- "weight" %in% names(args)
  has_rho <- "rho" %in% names(args)
  has_gamma <- "gamma" %in% names(args)
  has_tau <- "tau" %in% names(args)
  has_enroll_rate <- "enroll_rate" %in% names(args)
  has_fail_rate <- "fail_rate" %in% names(args)
  has_event <- "event" %in% names(args)
  has_n <- "n" %in% names(args)

  # ------------------------- #
  #     check inputs          #
  # ------------------------- #
  # check enrollment rate (not expected for RD)
  if (!has_enroll_rate && x != "rd") {
    stop("fixed_design: please input enroll_rate!")
  } else {
    enroll_rate <- args$enroll_rate
  }

  # check failure rate (not expected for RD)
  if (!has_fail_rate && x != "rd") {
    stop("fixed_design: please input fail_rate!")
  } else {
    fail_rate <- args$fail_rate
  }

  # check test parameters, like rho, gamma, tau
  if (has_rho && length(args$rho) > 1 && x %in% c("fh", "mb")) {
    stop("fixed_design: multiple rho can not be used in Fleming-Harrington or Magirr-Burman method!")
  }
  if (has_gamma && length(args$gamma) > 1 && x %in% c("fh", "mb")) {
    stop("fixed_design: multiple gamma can not be used in Fleming-Harrington or Magirr-Burman method!")
  }
  if (has_tau && length(args$tau) > 1 && x %in% c("fh", "mb")) {
    stop("fixed_design: multiple tau can not be used in Fleming-Harrington or Magirr-Burman method!")
  }
  if (has_tau && x == "fh") {
    stop("fixed_design: tau is not needed for Fleming-Harrington (FH) method!")
  }
  if (has_rho && has_gamma && x == "mb") {
    stop("fixed_design: rho and gamma are not needed for Magirr-Burman (MB) method!")
  }

  # check inputs necessary for RD
  if (x == "rd") {
    if (!"p_c" %in% names(args)) {
      stop("fixed_design: p_c is needed for RD!")
    }
    if (!"p_e" %in% names(args)) {
      stop("fixed_design: p_e is needed for RD!")
    }
    if (!"rd0" %in% names(args)) {
      stop("fixed_design: rd0 is needed for RD!")
    }
    if (is.null(power) && !has_n) {
      stop("fixed_design: sample size n = ... is needed for RD!")
    }
  }

  # ------------------------- #
  #     save inputs           #
  # ------------------------- #
  input <- list(
    alpha = alpha, power = power, ratio = ratio, study_duration = study_duration,
    weight = if (has_weight) {
      args$weight
    } else {
      NULL
    },
    rho = if (has_rho) {
      args$rho
    } else {
      NULL
    },
    gamma = if (has_gamma) {
      args$gamma
    } else {
      NULL
    },
    tau = if (has_tau) {
      args$tau
    } else {
      NULL
    },
    enroll_rate = if (has_enroll_rate) {
      args$enroll_rate
    } else {
      NULL
    },
    fail_rate = if (has_fail_rate) {
      args$fail_rate
    } else {
      NULL
    },
    n = if (has_n) {
      args$n
    } else {
      NULL
    }
  )

  # ------------------------- #
  #     generate design       #
  # ------------------------- #
  y <- switch(x,
    "ahr" = {
      if (!is.null(power)) {
        d <- gs_design_ahr(
          alpha = alpha, beta = 1 - power,
          upar = qnorm(1 - alpha), lpar = -Inf,
          enroll_rate = enroll_rate,
          fail_rate = fail_rate,
          ratio = ratio,
          analysis_time = study_duration
        )
      } else {
        d <- gs_power_ahr(
          upar = qnorm(1 - alpha), lpar = -Inf,
          enroll_rate = enroll_rate,
          fail_rate = fail_rate,
          ratio = ratio,
          analysis_time = study_duration,
          event = if (has_event) {
            args$event
          } else {
            NULL
          }
        )
      }
      ans <- tibble::tibble(
        design = "ahr",
        n = d$analysis$n,
        event = d$analysis$event,
        time = d$analysis$time,
        bound = (d$bound %>% filter(bound == "upper"))$z,
        alpha = alpha,
        power = (d$bound %>% filter(bound == "upper"))$probability
      )

      list(
        input = input, enroll_rate = d$enroll_rate,
        fail_rate = d$fail_rate, analysis = ans, design = "ahr"
      )
    },
    "fh" = {
      if (has_weight + has_rho + has_gamma == 0) {
        weight <- function(x, arm0, arm1) {
          wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 0.5)
        }
      }
      if (has_weight == 0 & has_rho + has_gamma >= 1) {
        weight <- function(x, arm0, arm1) {
          wlr_weight_fh(x, arm0, arm1,
            rho = ifelse(has_rho, args$rho, 0),
            gamma = ifelse(has_gamma, args$gamma, 0.5)
          )
        }
      }
      if (!is.null(power)) {
        d <- gs_design_wlr(
          alpha = alpha, beta = 1 - power,
          upar = qnorm(1 - alpha), lpar = -Inf,
          enroll_rate = enroll_rate,
          fail_rate = fail_rate,
          ratio = ratio,
          weight = weight,
          analysis_time = study_duration
        )
      } else {
        d <- gs_power_wlr(
          upar = qnorm(1 - alpha), lpar = -Inf,
          enroll_rate = enroll_rate,
          fail_rate = fail_rate,
          ratio = ratio,
          weight = weight,
          analysis_time = study_duration,
          event = NULL
        )
      }
      ans <- tibble::tibble(
        design = "fh",
        n = d$analysis$n,
        event = d$analysis$event,
        time = d$analysis$time,
        bound = (d$bound %>% filter(bound == "upper"))$z,
        alpha = alpha,
        power = (d$bound %>% filter(bound == "upper"))$probability
      )

      list(
        input = input, enroll_rate = d$enroll_rate, fail_rate = d$fail_rate,
        analysis = ans,
        design = "fh", design_par = list(
          rho = if (has_rho) {
            args$rho
          } else {
            0
          },
          gamma = if (has_gamma) {
            args$gamma
          } else {
            0.5
          }
        )
      )
    },
    "mb" = {
      # check if power is NULL or not
      if (!is.null(power)) {
        d <- gs_design_wlr(
          alpha = alpha,
          beta = 1 - power,
          enroll_rate = enroll_rate,
          fail_rate = fail_rate,
          ratio = 1,
          weight = function(x, arm0, arm1) {
            wlr_weight_fh(x, arm0, arm1,
              rho = -1, gamma = 0,
              tau = ifelse(has_tau, args$tau, 6)
            )
          },
          upper = gs_b,
          upar = qnorm(1 - alpha),
          lower = gs_b,
          lpar = -Inf,
          analysis_time = study_duration
        )
      } else {
        d <- gs_power_wlr(
          enroll_rate = enroll_rate,
          fail_rate = fail_rate,
          ratio = 1,
          weight = function(x, arm0, arm1) {
            wlr_weight_fh(x, arm0, arm1,
              rho = -1, gamma = 0,
              tau = ifelse(has_tau, args$tau, 6)
            )
          },
          upper = gs_b,
          upar = qnorm(1 - alpha),
          lower = gs_b,
          lpar = -Inf,
          analysis_time = study_duration,
          event = NULL
        )
      }

      # get the output of MB
      ans <- tibble::tibble(
        design = "mb",
        n = d$analysis$n,
        event = d$analysis$event,
        time = d$analysis$time,
        bound = (d$bound %>% filter(bound == "upper"))$z,
        alpha = alpha,
        power = (d$bound %>% filter(bound == "upper"))$probability
      )

      list(
        input = input, enroll_rate = d$enroll_rate, fail_rate = d$fail_rate, analysis = ans,
        design = "mb", design_par = list(tau = ifelse(has_tau, args$tau, 6))
      )
    },
    "lf" = {
      # check if it is stratum
      if (length(unique(enroll_rate$stratum)) != 1 | length(unique(fail_rate$stratum)) != 1) {
        warning("Lachin-Foulkes is not recommended for stratified designs!")
      }

      # calculate the S: duration of piecewise constant event rates
      m <- length(fail_rate$fail_rate)
      if (m == 1) {
        ss <- NULL
      } else {
        ss <- fail_rate$duration[1:(m - 1)]
      }

      # calculate the ahr as the hr in nSurv
      dd <- ahr(enroll_rate = enroll_rate, fail_rate = fail_rate, total_duration = study_duration, ratio = ratio)

      # use nSuve to develop the design
      d <- gsDesign::nSurv(
        alpha = alpha, beta = if (is.null(power)) {
          NULL
        } else {
          1 - power
        },
        ratio = ratio, hr = dd$ahr,
        # fail_rate
        lambdaC = fail_rate$fail_rate,
        S = ss, eta = fail_rate$dropout_rate,
        # enroll_rate
        gamma = enroll_rate$rate, R = enroll_rate$duration,
        T = study_duration, minfup = study_duration - sum(enroll_rate$duration)
      )

      ans <- tibble::tibble(
        design = "lf",
        n = d$n,
        event = d$d,
        time = d$T,
        bound = qnorm(1 - alpha),
        alpha = d$alpha,
        power = d$power
      )

      list(
        input = input,
        enroll_rate = enroll_rate %>% mutate(rate = rate * d$n / sum(enroll_rate$duration * enroll_rate$rate)),
        fail_rate = fail_rate,
        analysis = ans,
        design = "lf"
      )
    },
    "maxcombo" = {
      # organize the tests in MaxCombo
      max_combo_test <- data.frame(
        rho = if (has_rho) {
          args$rho
        } else {
          c(0, 0)
        },
        gamma = if (has_gamma) {
          args$gamma
        } else {
          c(0, 0.5)
        },
        tau = if (has_tau) {
          args$tau
        } else {
          -1
        }
      ) %>%
        mutate(test = seq(1, length(rho)), analysis = 1, analysis_time = study_duration)

      # check if power is NULL or not
      if (!is.null(power)) {
        d <- gs_design_combo(
          alpha = alpha, beta = 1 - power, ratio = ratio,
          enroll_rate = enroll_rate,
          fail_rate = fail_rate,
          fh_test = max_combo_test,
          upper = gs_b, upar = qnorm(1 - alpha),
          lower = gs_b, lpar = -Inf
        )
      } else {
        d <- gs_power_combo(
          ratio = ratio,
          enroll_rate = enroll_rate,
          fail_rate = fail_rate,
          fh_test = max_combo_test,
          upper = gs_b, upar = qnorm(1 - alpha),
          lower = gs_b, lpar = -Inf
        )
      }

      # get the output of MaxCombo
      ans <- tibble::tibble(
        design = "maxcombo",
        n = d$analysis$n,
        event = d$analysis$event,
        time = d$analysis$time,
        bound = (d$bound %>% filter(bound == "upper"))$z,
        alpha = alpha,
        power = (d$bound %>% filter(bound == "upper"))$probability
      )

      list(
        input = input,
        enroll_rate = d$enroll_rate, fail_rate = d$fail_rate, analysis = ans,
        design = "maxcombo", design_par = list(
          rho = if (has_rho) {
            args$rho
          } else {
            c(0, 0)
          },
          gamma = if (has_gamma) {
            args$gamma
          } else {
            c(0, 0.5)
          },
          tau = if (has_tau) {
            args$tau
          } else {
            c(-1, -1)
          }
        )
      )
    },
    "rd" = {
      if (!is.null(power)) {
        d <- gs_design_rd(
          p_c = tibble::tibble(stratum = "all", rate = args$p_c),
          p_e = tibble::tibble(stratum = "all", rate = args$p_e),
          alpha = alpha, beta = 1 - power, ratio = ratio,
          upper = gs_b, upar = qnorm(1 - alpha),
          lower = gs_b, lpar = -Inf,
          rd0 = args$rd0, weight = "unstratified"
        )
      } else {
        d <- gs_power_rd(
          p_c = tibble::tibble(stratum = "all", rate = args$p_c),
          p_e = tibble::tibble(stratum = "all", rate = args$p_e),
          ratio = ratio,
          upper = gs_b, upar = qnorm(1 - alpha),
          lower = gs_b, lpar = -Inf,
          n = tibble::tibble(stratum = "all", n = args$n, analysis = 1),
          rd0 = args$rd0, weight = "unstratified"
        )
      }

      # get the output of MaxCombo
      ans <- tibble::tibble(
        design = "rd",
        n = d$analysis$n,
        bound = (d$bound %>% filter(bound == "upper"))$z,
        alpha = alpha,
        power = (d$bound %>% filter(bound == "upper"))$probability
      )

      list(
        input = input,
        enroll_rate = d$enroll_rate, fail_rate = d$fail_rate, analysis = ans, design = "rd"
      )
    },
    "rmst" = {
      if (!is.null(power)) {
        d <- fixed_design_size_rmst(
          alpha = alpha, beta = 1 - power, ratio = ratio,
          enroll_rate = enroll_rate, fail_rate = fail_rate,
          analysis_time = study_duration,
          test = "rmst difference",
          tau = ifelse(has_tau, args$tau, study_duration)
        )
      } else {
        d <- fixed_design_power_rmst(
          alpha = alpha, ratio = ratio,
          enroll_rate = enroll_rate, fail_rate = fail_rate,
          analysis_time = study_duration,
          test = "rmst difference",
          tau = ifelse(has_tau, args$tau, study_duration)
        )
      }

      # get the output
      ans <- tibble::tibble(
        design = "rmst",
        n = d$analysis$n,
        event = d$analysis$event,
        time = d$analysis$time,
        bound = (d$bound %>% filter(bound == "upper"))$z,
        alpha = alpha,
        power = (d$bound %>% filter(bound == "upper"))$probability
      )

      list(
        input = input,
        enroll_rate = d$enroll_rate, fail_rate = d$fail_rate, analysis = ans,
        design = "rmst", design_par = list(tau = ifelse(has_tau, args$tau, study_duration))
      )
    },
    "milestone" = {
      if (!is.null(power)) {
        d <- fixed_design_size_rmst(
          alpha = alpha, beta = 1 - power, ratio = ratio,
          enroll_rate = enroll_rate, fail_rate = fail_rate,
          analysis_time = study_duration,
          test = "survival difference",
          tau = ifelse(has_tau, args$tau, study_duration)
        )
      } else {
        d <- fixed_design_power_rmst(
          alpha = alpha, ratio = ratio,
          enroll_rate = enroll_rate, fail_rate = fail_rate,
          analysis_time = study_duration,
          test = "survival difference",
          tau = ifelse(has_tau, args$tau, study_duration)
        )
      }

      # get the output of MaxCombo
      ans <- tibble::tibble(
        design = "milestone",
        n = d$analysis$n,
        event = d$analysis$event,
        time = d$analysis$time,
        bound = (d$bound %>% filter(bound == "upper"))$z,
        alpha = alpha,
        power = (d$bound %>% filter(bound == "upper"))$probability
      )

      list(
        input = input,
        enroll_rate = d$enroll_rate, fail_rate = d$fail_rate, analysis = ans,
        design = "milestone", design_par = list(tau = ifelse(has_tau, args$tau, study_duration))
      )
    }
  )

  class(y) <- c("fixed_design", class(y))
  return(y)
}
