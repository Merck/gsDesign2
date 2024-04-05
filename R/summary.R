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

#' Summary for fixed design or group sequential design objects
#'
#' @param object A design object returned by fixed_design_xxx() and gs_design_xxx().
#' @param ... Additional parameters (not used).
#'
#' @return A summary table (data frame).
#'
#' @export
#'
#' @rdname summary
#'
#' @examples
#' library(dplyr)
#'
#' # Enrollment rate
#' enroll_rate <- define_enroll_rate(
#'   duration = 18,
#'   rate = 20
#' )
#'
#' # Failure rates
#' fail_rate <- define_fail_rate(
#'   duration = c(4, 100),
#'   fail_rate = log(2) / 12,
#'   hr = c(1, .6),
#'   dropout_rate = .001
#' )
#'
#' # Study duration in months
#' study_duration <- 36
#'
#' # Experimental / Control randomization ratio
#' ratio <- 1
#'
#' # 1-sided Type I error
#' alpha <- 0.025
#' # Type II error (1 - power)
#' beta <- 0.1
#'
#' # AHR ----
#' # under fixed power
#' fixed_design_ahr(
#'   alpha = alpha,
#'   power = 1 - beta,
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   study_duration = study_duration,
#'   ratio = ratio
#' ) %>% summary()
#'
#' # FH ----
#' # under fixed power
#' fixed_design_fh(
#'   alpha = alpha,
#'   power = 1 - beta,
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   study_duration = study_duration,
#'   ratio = ratio
#' ) %>% summary()
#'
summary.fixed_design <- function(object, ...) {
  x <- object
  x_design <- switch(x$design,
    "ahr" = {
      "Average hazard ratio"
    },
    "lf" = {
      "Lachin and Foulkes"
    },
    "rd" = {
      "Risk difference"
    },
    "milestone" = {
      paste0("Milestone: tau = ", x$design_par$tau)
    },
    "rmst" = {
      paste0("RMST: tau = ", x$design_par$tau)
    },
    "mb" = {
      paste0("Modestly weighted LR: tau = ", x$design_par$tau)
    },
    "fh" = {
      if (x$design_par$rho == 0 & x$design_par$gamma == 0) {
        paste0("Fleming-Harrington FH(0, 0) (logrank)")
      } else {
        paste0("Fleming-Harrington FH(", x$design_par$rho, ", ", x$design_par$gamma, ")")
      }
    },
    "maxcombo" = {
      temp <- paste0(
        "MaxCombo: FH(",
        paste(apply(do.call(rbind, x$design_par[c(1:2)]), 2, paste, collapse = ", "), collapse = "), FH("),
        ")"
      )
      gsub(pattern = "FH\\(0, 0\\)", replacement = "logrank", x = temp)
    }
  )

  ans <- x$analysis %>% mutate(design = x_design)
  ans <- ans %>% dplyr::rename(Design = design)

  if ("n" %in% names(ans)) {
    ans <- ans %>% dplyr::rename(N = n)
  }

  if ("event" %in% names(ans)) {
    ans <- ans %>% dplyr::rename(Events = event)
  }

  if ("time" %in% names(ans)) {
    ans <- ans %>% dplyr::rename(Time = time)
  }

  if ("bound" %in% names(ans)) {
    ans <- ans %>% dplyr::rename(Bound = bound)
  }

  if ("power" %in% names(ans)) {
    ans <- ans %>% dplyr::rename(Power = power)
  }

  class(ans) <- c("fixed_design", x$design, class(ans))
  return(ans)
}


#' @rdname summary
#'
#' @param analysis_vars The variables to be put at the summary header of each analysis.
#' @param analysis_decimals The displayed number of digits of `analysis_vars`.
#' @param col_vars The variables to be displayed.
#' @param col_decimals The decimals to be displayed for the displayed variables in `col_vars`.
#' @param bound_names Names for bounds; default is `c("Efficacy", "Futility")`.
#'
#' @importFrom dplyr all_of
#'
#' @export
#'
#' @examples
#' # Design parameters ----
#' library(gsDesign)
#' library(gsDesign2)
#' library(dplyr)
#'
#' # enrollment/failure rates
#' enroll_rate <- define_enroll_rate(
#'   stratum = "All",
#'   duration = 12,
#'   rate = 1
#' )
#' fail_rate <- define_fail_rate(
#'   duration = c(4, 100),
#'   fail_rate = log(2) / 12,
#'   hr = c(1, .6),
#'   dropout_rate = .001
#' )
#'
#' # Information fraction
#' info_frac <- (1:3) / 3
#'
#' # Analysis times in months; first 2 will be ignored as info_frac will not be achieved
#' analysis_time <- c(.01, .02, 36)
#'
#' # Experimental / Control randomization ratio
#' ratio <- 1
#'
#' # 1-sided Type I error
#' alpha <- 0.025
#'
#' # Type II error (1 - power)
#' beta <- .1
#'
#' # Upper bound
#' upper <- gs_spending_bound
#' upar <- list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
#'
#' # Lower bound
#' lower <- gs_spending_bound
#' lpar <- list(sf = gsDesign::sfHSD, total_spend = 0.1, param = 0, timing = NULL)
#'
#' # weight function in WLR
#' wgt00 <- function(x, arm0, arm1) {
#'   wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 0)
#' }
#' wgt05 <- function(x, arm0, arm1) {
#'   wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = .5)
#' }
#'
#' # test in COMBO
#' fh_test <- rbind(
#'   data.frame(rho = 0, gamma = 0, tau = -1, test = 1, analysis = 1:3, analysis_time = c(12, 24, 36)),
#'   data.frame(rho = c(0, 0.5), gamma = 0.5, tau = -1, test = 2:3, analysis = 3, analysis_time = 36)
#' )
#'
#' # Example 1 ----
#' \donttest{
#' x_ahr <- gs_design_ahr(
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   info_frac = info_frac, # Information fraction
#'   analysis_time = analysis_time,
#'   ratio = ratio,
#'   alpha = alpha,
#'   beta = beta,
#'   upper = upper,
#'   upar = upar,
#'   lower = lower,
#'   lpar = lpar
#' )
#'
#' x_ahr %>% summary()
#'
#' # Customize the digits to display
#' x_ahr %>% summary(analysis_vars = c("time", "event", "info_frac"), analysis_decimals = c(1, 0, 2))
#'
#' # Customize the labels of the crossing probability
#' x_ahr %>% summary(bound_names = c("A is better", "B is better"))
#'
#' # Customize the variables to be summarized for each analysis
#' x_ahr %>% summary(analysis_vars = c("n", "event"), analysis_decimals = c(1, 1))
#' }
#'
#' # Example 2 ----
#' \donttest{
#' x_wlr <- gs_design_wlr(
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   weight = wgt05,
#'   info_frac = NULL,
#'   analysis_time = sort(unique(x_ahr$analysis$time)),
#'   ratio = ratio,
#'   alpha = alpha,
#'   beta = beta,
#'   upper = upper,
#'   upar = upar,
#'   lower = lower,
#'   lpar = lpar
#' )
#' x_wlr %>% summary()
#' }
#' # Maxcombo ----
#' \donttest{
#' x_combo <- gs_design_combo(
#'   ratio = 1,
#'   alpha = 0.025,
#'   beta = 0.2,
#'   enroll_rate = define_enroll_rate(duration = 12, rate = 500 / 12),
#'   fail_rate = tibble::tibble(
#'     stratum = "All",
#'     duration = c(4, 100),
#'     fail_rate = log(2) / 15, hr = c(1, .6), dropout_rate = .001
#'   ),
#'   fh_test = fh_test,
#'   upper = gs_spending_combo,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
#'   lower = gs_spending_combo,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2)
#' )
#' x_combo %>% summary()
#' }
#' # Risk difference ----
#' \donttest{
#' gs_design_rd(
#'   p_c = tibble::tibble(stratum = "All", rate = .2),
#'   p_e = tibble::tibble(stratum = "All", rate = .15),
#'   info_frac = c(0.7, 1),
#'   rd0 = 0,
#'   alpha = .025,
#'   beta = .1,
#'   ratio = 1,
#'   stratum_prev = NULL,
#'   weight = "unstratified",
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign::gsDesign(
#'     k = 3, test.type = 1, sfu = gsDesign::sfLDOF, sfupar = NULL
#'   )$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' ) %>% summary()
#' }
summary.gs_design <- function(object,
                              analysis_vars = NULL,
                              analysis_decimals = NULL,
                              col_vars = NULL,
                              col_decimals = NULL,
                              bound_names = c("Efficacy", "Futility"),
                              ...) {
  x <- object
  method <- class(x)[class(x) %in% c("ahr", "wlr", "combo", "rd")]
  x_bound <- x$bound
  x_analysis <- x$analysis
  n_analysis <- max(x_analysis$analysis)

  # Prepare the columns decimals ----
  if (method == "ahr") {
    if (is.null(col_vars) && is.null(col_decimals)) {
      x_decimals <- tibble::tibble(
        col_vars = c("analysis", "bound", "z", "~hr at bound", "nominal p", "Alternate hypothesis", "Null hypothesis"),
        col_decimals = c(NA, NA, 2, 4, 4, 4, 4)
      )
    } else {
      x_decimals <- tibble::tibble(col_vars = col_vars, col_decimals = col_decimals)
    }
  }

  if (method == "wlr") {
    if (is.null(col_vars) && is.null(col_decimals)) {
      x_decimals <- tibble::tibble(
        col_vars = c("analysis", "bound", "z", "~whr at bound", "nominal p", "Alternate hypothesis", "Null hypothesis"),
        col_decimals = c(NA, NA, 2, 4, 4, 4, 4)
      )
    } else {
      x_decimals <- tibble::tibble(col_vars = col_vars, col_decimals = col_decimals)
    }
  }

  if (method == "combo") {
    if (is.null(col_vars) && is.null(col_decimals)) {
      x_decimals <- tibble::tibble(
        col_vars = c("analysis", "bound", "z", "nominal p", "Alternate hypothesis", "Null hypothesis"),
        col_decimals = c(NA, NA, 2, 4, 4, 4)
      )
    } else {
      x_decimals <- tibble::tibble(col_vars = col_vars, col_decimals = col_decimals)
    }
  }

  if (method == "rd") {
    if (is.null(col_vars) && is.null(col_decimals)) {
      x_decimals <- tibble::tibble(
        col_vars = c(
          "analysis", "bound", "z", "~risk difference at bound",
          "nominal p", "Alternate hypothesis", "Null hypothesis"
        ),
        col_decimals = c(NA, NA, 2, 4, 4, 4, 4)
      )
    } else {
      x_decimals <- tibble::tibble(col_vars = col_vars, col_decimals = col_decimals)
    }
  }

  # Prepare the analysis summary row ----
  # get the
  # (1) analysis variables to be displayed on the header
  # (2) decimals to be displayed for the analysis variables in (3)
  if (is.null(analysis_vars) && is.null(analysis_decimals)) {
    if (method %in% c("ahr", "wlr")) {
      analysis_vars <- c("time", "n", "event", "ahr", "info_frac")
      analysis_decimals <- c(1, 1, 1, 2, 2)
    }
    if (method == "combo") {
      analysis_vars <- c("time", "n", "event", "ahr", "event_frac")
      analysis_decimals <- c(1, 1, 1, 2, 2)
    }
    if (method == "rd") {
      analysis_vars <- c("n", "rd", "info_frac")
      analysis_decimals <- c(1, 4, 2)
    }
  } else if (is.null(analysis_vars) && !is.null(analysis_decimals)) {
    stop("summary: please input analysis_vars and analysis_decimals in pairs!")
  } else if (!is.null(analysis_vars) && is.null(analysis_decimals)) {
    stop("summary: please input analysis_vars and analysis_decimals in pairs!")
  }

  # set the analysis summary header
  analyses <- x_analysis %>%
    dplyr::group_by(analysis) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::select(all_of(c("analysis", analysis_vars))) %>%
    dplyr::arrange(analysis)

  if ("analysis" %in% names(analyses)) {
    analyses <- analyses %>% dplyr::rename(Analysis = analysis)
  }

  if ("time" %in% names(analyses)) {
    analyses <- analyses %>% dplyr::rename(Time = time)
    analysis_vars <- replace(analysis_vars, analysis_vars == "time", "Time")
  }

  if ("event" %in% names(analyses)) {
    analyses <- analyses %>% dplyr::rename(Event = event)
    analysis_vars <- replace(analysis_vars, analysis_vars == "event", "Event")
  }

  if ("ahr" %in% names(analyses)) {
    analyses <- analyses %>% dplyr::rename(AHR = ahr)
    analysis_vars <- replace(analysis_vars, analysis_vars == "ahr", "AHR")
  }

  if ("n" %in% names(analyses)) {
    analyses <- analyses %>% dplyr::rename(N = n)
    analysis_vars <- replace(analysis_vars, analysis_vars == "n", "N")
  }

  if ("info_frac" %in% names(analyses)) {
    analyses <- analyses %>% dplyr::rename(`Information fraction` = info_frac)
    analysis_vars <- replace(analysis_vars, analysis_vars == "info_frac", "Information fraction")
  }

  if ("event_frac" %in% names(analyses)) {
    analyses <- analyses %>% dplyr::rename(`Event fraction` = event_frac)
    analysis_vars <- replace(analysis_vars, analysis_vars == "event_frac", "Event fraction")
  }

  # Merge 2 tables:
  # 1. Alternate hypothesis table.
  # 2. Null hypothesis table.
  #
  # Table A: a table under alternative hypothesis.
  xy <- x_bound %>%
    dplyr::rename("Alternate hypothesis" = probability) %>%
    dplyr::rename("Null hypothesis" = probability0) %>%
    # change Upper -> bound_names[1], e.g., Efficacy
    # change Lower -> bound_names[2], e.g., Futility
    dplyr::mutate(bound = dplyr::recode(bound, "upper" = bound_names[1], "lower" = bound_names[2]))

  if ("probability0" %in% colnames(x_bound)) {
    xy <- x_bound %>%
      dplyr::rename("Alternate hypothesis" = probability) %>%
      dplyr::rename("Null hypothesis" = probability0)
  } else {
    xy <- x_bound %>%
      dplyr::rename("Alternate hypothesis" = probability) %>%
      tibble::add_column("Null hypothesis" = "-")
  }
  # change Upper -> bound_names[1], e.g., Efficacy
  # change Lower -> bound_names[2], e.g., Futility
  xy <- xy %>%
    dplyr::mutate(bound = dplyr::recode(bound, "upper" = bound_names[1], "lower" = bound_names[2])) %>%
    dplyr::arrange(analysis, desc(bound))

  # Merge 2 tables:
  # (1) Analysis summary table
  # (2) xy: bound_summary_detail table
  #
  # Merge 3 tables: 1 line per analysis, alternate hypothesis table, null hypothesis table
  #
  # If the method is AHR
  if (method == "ahr") {
    # Header
    analysis_summary_header <- analyses %>% dplyr::select(all_of(c("Analysis", analysis_vars)))
    # Bound details
    bound_summary_detail <- xy
  }

  # If the method is WLR, change AHR to wAHR
  if (method == "wlr") {
    # Header
    analysis_summary_header <- analyses %>% dplyr::select(all_of(c("Analysis", analysis_vars)))
    if ("ahr" %in% analysis_vars) {
      analysis_summary_header <- analysis_summary_header %>% dplyr::rename(wahr = ahr)
    }
    # Bound details
    if ("~hr at bound" %in% names(xy)) {
      bound_summary_detail <- xy %>% dplyr::rename("~whr at bound" = "~hr at bound")
    } else {
      bound_summary_detail <- xy
    }
  }

  # If the method is COMBO, remove the column of "~HR at bound", and remove AHR from header
  if (method == "combo") {
    # Header
    analysis_summary_header <- analyses %>% dplyr::select(all_of(c("Analysis", analysis_vars)))
    # Bound details
    if ("~hr at bound" %in% names(xy)) {
      stop("summary: ~hr at bound can't be display!")
    } else {
      bound_summary_detail <- xy
    }
  }

  # If the method is RD
  if (method == "rd") {
    # Header
    analysis_summary_header <- analyses %>%
      dplyr::select(all_of(c("Analysis", analysis_vars))) %>%
      dplyr::rename("Risk difference" = rd)
    # Bound details
    bound_summary_detail <- xy
  }

  if ("analysis" %in% colnames(bound_summary_detail)) {
    bound_summary_detail <- bound_summary_detail %>% dplyr::rename(Analysis = analysis)
  }
  if ("bound" %in% colnames(bound_summary_detail)) {
    bound_summary_detail <- bound_summary_detail %>% dplyr::rename(Bound = bound)
  }
  if ("z" %in% colnames(bound_summary_detail)) {
    bound_summary_detail <- bound_summary_detail %>% dplyr::rename(Z = z)
  }
  if ("nominal p" %in% colnames(bound_summary_detail)) {
    bound_summary_detail <- bound_summary_detail %>% dplyr::rename("Nominal p" = "nominal p")
  }
  if ("~hr at bound" %in% colnames(bound_summary_detail)) {
    bound_summary_detail <- bound_summary_detail %>% dplyr::rename("~HR at bound" = "~hr at bound")
  }
  if ("~whr at bound" %in% colnames(bound_summary_detail)) {
    bound_summary_detail <- bound_summary_detail %>% dplyr::rename("~wHR at bound" = "~whr at bound")
  }
  if ("~risk difference at bound" %in% colnames(bound_summary_detail)) {
    bound_summary_detail <- bound_summary_detail %>%
      dplyr::rename("~Risk difference at bound" = "~risk difference at bound")
  }

  output <- table_ab(
    # A data frame to be show as the summary header
    # It has only ONE record for each value of `byvar`
    table_a = analysis_summary_header,
    # A data frame to be shown as the listing details
    # It has >= 1 records for each value of `byvar`
    table_b = bound_summary_detail,
    decimals = c(0, analysis_decimals),
    byvar = "Analysis"
  ) %>%
    dplyr::group_by(Analysis)


  if (method == "ahr") {
    output <- output %>% select(
      Analysis, Bound, Z,
      `~HR at bound`, `Nominal p`, `Alternate hypothesis`, `Null hypothesis`
    )
  } else if (method == "wlr") {
    output <- output %>% select(
      Analysis, Bound, Z,
      `~wHR at bound`, `Nominal p`, `Alternate hypothesis`, `Null hypothesis`
    )
  } else if (method == "combo") {
    output <- output %>% select(
      Analysis, Bound, Z,
      `Nominal p`, `Alternate hypothesis`, `Null hypothesis`
    )
  } else if (method == "rd") {
    output <- output %>% select(
      Analysis, Bound, Z,
      `~Risk difference at bound`, `Nominal p`,
      `Alternate hypothesis`, `Null hypothesis`
    )
  }

  # Set the decimals to display ----
  if ("analysis" %in% x_decimals$col_vars) {
    x_decimals <- x_decimals %>% mutate(col_vars = dplyr::if_else(col_vars == "analysis", "Analysis", col_vars))
  }

  if ("bound" %in% x_decimals$col_vars) {
    x_decimals <- x_decimals %>% mutate(col_vars = dplyr::if_else(col_vars == "bound", "Bound", col_vars))
  }

  if ("z" %in% x_decimals$col_vars) {
    x_decimals <- x_decimals %>% mutate(col_vars = dplyr::if_else(col_vars == "z", "Z", col_vars))
  }

  if ("~risk difference at bound" %in% x_decimals$col_vars) {
    x_decimals <- x_decimals %>%
      mutate(col_vars = dplyr::if_else(col_vars == "~risk difference at bound",
        "~Risk difference at bound", col_vars
      ))
  }

  if ("~hr at bound" %in% x_decimals$col_vars) {
    x_decimals <- x_decimals %>%
      mutate(col_vars = dplyr::if_else(col_vars == "~hr at bound",
        "~HR at bound", col_vars
      ))
  }

  if ("~whr at bound" %in% x_decimals$col_vars) {
    x_decimals <- x_decimals %>%
      mutate(col_vars = dplyr::if_else(col_vars == "~whr at bound",
        "~wHR at bound", col_vars
      ))
  }

  if ("nominal p" %in% x_decimals$col_vars) {
    x_decimals <- x_decimals %>% mutate(col_vars = dplyr::if_else(col_vars == "nominal p", "Nominal p", col_vars))
  }


  output <- output %>% select(x_decimals$col_vars)
  if ("Z" %in% colnames(output)) {
    output <- output %>% dplyr::mutate_at(
      "Z",
      round,
      (x_decimals %>% filter(col_vars == "Z"))$col_decimals
    )
  }
  if ("~HR at bound" %in% colnames(output)) {
    output <- output %>% dplyr::mutate_at(
      "~HR at bound",
      round,
      (x_decimals %>% filter(col_vars == "~HR at bound"))$col_decimals
    )
  }
  if ("~Risk difference at bound" %in% colnames(output)) {
    output <- output %>% dplyr::mutate_at(
      "~Risk difference at bound",
      round,
      (x_decimals %>% filter(col_vars == "~Risk difference at bound"))$col_decimals
    )
  }
  if ("Nominal p" %in% colnames(output)) {
    output <- output %>% dplyr::mutate_at(
      "Nominal p",
      round,
      (x_decimals %>% filter(col_vars == "Nominal p"))$col_decimals
    )
  }
  if ("Alternate hypothesis" %in% colnames(output)) {
    output <- output %>% dplyr::mutate_at(
      "Alternate hypothesis",
      round,
      (x_decimals %>% filter(col_vars == "Alternate hypothesis"))$col_decimals
    )
  }
  if ("Null hypothesis" %in% colnames(output) && is.vector(output[["Null hypothesis"]], mode = "numeric")) {
    output <- output %>% dplyr::mutate_at(
      "Null hypothesis",
      round,
      (x_decimals %>% filter(col_vars == "Null hypothesis"))$col_decimals
    )
  }

  class(output) <- c(method, "gs_design", class(output))
  if ("non_binding" %in% class(object)) {
    class(output) <- c("non_binding", class(output))
  }

  return(output)
}

#' @rdname summary
#'
#' @param analysis_vars The variables to be put at the summary header of each analysis.
#' @param analysis_decimals The displayed number of digits of `analysis_vars`.
#' @param col_vars The variables to be displayed.
#' @param col_decimals The decimals to be displayed for the displayed variables in `col_vars`.
#' @param bound_names Names for bounds; default is `c("Efficacy", "Futility")`.
#'
#' @importFrom dplyr all_of
#'
#' @export
#'
#' @examples
#' # Design parameters ----
#' library(gsDesign)
#' library(gsDesign2)
#' library(dplyr)
#'
#' # enrollment/failure rates
#' enroll_rate <- define_enroll_rate(
#'   stratum = "All",
#'   duration = 12,
#'   rate = 1
#' )
#' fail_rate <- define_fail_rate(
#'   duration = c(4, 100),
#'   fail_rate = log(2) / 12,
#'   hr = c(1, .6),
#'   dropout_rate = .001
#' )
#'
#' # Information fraction
#' info_frac <- (1:3) / 3
#'
#' # Analysis times in months; first 2 will be ignored as info_frac will not be achieved
#' analysis_time <- c(.01, .02, 36)
#'
#' # Experimental / Control randomization ratio
#' ratio <- 1
#'
#' # 1-sided Type I error
#' alpha <- 0.025
#'
#' # Type II error (1 - power)
#' beta <- .1
#'
#' # Upper bound
#' upper <- gs_spending_bound
#' upar <- list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
#'
#' # Lower bound
#' lower <- gs_spending_bound
#' lpar <- list(sf = gsDesign::sfHSD, total_spend = 0.1, param = 0, timing = NULL)
#'
#' # weight function in WLR
#' wgt00 <- function(x, arm0, arm1) {
#'   wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 0)
#' }
#' wgt05 <- function(x, arm0, arm1) {
#'   wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = .5)
#' }
#'
#' # test in COMBO
#' fh_test <- rbind(
#'   data.frame(rho = 0, gamma = 0, tau = -1, test = 1, analysis = 1:3, analysis_time = c(12, 24, 36)),
#'   data.frame(rho = c(0, 0.5), gamma = 0.5, tau = -1, test = 2:3, analysis = 3, analysis_time = 36)
#' )
#'
#' # Example 1 ----
#' \donttest{
#' x_ahr <- gs_design_ahr(
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   info_frac = info_frac, # Information fraction
#'   analysis_time = analysis_time,
#'   ratio = ratio,
#'   alpha = alpha,
#'   beta = beta,
#'   upper = upper,
#'   upar = upar,
#'   lower = lower,
#'   lpar = lpar
#' )
#'
#' x_ahr %>% summary()
#'
#' # Customize the digits to display
#' x_ahr %>% summary(analysis_vars = c("time", "event", "info_frac"), analysis_decimals = c(1, 0, 2))
#'
#' # Customize the labels of the crossing probability
#' x_ahr %>% summary(bound_names = c("A is better", "B is better"))
#'
#' # Customize the variables to be summarized for each analysis
#' x_ahr %>% summary(analysis_vars = c("n", "event"), analysis_decimals = c(1, 1))
#' }
#'
#' # Example 2 ----
#' \donttest{
#' x_wlr <- gs_design_wlr(
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   weight = wgt05,
#'   info_frac = NULL,
#'   analysis_time = sort(unique(x_ahr$analysis$time)),
#'   ratio = ratio,
#'   alpha = alpha,
#'   beta = beta,
#'   upper = upper,
#'   upar = upar,
#'   lower = lower,
#'   lpar = lpar
#' )
#' x_wlr %>% summary()
#' }
#' # Maxcombo ----
#' \donttest{
#' x_combo <- gs_design_combo(
#'   ratio = 1,
#'   alpha = 0.025,
#'   beta = 0.2,
#'   enroll_rate = define_enroll_rate(duration = 12, rate = 500 / 12),
#'   fail_rate = tibble::tibble(
#'     stratum = "All",
#'     duration = c(4, 100),
#'     fail_rate = log(2) / 15, hr = c(1, .6), dropout_rate = .001
#'   ),
#'   fh_test = fh_test,
#'   upper = gs_spending_combo,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
#'   lower = gs_spending_combo,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2)
#' )
#' x_combo %>% summary()
#' }
#' # Risk difference ----
#' \donttest{
#' gs_design_rd(
#'   p_c = tibble::tibble(stratum = "All", rate = .2),
#'   p_e = tibble::tibble(stratum = "All", rate = .15),
#'   info_frac = c(0.7, 1),
#'   rd0 = 0,
#'   alpha = .025,
#'   beta = .1,
#'   ratio = 1,
#'   stratum_prev = NULL,
#'   weight = "unstratified",
#'   upper = gs_b,
#'   lower = gs_b,
#'   upar = gsDesign::gsDesign(
#'     k = 3, test.type = 1, sfu = gsDesign::sfLDOF, sfupar = NULL
#'   )$upper$bound,
#'   lpar = c(qnorm(.1), rep(-Inf, 2))
#' ) %>% summary()
#' }
summary.gs_update <- function(object,
                              analysis_vars = NULL,
                              analysis_decimals = NULL,
                              col_vars = NULL,
                              col_decimals = NULL,
                              bound_names = c("Efficacy", "Futility"),
                              ...) {
  x <- object
  method <- class(x)[class(x) %in% c("ahr", "wlr", "combo", "rd")]
  x_bound <- x$bound
  x_analysis <- x$analysis
  n_analysis <- max(x_analysis$analysis)

  # Prepare the columns decimals ----
  if (method == "ahr") {
    if (is.null(col_vars) && is.null(col_decimals)) {
      x_decimals <- tibble::tibble(
        col_vars = c("analysis", "bound", "z", "~hr at bound", "nominal p", "Alternate hypothesis", "Null hypothesis"),
        col_decimals = c(NA, NA, 2, 4, 4, 4, 4)
      )
    } else {
      x_decimals <- tibble::tibble(col_vars = col_vars, col_decimals = col_decimals)
    }
  }

  if (method == "wlr") {
    if (is.null(col_vars) && is.null(col_decimals)) {
      x_decimals <- tibble::tibble(
        col_vars = c("analysis", "bound", "z", "~whr at bound", "nominal p", "Alternate hypothesis", "Null hypothesis"),
        col_decimals = c(NA, NA, 2, 4, 4, 4, 4)
      )
    } else {
      x_decimals <- tibble::tibble(col_vars = col_vars, col_decimals = col_decimals)
    }
  }

  if (method == "combo") {
    if (is.null(col_vars) && is.null(col_decimals)) {
      x_decimals <- tibble::tibble(
        col_vars = c("analysis", "bound", "z", "nominal p", "Alternate hypothesis", "Null hypothesis"),
        col_decimals = c(NA, NA, 2, 4, 4, 4)
      )
    } else {
      x_decimals <- tibble::tibble(col_vars = col_vars, col_decimals = col_decimals)
    }
  }

  if (method == "rd") {
    if (is.null(col_vars) && is.null(col_decimals)) {
      x_decimals <- tibble::tibble(
        col_vars = c(
          "analysis", "bound", "z", "~risk difference at bound",
          "nominal p", "Alternate hypothesis", "Null hypothesis"
        ),
        col_decimals = c(NA, NA, 2, 4, 4, 4, 4)
      )
    } else {
      x_decimals <- tibble::tibble(col_vars = col_vars, col_decimals = col_decimals)
    }
  }

  # Prepare the analysis summary row ----
  # get the
  # (1) analysis variables to be displayed on the header
  # (2) decimals to be displayed for the analysis variables in (3)
  if (is.null(analysis_vars) && is.null(analysis_decimals)) {
    if (method %in% c("ahr", "wlr")) {
      analysis_vars <- c("time", "n", "event", "ahr", "info_frac")
      analysis_decimals <- c(1, 1, 1, 2, 2)
    }
    if (method == "combo") {
      analysis_vars <- c("time", "n", "event", "ahr", "event_frac")
      analysis_decimals <- c(1, 1, 1, 2, 2)
    }
    if (method == "rd") {
      analysis_vars <- c("n", "rd", "info_frac")
      analysis_decimals <- c(1, 4, 2)
    }
  } else if (is.null(analysis_vars) && !is.null(analysis_decimals)) {
    stop("summary: please input analysis_vars and analysis_decimals in pairs!")
  } else if (!is.null(analysis_vars) && is.null(analysis_decimals)) {
    stop("summary: please input analysis_vars and analysis_decimals in pairs!")
  }

  # set the analysis summary header
  analyses <- x_analysis %>%
    dplyr::group_by(analysis) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::select(all_of(c("analysis", analysis_vars))) %>%
    dplyr::arrange(analysis)

  if ("analysis" %in% names(analyses)) {
    analyses <- analyses %>% dplyr::rename(Analysis = analysis)
  }

  if ("time" %in% names(analyses)) {
    analyses <- analyses %>% dplyr::rename(Time = time)
    analysis_vars <- replace(analysis_vars, analysis_vars == "time", "Time")
  }

  if ("event" %in% names(analyses)) {
    analyses <- analyses %>% dplyr::rename(Event = event)
    analysis_vars <- replace(analysis_vars, analysis_vars == "event", "Event")
  }

  if ("ahr" %in% names(analyses)) {
    analyses <- analyses %>% dplyr::rename(AHR = ahr)
    analysis_vars <- replace(analysis_vars, analysis_vars == "ahr", "AHR")
  }

  if ("n" %in% names(analyses)) {
    analyses <- analyses %>% dplyr::rename(N = n)
    analysis_vars <- replace(analysis_vars, analysis_vars == "n", "N")
  }

  if ("info_frac" %in% names(analyses)) {
    analyses <- analyses %>% dplyr::rename(`Information fraction` = info_frac)
    analysis_vars <- replace(analysis_vars, analysis_vars == "info_frac", "Information fraction")
  }

  if ("event_frac" %in% names(analyses)) {
    analyses <- analyses %>% dplyr::rename(`Event fraction` = event_frac)
    analysis_vars <- replace(analysis_vars, analysis_vars == "event_frac", "Event fraction")
  }

  # Merge 2 tables:
  # 1. Alternate hypothesis table.
  # 2. Null hypothesis table.
  #
  # Table A: a table under alternative hypothesis.
  xy <- x_bound %>%
    dplyr::rename("Alternate hypothesis" = probability) %>%
    dplyr::rename("Null hypothesis" = probability0) %>%
    # change Upper -> bound_names[1], e.g., Efficacy
    # change Lower -> bound_names[2], e.g., Futility
    dplyr::mutate(bound = dplyr::recode(bound, "upper" = bound_names[1], "lower" = bound_names[2]))

  if ("probability0" %in% colnames(x_bound)) {
    xy <- x_bound %>%
      dplyr::rename("Alternate hypothesis" = probability) %>%
      dplyr::rename("Null hypothesis" = probability0)
  } else {
    xy <- x_bound %>%
      dplyr::rename("Alternate hypothesis" = probability) %>%
      tibble::add_column("Null hypothesis" = "-")
  }
  # change Upper -> bound_names[1], e.g., Efficacy
  # change Lower -> bound_names[2], e.g., Futility
  xy <- xy %>%
    dplyr::mutate(bound = dplyr::recode(bound, "upper" = bound_names[1], "lower" = bound_names[2])) %>%
    dplyr::arrange(analysis, desc(bound))

  # Merge 2 tables:
  # (1) Analysis summary table
  # (2) xy: bound_summary_detail table
  #
  # Merge 3 tables: 1 line per analysis, alternate hypothesis table, null hypothesis table
  #
  # If the method is AHR
  if (method == "ahr") {
    # Header
    analysis_summary_header <- analyses %>% dplyr::select(all_of(c("Analysis", analysis_vars)))
    # Bound details
    bound_summary_detail <- xy
  }

  # If the method is WLR, change AHR to wAHR
  if (method == "wlr") {
    # Header
    analysis_summary_header <- analyses %>% dplyr::select(all_of(c("Analysis", analysis_vars)))
    if ("ahr" %in% analysis_vars) {
      analysis_summary_header <- analysis_summary_header %>% dplyr::rename(wahr = ahr)
    }
    # Bound details
    if ("~hr at bound" %in% names(xy)) {
      bound_summary_detail <- xy %>% dplyr::rename("~whr at bound" = "~hr at bound")
    } else {
      bound_summary_detail <- xy
    }
  }

  # If the method is COMBO, remove the column of "~HR at bound", and remove AHR from header
  if (method == "combo") {
    # Header
    analysis_summary_header <- analyses %>% dplyr::select(all_of(c("Analysis", analysis_vars)))
    # Bound details
    if ("~hr at bound" %in% names(xy)) {
      stop("summary: ~hr at bound can't be display!")
    } else {
      bound_summary_detail <- xy
    }
  }

  # If the method is RD
  if (method == "rd") {
    # Header
    analysis_summary_header <- analyses %>%
      dplyr::select(all_of(c("Analysis", analysis_vars))) %>%
      dplyr::rename("Risk difference" = rd)
    # Bound details
    bound_summary_detail <- xy
  }

  if ("analysis" %in% colnames(bound_summary_detail)) {
    bound_summary_detail <- bound_summary_detail %>% dplyr::rename(Analysis = analysis)
  }
  if ("bound" %in% colnames(bound_summary_detail)) {
    bound_summary_detail <- bound_summary_detail %>% dplyr::rename(Bound = bound)
  }
  if ("z" %in% colnames(bound_summary_detail)) {
    bound_summary_detail <- bound_summary_detail %>% dplyr::rename(Z = z)
  }
  if ("nominal p" %in% colnames(bound_summary_detail)) {
    bound_summary_detail <- bound_summary_detail %>% dplyr::rename("Nominal p" = "nominal p")
  }
  if ("~hr at bound" %in% colnames(bound_summary_detail)) {
    bound_summary_detail <- bound_summary_detail %>% dplyr::rename("~HR at bound" = "~hr at bound")
  }
  if ("~whr at bound" %in% colnames(bound_summary_detail)) {
    bound_summary_detail <- bound_summary_detail %>% dplyr::rename("~wHR at bound" = "~whr at bound")
  }
  if ("~risk difference at bound" %in% colnames(bound_summary_detail)) {
    bound_summary_detail <- bound_summary_detail %>%
      dplyr::rename("~Risk difference at bound" = "~risk difference at bound")
  }

  output <- table_ab(
    # A data frame to be show as the summary header
    # It has only ONE record for each value of `byvar`
    table_a = analysis_summary_header,
    # A data frame to be shown as the listing details
    # It has >= 1 records for each value of `byvar`
    table_b = bound_summary_detail,
    decimals = c(0, analysis_decimals),
    byvar = "Analysis"
  ) %>%
    dplyr::group_by(Analysis)


  if (method == "ahr") {
    output <- output %>% select(
      Analysis, Bound, Z,
      `~HR at bound`, `Nominal p`, `Alternate hypothesis`, `Null hypothesis`
    )
  } else if (method == "wlr") {
    output <- output %>% select(
      Analysis, Bound, Z,
      `~wHR at bound`, `Nominal p`, `Alternate hypothesis`, `Null hypothesis`
    )
  } else if (method == "combo") {
    output <- output %>% select(
      Analysis, Bound, Z,
      `Nominal p`, `Alternate hypothesis`, `Null hypothesis`
    )
  } else if (method == "rd") {
    output <- output %>% select(
      Analysis, Bound, Z,
      `~Risk difference at bound`, `Nominal p`,
      `Alternate hypothesis`, `Null hypothesis`
    )
  }

  # Set the decimals to display ----
  if ("analysis" %in% x_decimals$col_vars) {
    x_decimals <- x_decimals %>% mutate(col_vars = dplyr::if_else(col_vars == "analysis", "Analysis", col_vars))
  }

  if ("bound" %in% x_decimals$col_vars) {
    x_decimals <- x_decimals %>% mutate(col_vars = dplyr::if_else(col_vars == "bound", "Bound", col_vars))
  }

  if ("z" %in% x_decimals$col_vars) {
    x_decimals <- x_decimals %>% mutate(col_vars = dplyr::if_else(col_vars == "z", "Z", col_vars))
  }

  if ("~risk difference at bound" %in% x_decimals$col_vars) {
    x_decimals <- x_decimals %>%
      mutate(col_vars = dplyr::if_else(col_vars == "~risk difference at bound", "~Risk difference at bound", col_vars
      ))
  }

  if ("~hr at bound" %in% x_decimals$col_vars) {
    x_decimals <- x_decimals %>%
      mutate(col_vars = dplyr::if_else(col_vars == "~hr at bound", "~HR at bound", col_vars
      ))
  }

  if ("~whr at bound" %in% x_decimals$col_vars) {
    x_decimals <- x_decimals %>%
      mutate(col_vars = dplyr::if_else(col_vars == "~whr at bound", "~wHR at bound", col_vars
      ))
  }

  if ("nominal p" %in% x_decimals$col_vars) {
    x_decimals <- x_decimals %>% mutate(col_vars = dplyr::if_else(col_vars == "nominal p", "Nominal p", col_vars))
  }


  output <- output %>% select(x_decimals$col_vars)
  if ("Z" %in% colnames(output)) {
    output <- output %>% dplyr::mutate_at(
      "Z",
      round,
      (x_decimals %>% filter(col_vars == "Z"))$col_decimals
    )
  }
  if ("~HR at bound" %in% colnames(output)) {
    output <- output %>% dplyr::mutate_at(
      "~HR at bound",
      round,
      (x_decimals %>% filter(col_vars == "~HR at bound"))$col_decimals
    )
  }
  if ("~Risk difference at bound" %in% colnames(output)) {
    output <- output %>% dplyr::mutate_at(
      "~Risk difference at bound",
      round,
      (x_decimals %>% filter(col_vars == "~Risk difference at bound"))$col_decimals
    )
  }
  if ("Nominal p" %in% colnames(output)) {
    output <- output %>% dplyr::mutate_at(
      "Nominal p",
      round,
      (x_decimals %>% filter(col_vars == "Nominal p"))$col_decimals
    )
  }
  if ("Alternate hypothesis" %in% colnames(output)) {
    output <- output %>% dplyr::mutate_at(
      "Alternate hypothesis",
      round,
      (x_decimals %>% filter(col_vars == "Alternate hypothesis"))$col_decimals
    )
  }
  if ("Null hypothesis" %in% colnames(output) && is.vector(output[["Null hypothesis"]], mode = "numeric")) {
    output <- output %>% dplyr::mutate_at(
      "Null hypothesis",
      round,
      (x_decimals %>% filter(col_vars == "Null hypothesis"))$col_decimals
    )
  }

  class(output) <- c(method, "gs_design", class(output))
  if ("non_binding" %in% class(object)) {
    class(output) <- c("non_binding", class(output))
  }

  return(output)
}
