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
#' ) |> summary()
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
#' ) |> summary()
#'
summary.fixed_design <- function(object, ...) {
  ans <- object$analysis
  ans$design <- attr(object, "design_display")

  # capitalize names
  ans <- cap_names(ans)
  # Propagate attributes for as_gt()/as_rtf() tables
  attr(ans, "title") <- attr(object, "title")
  attr(ans, "footnote") <- attr(object, "footnote")

  ans <- add_class(ans, "fixed_design_summary")
  return(ans)
}


#' @rdname summary
#'
#' @param analysis_vars The variables to be put at the summary header of each analysis.
#' @param analysis_decimals The displayed number of digits of `analysis_vars`.
#'   If the vector is unnamed, it must match the length of `analysis_vars`. If
#'   the vector is named, you only have to specify the number of digits for the
#'   variables you want to be displayed differently than the defaults.
#' @param col_vars The variables to be displayed.
#' @param col_decimals The decimals to be displayed for the displayed variables in `col_vars`.
#'   If the vector is unnamed, it must match the length of `col_vars`. If the
#'   vector is named, you only have to specify the number of digits for the
#'   columns you want to be displayed differently than the defaults.
#' @param bound_names Names for bounds; default is `c("Efficacy", "Futility")`.
#'
#' @export
#'
#' @examples
#' # Design parameters ----
#' library(gsDesign)
#' library(gsDesign2)
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
#' x_ahr |> summary()
#'
#' # Customize the digits to display
#' x_ahr |> summary(analysis_vars = c("time", "event", "info_frac"), analysis_decimals = c(1, 0, 2))
#'
#' # Customize the labels of the crossing probability
#' x_ahr |> summary(bound_names = c("A is better", "B is better"))
#'
#' # Customize the variables to be summarized for each analysis
#' x_ahr |> summary(analysis_vars = c("n", "event"), analysis_decimals = c(1, 1))
#'
#' # Customize the digits for the columns
#' x_ahr |> summary(col_decimals = c(z = 4))
#'
#' # Customize the columns to display
#' x_ahr |> summary(col_vars = c("z", "~hr at bound", "nominal p"))
#'
#' # Customize columns and digits
#' x_ahr |> summary(col_vars = c("z", "~hr at bound", "nominal p"),
#'                   col_decimals = c(4, 2, 2))
#' }
#'
#' # Example 2 ----
#' \donttest{
#' x_wlr <- gs_design_wlr(
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   weight = list(method = "fh", param = list(rho = 0, gamma = 0.5)),
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
#' x_wlr |> summary()
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
#' x_combo |> summary()
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
#' ) |> summary()
#' }
summary.gs_design <- function(object,
                              analysis_vars = NULL,
                              analysis_decimals = NULL,
                              col_vars = NULL,
                              col_decimals = NULL,
                              bound_names = c("Efficacy", "Futility"),
                              ...) {
  x <- object
  x_bound <- x$bound
  x_analysis <- x$analysis
  method <- x$design

  # Prepare the analysis summary row ----
  # get the
  # (1) analysis variables to be displayed on the header
  # (2) decimals to be displayed for the analysis variables in (3)
  default_vars <- if (method == "rd") c("n", "rd", "info_frac") else c(
    "time", "n", "event", "ahr",
    switch(method, ahr = "info_frac0", wlr = "info_frac0", combo = "event_frac")
  )
  default_decimals <- if (method == "rd") c(1, 4, 2) else c(1, 1, 1, 2, 2)

  # Filter analysis variables and update decimal places
  analysis_decimals <- get_decimals(
    analysis_vars, analysis_decimals, default_vars, default_decimals
  )
  analysis_vars <- names(analysis_decimals)

  # set the analysis summary header
  analyses <- x_analysis |>
    group_by(analysis) |>
    filter(row_number() == 1) |>
    select(all_of(c("analysis", analysis_vars)))

  # Merge 2 tables:
  # 1. Alternate hypothesis table.
  # 2. Null hypothesis table.
  #
  # Table A: a table under alternative hypothesis.
  xy <- x_bound
  # change Upper -> bound_names[1], e.g., Efficacy
  # change Lower -> bound_names[2], e.g., Futility
  xy$bound <- replace_values(xy$bound, c(upper = bound_names[1], lower = bound_names[2]))

  if (!"probability0" %in% names(xy)) xy$probability0 <- "-"
  xy <- xy |> arrange(analysis, desc(bound))

  # Merge 2 tables:
  # (1) Analysis summary table
  # (2) xy: bound details table
  #
  # Merge 3 tables: 1 line per analysis, alternate hypothesis table, null hypothesis table

  # If the method is WLR, change HR to wHR
  if (method == "wlr") xy <- replace_names(xy, c("~hr at bound" = "~whr at bound"))

  output <- table_ab(
    # A data frame to be show as the summary header
    # It has only ONE record for each value of `byvar`
    table_a = cap_names(analyses),
    # A data frame to be shown as the listing details
    # It has >= 1 records for each value of `byvar`
    table_b = cap_names(xy),
    decimals = c(0, analysis_decimals),
    byvar = "Analysis"
  )

  # Prepare the columns decimals ----
  default_decimals <- c(NA, NA, 2, if (method != "combo") 4, 4, 4, 4)
  default_vars <- c(
    "analysis", "bound", "z",
    sprintf("~%s at bound", switch(method, ahr = "hr", wlr = "whr", rd = "risk difference")),
    "nominal p", "Alternate hypothesis", "Null hypothesis"
  )

  # Filter columns and update decimal places
  col_decimals <- get_decimals(col_vars, col_decimals, default_vars, default_decimals)

  # "bound" is a required column
  if (!"bound" %in% names(col_decimals)) col_decimals <- c(bound = NA, col_decimals)

  col_decimals <- cap_names(col_decimals)
  col_vars <- names(col_decimals)

  output <- output |> group_by(Analysis) |> select(all_of(col_vars))
  # Set the decimals to display ----
  for (j in col_vars) output[[j]] <- round2(output[[j]], col_decimals[j])

  output <- add_class(output, "gs_design_summary")
  attr(output, "binding") <- attr(x, "binding")
  attr(output, "design") <- x$design

  # Save the full alpha as an attribute of the output summary table
  # Use input$alpha when given power to calculate sample size
  attr(output, "full_alpha") <- object$input$alpha %||% {
    # when given sample size to calculate power
    if (is.list(object$input$upar)) object$input$upar$total_spend else 0.025
  }

  return(output)
}

# get a named vector of decimals (names are variable names)
get_decimals <- function(vars, decs, vars_default, decs_default) {
  names(decs_default) <- vars_default
  # merge user-provided named decimals into default
  decs_vars <- names(decs)
  decs_default[decs_vars] <- decs

  # get the variable names passed to the 'vars' and 'decs' arguments
  vars_name <- as.character(substitute(vars))  # e.g., 'analysis_vars'
  decs_name <- as.character(substitute(decs))  # e.g., 'analysis_decimals'
  if (is.null(vars)) {
    if (!is.null(decs) && is.null(decs_vars))
      stop("'", decs_name, "' must be a named vector if '", vars_name, "' is not provided")
    vars <- vars_default
  }
  decs <- (if (is.null(decs_vars)) decs) %||% decs_default[vars]
  if (length(vars) != length(decs))
    stop("'", vars_name, "' and '", decs_name, "' must be of the same length")
  names(decs) <- vars
  decs
}

# capitalize variable names
cap_names <- function(x) {
  low <- c(
    "analysis", "design", "power", "time", "n", "bound", "z",
    "~risk difference at bound", "~hr at bound", "~whr at bound", "nominal p"
  )
  # map lowercase names to capitalized names
  map <- setNames(cap_initial(low), low)
  map <- gsub("^~risk ", "~Risk ", map)
  map <- gsub("^(~w?)(hr) ", "\\1HR ", map, perl = TRUE)
  map <- c(
    map, ahr = "AHR", event = "Events", rd = "Risk difference",
    probability = "Alternate hypothesis", probability0 = "Null hypothesis",
    info_frac0 = "Information fraction", info_frac = "Information fraction",
    event_frac = "Event fraction"
  )
  replace_names(x, map)
}
