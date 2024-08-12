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

#' Convert summary table of a fixed or group sequential design object to a gt object
#'
#' @param x A summary object of a fixed or group sequential design.
#' @param ... Additional arguments (not used).
#'
#' @return A `gt_tbl` object.
#'
#' @export
as_gt <- function(x, ...) {
  UseMethod("as_gt", x)
}

#' @rdname as_gt
#'
#' @export
#'
#' @examplesIf interactive() && !identical(Sys.getenv("IN_PKGDOWN"), "true")
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
#'   dropout_rate = .001,
#'   hr = c(1, .6)
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
#'
#' # Type II error (1 - power)
#' beta <- 0.1
#'
#' # Example 1 ----
#' fixed_design_ahr(
#'   alpha = alpha, power = 1 - beta,
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   study_duration = study_duration, ratio = ratio
#' ) %>%
#'   summary() %>%
#'   as_gt()
#'
#' # Example 2 ----
#' fixed_design_fh(
#'   alpha = alpha, power = 1 - beta,
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   study_duration = study_duration, ratio = ratio
#' ) %>%
#'   summary() %>%
#'   as_gt()
as_gt.fixed_design <- function(x, title = NULL, footnote = NULL, ...) {
  method <- design_method(x)
  ans <- gt::gt(x) %>%
    gt::tab_header(title = title %||% method_title(method)) %>%
    gt::tab_footnote(
      footnote = footnote %||% method_footnote(x, method),
      locations = gt::cells_title(group = "title")
    )
  return(ans)
}

# get the design method
design_method <- function(x) {
  methods <- c("ahr", "fh", "mb", "lf", "rd", "maxcombo", "milestone", "rmst")
  intersect(methods, class(x))[1]
}

# get the default title
method_title <- function(method) {
  sprintf("Fixed Design %s Method", switch(
    method,
    ahr = "under AHR", fh = "under Fleming-Harrington", mb = "under Magirr-Burman",
    lf = "under Lachin and Foulkes", maxcombo = "under MaxCombo",
    milestone = "under Milestone", rmst = "under Restricted Mean Survival Time",
    rd = "of Risk Difference under Farrington-Manning"
  ))
}

# get the default footnote
method_footnote <- function(x, method) {
  switch(
    method,
    ahr = "Power computed with average hazard ratio method.",
    fh = paste(
      "Power for Fleming-Harrington test", substring(x$Design, 19),
      "using method of Yung and Liu."
    ),
    lf = paste(
      "Power using Lachin and Foulkes method applied using expected",
      "average hazard ratio (AHR) at time of planned analysis."
    ),
    rd = paste(
      "Risk difference power without continuity correction using method of",
      "Farrington and Manning."
    ),
    maxcombo = paste0(
      "Power for MaxCombo test with Fleming-Harrington tests ",
      substring(x$Design, 9), "."
    ),
    # for mb, milestone, and rmst
    paste("Power for", x$Design, "computed with method of Yung and Liu.")
  )
}

#' @rdname as_gt
#'
#' @param title A string to specify the title of the gt table.
#' @param subtitle A string to specify the subtitle of the gt table.
#' @param colname_spanner A string to specify the spanner of the gt table.
#' @param colname_spannersub A vector of strings to specify the spanner details of the gt table.
#' @param footnote A list containing `content`, `location`, and `attr`.
#'   `content` is a vector of string to specify the footnote text;
#'   `location` is a vector of string to specify the locations to put the
#'   superscript of the footnote index;
#'   `attr` is a vector of string to specify the attributes of the footnotes,
#'   for example, `c("colname", "title", "subtitle", "analysis", "spanner")`;
#'   users can use the functions in the `gt` package to customize the table.
#' @param display_bound A vector of strings specifying the label of the bounds.
#'   The default is `c("Efficacy", "Futility")`.
#' @param display_columns A vector of strings specifying the variables to be
#'   displayed in the summary table.
#' @param display_inf_bound Logical, whether to display the +/-inf bound.
#'
#' @export
#'
#' @examplesIf interactive() && !identical(Sys.getenv("IN_PKGDOWN"), "true")
#' library(dplyr)
#' # Example 1 ----
#' # The default output
#'
#' gs_design_ahr() %>%
#'   summary() %>%
#'   as_gt()
#'
#' gs_power_ahr() %>%
#'   summary() %>%
#'   as_gt()
#'
#' gs_design_wlr() %>%
#'   summary() %>%
#'   as_gt()
#'
#' gs_power_wlr() %>%
#'   summary() %>%
#'   as_gt()
#'
#' gs_power_combo() %>%
#'   summary() %>%
#'   as_gt()
#'
#' gs_design_rd() %>%
#'   summary() %>%
#'   as_gt()
#'
#' gs_power_rd() %>%
#'   summary() %>%
#'   as_gt()
#'
#' # Example 2 ----
#' # Usage of title = ..., subtitle = ...
#' # to edit the title/subtitle
#' gs_power_wlr() %>%
#'   summary() %>%
#'   as_gt(
#'     title = "Bound Summary",
#'     subtitle = "from gs_power_wlr"
#'   )
#'
#' # Example 3 ----
#' # Usage of colname_spanner = ..., colname_spannersub = ...
#' # to edit the spanner and its sub-spanner
#' gs_power_wlr() %>%
#'   summary() %>%
#'   as_gt(
#'     colname_spanner = "Cumulative probability to cross boundaries",
#'     colname_spannersub = c("under H1", "under H0")
#'   )
#'
#' # Example 4 ----
#' # Usage of footnote = ...
#' # to edit the footnote
#' gs_power_wlr() %>%
#'   summary() %>%
#'   as_gt(
#'     footnote = list(
#'       content = c(
#'         "approximate weighted hazard ratio to cross bound.",
#'         "wAHR is the weighted AHR.",
#'         "the crossing probability.",
#'         "this table is generated by gs_power_wlr."
#'       ),
#'       location = c("~wHR at bound", NA, NA, NA),
#'       attr = c("colname", "analysis", "spanner", "title")
#'     )
#'   )
#'
#' # Example 5 ----
#' # Usage of display_bound = ...
#' # to either show efficacy bound or futility bound, or both(default)
#' gs_power_wlr() %>%
#'   summary() %>%
#'   as_gt(display_bound = "Efficacy")
#'
#' # Example 6 ----
#' # Usage of display_columns = ...
#' # to select the columns to display in the summary table
#' gs_power_wlr() %>%
#'   summary() %>%
#'   as_gt(display_columns = c("Analysis", "Bound", "Nominal p", "Z", "Probability"))
#'
as_gt.gs_design <- function(
    x,
    title = NULL,
    subtitle = NULL,
    colname_spanner = "Cumulative boundary crossing probability",
    colname_spannersub = c("Alternate hypothesis", "Null hypothesis"),
    footnote = NULL,
    display_bound = c("Efficacy", "Futility"),
    display_columns = NULL,
    display_inf_bound = FALSE,
    ...) {

  method <- intersect(class(x), c("ahr", "wlr", "combo", "rd"))[1]
  full_alpha <- attr(x, "full_alpha")

  x_alpha <- max(filter(x, Bound == display_bound[1])[[colname_spannersub[2]]])

  x_non_binding <- inherits(x, "non_binding")

  x_k <- as.numeric(substr(x$Analysis, 11, 11))

  if (!display_inf_bound) x <- filter(x, !is.infinite(Z))

  x_old <- x

  # Set defaults ----
  # set different default title to different methods
  if (is.null(title)) title <- paste("Bound summary", switch(
    method,
    ahr = "for AHR design", wlr = "for WLR design",
    combo = "for MaxCombo design", rd = "of Binary Endpoint"
  ))

  # set different default subtitle to different methods
  if (is.null(subtitle)) subtitle <- switch(
    method,
    ahr = "AHR approximations of ~HR at bound",
    wlr = "WLR approximation of ~wHR at bound",
    combo = "MaxCombo approximation",
    rd = "measured by risk difference"
  )

  # set different default columns to display
  if (is.null(display_columns)) display_columns <- c(
    "Analysis", "Bound", "Z", "Nominal p",
    sprintf("%s at bound", switch(method, ahr = "~HR", wlr = "~wHR", rd = "~Risk difference")),
    "Alternate hypothesis", "Null hypothesis"
  )
  # filter the columns to display as the output
  ## if `Probability` is selected to output, then transform it to `c("Alternate hypothesis", "Null hypothesis")`
  if (any(i <- display_columns == "Probability"))
    display_columns <- c(display_columns[!i], "Alternate hypothesis", "Null hypothesis")
  ## check if the `display_columns` are included in `x` output
  if (!all(display_columns %in% names(x))) stop(
    "not all variable names in 'display_columns' are in the summary_bound object!"
  )
  x <- x[, display_columns]

  # set different default footnotes to different methods
  if (is.null(footnote)) footnote <- switch(
    method,
    ahr = list(
      content = c(
        if (i1 <- "~HR at bound" %in% display_columns)
          "Approximate hazard ratio to cross bound.",
        if (i2 <- "Nominal p" %in% display_columns)
          "One-sided p-value for experimental vs control treatment.
          Value < 0.5 favors experimental, > 0.5 favors control."
      ),
      location = c(if (i1) "~HR at bound", if (i2) "Nominal p"),
      attr = c(if (i1) "colname", if (i2) "colname")
    ),
    wlr = list(
      content = c(
        if (i1 <- "~wHR at bound" %in% display_columns)
          "Approximate hazard ratio to cross bound.",
        if (i2 <- "Nominal p" %in% display_columns)
          "One-sided p-value for experimental vs control treatment.
          Value < 0.5 favors experimental, > 0.5 favors control.",
        "wAHR is the weighted AHR."
      ),
      location = c(if (i1) "~wHR at bound", if (i2) "Nominal p"),
      attr = c(if (i1) "colname", if (i2) "colname", "analysis")
    ),
    combo = list(
      content = c(
        if (i2 <- "Nominal p" %in% display_columns)
          "One-sided p-value for experimental vs control treatment.
          Value < 0.5 favors experimental, > 0.5 favors control.",
        "EF is event fraction. AHR  is under regular weighted log rank test."),
      location = if (i2) "Nominal p",
      attr = c(if (i2) "colname", "analysis")
    ),
    rd = list(
      content = if (i2 <- "Nominal p" %in% display_columns)
        "One-sided p-value for experimental vs control treatment.
        Value < 0.5 favors experimental, > 0.5 favors control.",
      location = if (i2) "Nominal p",
      attr = if (i2) "colname"
    )
  )

  # Filter out inf bound ----
  x <- subset(x, !is.na(`Alternate hypothesis`) & !is.na(`Null hypothesis`))

  # Add spanner ----
  i <- match(c("Alternate hypothesis", "Null hypothesis"), names(x))
  names(x)[i] <- colname_spannersub

  x <- x %>%
    subset(Bound %in% display_bound) %>%
    dplyr::arrange(Analysis) %>%
    dplyr::group_by(Analysis) %>%
    gt::gt() %>%
    gt::tab_spanner(
      columns = dplyr::all_of(colname_spannersub),
      label = colname_spanner
    ) %>%
    gt::tab_header(title = title, subtitle = subtitle)

  # Add footnotes ----
  for (i in seq_along(footnote$content)) {
    att <- footnote$attr[i]
    loc <- if (att == "colname") {
      # footnotes are added on the colnames
      gt::cells_column_labels(columns = footnote$location[i])
    } else if (att %in% c("title", "subtitle")) {
      # on the title/subtitle
      gt::cells_title(group = att)
    } else if (att == "analysis") {
      # on the analysis summary row, which is a grouping variable, i.e., Analysis
      gt::cells_row_groups(groups = dplyr::starts_with("Analysis"))
    } else if (att == "spanner") {
      # on the column spanner
      gt::cells_column_spanners(spanners = colname_spanner)
    }
    if (!is.null(loc))
      x <- gt::tab_footnote(x, footnote = footnote$content[i], locations = loc)
  }

  ## if it is non-binding design
  if (x_non_binding && x_alpha < full_alpha) x <- gt::tab_footnote(
    x,
    footnote = footnote_non_binding(x_alpha, full_alpha),
    locations = gt::cells_body(
      columns = colname_spannersub[2],
      rows = substr(x_old$Analysis, 1, 11) == paste0("Analysis: ", max(x_k)) &
        x_old$Bound == display_bound[1]
    )
  )

  return(x)
}

footnote_non_binding <- function(x_alpha, full_alpha) {
  a1 <- format(x_alpha, scientific = FALSE)
  a2 <- format(full_alpha, scientific = FALSE)
  a3 <- format(full_alpha - x_alpha, scientific = FALSE)
  paste0(
    "Cumulative alpha for final analysis ",
    "(", a1, ") ", "is less than the full alpha ", "(", a2, ") ",
    "when the futility bound is non-binding. ",
    "The smaller value subtracts the probability of crossing a futility bound ",
    "before crossing an efficacy bound at a later analysis ",
    "(", a2, " - ", a3, " = ", a1, ") ", "under the null hypothesis."
  )
}
