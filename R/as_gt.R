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
#' @examples
#' # Fixed design examples ----
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
#' ) |>
#'   summary() |>
#'   as_gt()
#'
#' # Example 2 ----
#' fixed_design_fh(
#'   alpha = alpha, power = 1 - beta,
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   study_duration = study_duration, ratio = ratio
#' ) |>
#'   summary() |>
#'   as_gt()
as_gt.fixed_design_summary <- function(x, title = NULL, footnote = NULL, ...) {
  if (is.null(title)) title <- attr(x, "title")
  if (is.null(footnote)) footnote <- attr(x, "footnote")

  ans <- gt::gt(x) |>
    gt::tab_header(title = title) |>
    gt::tab_footnote(
      footnote = footnote,
      locations = gt::cells_title(group = "title")
    )
  return(ans)
}

#' @rdname as_gt
#'
#' @param title A string to specify the title of the gt table.
#' @param subtitle A string to specify the subtitle of the gt table.
#' @param colname_spanner A string to specify the spanner of the gt table.
#' @param colname_spannersub A vector of strings to specify the spanner details
#'   of the gt table.
#' @param footnote A list containing `content`, `location`, and `attr`.
#'   `content` is a vector of string to specify the footnote text; `location` is
#'   a vector of string to specify the locations to put the superscript of the
#'   footnote index; `attr` is a vector of string to specify the attributes of
#'   the footnotes, for example, `c("colname", "title", "subtitle", "analysis",
#'   "spanner")`; users can use the functions in the `gt` package to customize
#'   the table. To disable footnotes, use `footnote = FALSE`.
#' @param display_bound A vector of strings specifying the label of the bounds.
#'   The default is `c("Efficacy", "Futility")`.
#' @param display_columns A vector of strings specifying the variables to be
#'   displayed in the summary table.
#' @param display_inf_bound Logical, whether to display the +/-inf bound.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Group sequential design examples ---
#'
#' # Example 1 ----
#' # The default output
#'
#' gs_design_ahr() |>
#'   summary() |>
#'   as_gt()
#'
#' gs_power_ahr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_gt()
#'
#' gs_design_wlr() |>
#'   summary() |>
#'   as_gt()
#'
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_gt()
#'
#' gs_power_combo() |>
#'   summary() |>
#'   as_gt()
#'
#' gs_design_rd() |>
#'   summary() |>
#'   as_gt()
#'
#' gs_power_rd() |>
#'   summary() |>
#'   as_gt()
#'
#' # Example 2 ----
#' # Usage of title = ..., subtitle = ...
#' # to edit the title/subtitle
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_gt(
#'     title = "Bound Summary",
#'     subtitle = "from gs_power_wlr"
#'   )
#'
#' # Example 3 ----
#' # Usage of colname_spanner = ..., colname_spannersub = ...
#' # to edit the spanner and its sub-spanner
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_gt(
#'     colname_spanner = "Cumulative probability to cross boundaries",
#'     colname_spannersub = c("under H1", "under H0")
#'   )
#'
#' # Example 4 ----
#' # Usage of footnote = ...
#' # to edit the footnote
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
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
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_gt(display_bound = "Efficacy")
#'
#' # Example 6 ----
#' # Usage of display_columns = ...
#' # to select the columns to display in the summary table
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_gt(display_columns = c("Analysis", "Bound", "Nominal p", "Z", "Probability"))
#' }
as_gt.gs_design_summary <- function(
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

  x_old <- x
  parts <- gsd_parts(
    x, title, subtitle, colname_spannersub, footnote,
    display_bound, display_columns, display_inf_bound
  )

  x <- parts$x |>
    group_by(Analysis) |>
    gt::gt() |>
    gt::tab_spanner(
      columns = all_of(colname_spannersub),
      label = colname_spanner
    ) |>
    gt::tab_header(title = parts$title, subtitle = parts$subtitle)

  # Add footnotes ----
  add_footnote <- !isFALSE(footnote)
  footnote <- parts$footnote
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

  # add footnote for non-binding design
  footnote_nb <- if (add_footnote) gsd_footnote_nb(x_old, parts$alpha)
  if (!is.null(footnote_nb)) x <- gt::tab_footnote(
    x,
    footnote = footnote_nb,
    locations = gt::cells_body(
      columns = colname_spannersub[2],
      rows = gsd_footnote_row(parts$x, display_bound[1])
    )
  )

  return(x)
}

# get different default columns to display
gsd_columns <- function(columns, method, x) {
  # set different default columns to display
  if (is.null(columns)) columns <- c(
    "Analysis", "Bound", "Z", "Nominal p",
    sprintf("%s at bound", switch(method, ahr = "~HR", wlr = "~wHR", rd = "~Risk difference")),
    "Alternate hypothesis", "Null hypothesis"
  )
  # filter the columns to display as the output: if `Probability` is selected to
  # output, transform it to `c("Alternate hypothesis", "Null hypothesis")`
  if (any(i <- columns == "Probability"))
    columns <- c(columns[!i], "Alternate hypothesis", "Null hypothesis")
  ## check if the `display_columns` are included in `x` output
  if (!all(columns %in% names(x))) stop(
    "not all variable names in 'display_columns' are in the summary_bound object!"
  )
  columns
}

# default footnotes for 'gs_design' tables
gsd_footnote <- function(method, columns) {
  n <- c("Nominal p", "~HR at bound", "~wHR at bound")
  i <- n %in% columns
  res <- if (i[1]) list(
    content = paste(
      "One-sided p-value for experimental vs control treatment.",
      "Value < 0.5 favors experimental, > 0.5 favors control."
    ),
    location = n[1], attr = "colname"
  ) else {
    list(content = NULL, location = NULL, attr = NULL)
  }
  x <- "Approximate hazard ratio to cross bound."
  switch(
    method,
    ahr = res %+% if (i[2]) list(x, n[2], "colname"),
    wlr = res %+% (if (i[3]) list(x, n[3], "colname")) %+%
      list("wAHR is the weighted AHR.", NULL, "analysis"),
    combo = res %+% list(
      "EF is event fraction. AHR is under regular weighted log rank test.",
      NULL, "analysis"
    ),
    rd = res
  )
}

# footnote for non-binding designs
gsd_footnote_nb <- function(x, x_alpha) {
  full_alpha <- attr(x, "full_alpha")
  if (attr(x, "binding") || x_alpha >= full_alpha) return()
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

# where to add the non-binding design footnote
gsd_footnote_row <- function(x, bound) {
  # for a vector of "Analysis: N", get a logical vector `i`, in which `TRUE`
  # indicates the position of the largest `N`
  a <- x$Analysis
  r <- "^Analysis: ([0-9]+).*"
  i <- grepl(r, a)
  k <- as.numeric(sub(r, '\\1', a[i]))
  i[i] <- k == max(k)
  i & x$Bound == bound
}

# a list of information for `as_[gt|rtf].gs_design()` methods: the transformed
# data, title, and footnote, etc.
gsd_parts <- function(
  x, title, subtitle, spannersub, footnote, bound, columns, inf_bound,
  transform = identity
) {
  method <- attr(x, "design")
  if (!inf_bound) x <- filter(x, !is.infinite(Z))
  # `x` needs a custom transformation in as_rtf()
  x2 <- transform(x)

  columns <- gsd_columns(columns, method, x2)
  x2 <- x2[, columns]
  x2 <- subset(x2, !is.na(`Alternate hypothesis`) & !is.na(`Null hypothesis`))
  x2 <- subset(x2, Bound %in% bound)

  i <- match(c("Alternate hypothesis", "Null hypothesis"), names(x2))
  names(x2)[i] <- spannersub

  title <- title %||% paste("Bound summary", switch(
    method,
    ahr = "for AHR design", wlr = "for WLR design",
    combo = "for MaxCombo design", rd = "of Binary Endpoint"
  ))
  subtitle <- subtitle %||% switch(
    method,
    ahr = "AHR approximations of ~HR at bound",
    wlr = "WLR approximation of ~wHR at bound",
    combo = "MaxCombo approximation",
    rd = "measured by risk difference"
  )

  list(
    x = arrange(x2, Analysis),
    title = title, subtitle = subtitle,
    footnote = if (!isFALSE(footnote)) footnote %||% gsd_footnote(method, columns),
    alpha = max(filter(x, Bound == bound[1])[["Null hypothesis"]])
  )
}

# Only purpose of the method below is to fix S3 redirection when gsDesign2 is
# loaded after simtrial, which masks the as_gt() generic from simtrial

#' @export
as_gt.simtrial_gs_wlr <- function(x, ...) {
  f <- getFromNamespace("as_gt.simtrial_gs_wlr", "simtrial")
  f(x, ...)
}
