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

#' Convert summary table of a fixed or group sequential design object to an lt table
#'
#' @param x A summary object of a fixed or group sequential design.
#' @param ... Additional arguments (not used).
#'
#' @return An `lt_tbl` object.
#'
#' @export
as_lt <- function(x, ...) {
  UseMethod("as_lt", x)
}

# Apply the bundled gt-style theme so as_lt() output resembles the gt tables
# that as_gt() used to produce. lt::lt_css() dedups this stylesheet across a
# knitted document, so it is emitted once even with many tables.
lt_gt_style <- function(x) {
  lt::lt_css(x, system.file("css", "gt-style.css", package = "gsDesign2"))
}

#' @rdname as_lt
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
#'   as_lt()
#'
#' # Example 2 ----
#' fixed_design_fh(
#'   alpha = alpha, power = 1 - beta,
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   study_duration = study_duration, ratio = ratio
#' ) |>
#'   summary() |>
#'   as_lt()
as_lt.fixed_design_summary <- function(x, title = NULL, footnote = NULL, ...) {
  if (is.null(title)) title <- attr(x, "title")
  if (is.null(footnote)) footnote <- attr(x, "footnote")

  ans <- lt::lt(as.data.frame(x)) |>
    lt::lt_header(title = title)

  if (!isFALSE(footnote)) {
    ans <- ans |>
      lt::lt_footnote(text = footnote, where = "title")
  }

  lt_gt_style(ans)
}

#' @rdname as_lt
#'
#' @param title A string to specify the title of the table.
#' @param subtitle A string to specify the subtitle of the table.
#' @param colname_spanner A string to specify the spanner of the table.
#' @param colname_spannersub A vector of strings to specify the spanner details
#'   of the table.
#' @param footnote A list containing `content`, `location`, and `attr`.
#'   `content` is a vector of string to specify the footnote text; `location` is
#'   a vector of string to specify the locations to put the superscript of the
#'   footnote index; `attr` is a vector of string to specify the attributes of
#'   the footnotes, for example, `c("colname", "title", "subtitle", "analysis",
#'   "spanner")`. To disable footnotes, use `footnote = FALSE`.
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
#'   as_lt()
#'
#' gs_power_ahr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_lt()
#'
#' gs_design_wlr() |>
#'   summary() |>
#'   as_lt()
#'
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_lt()
#'
#' gs_power_combo() |>
#'   summary() |>
#'   as_lt()
#'
#' gs_design_rd() |>
#'   summary() |>
#'   as_lt()
#'
#' gs_power_rd() |>
#'   summary() |>
#'   as_lt()
#'
#' # Example 2 ----
#' # Usage of title = ..., subtitle = ...
#' # to edit the title/subtitle
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_lt(
#'     title = "Bound Summary",
#'     subtitle = "from gs_power_wlr"
#'   )
#'
#' # Example 3 ----
#' # Usage of colname_spanner = ..., colname_spannersub = ...
#' # to edit the spanner and its sub-spanner
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_lt(
#'     colname_spanner = "Cumulative probability to cross boundaries",
#'     colname_spannersub = c("under H1", "under H0")
#'   )
#'
#' # Example 4 ----
#' # Usage of footnote = ...
#' # to edit the footnote
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_lt(
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
#'   as_lt(display_bound = "Efficacy")
#'
#' # Example 6 ----
#' # Usage of display_columns = ...
#' # to select the columns to display in the summary table
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_lt(display_columns = c("Analysis", "Bound", "Nominal p", "Z", "Probability"))
#' }
as_lt.gs_design_summary <- function(
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

  ans <- lt::lt(as.data.frame(parts$x), row_group = "Analysis") |>
    lt::lt_spanner(
      label = colname_spanner,
      columns = colname_spannersub
    ) |>
    lt::lt_header(title = parts$title, subtitle = parts$subtitle)

  # Add footnotes
  add_footnote <- !isFALSE(footnote)
  footnote <- parts$footnote
  for (i in seq_along(footnote$content)) {
    att <- footnote$attr[i]
    if (att == "colname") {
      ans <- lt::lt_footnote(ans, text = footnote$content[i],
                             where = "column", columns = footnote$location[i])
    } else if (att == "title") {
      ans <- lt::lt_footnote(ans, text = footnote$content[i], where = "title")
    } else if (att == "subtitle") {
      ans <- lt::lt_footnote(ans, text = footnote$content[i], where = "subtitle")
    } else if (att == "analysis") {
      ans <- lt::lt_footnote(ans, text = footnote$content[i],
                             where = "group", columns = "Analysis",
                             match = "starts_with")
    } else if (att == "spanner") {
      ans <- lt::lt_footnote(ans, text = footnote$content[i],
                             where = "spanner", columns = colname_spanner)
    }
  }

  # Add footnote for non-binding design
  footnote_nb <- if (add_footnote) gsd_footnote_nb(x_old, parts$alpha)
  if (!is.null(footnote_nb)) {
    rows <- which(gsd_footnote_row(parts$x, display_bound[1]))
    ans <- lt::lt_footnote(ans, text = footnote_nb, where = "body",
                           columns = colname_spannersub[2], rows = rows)
  }

  lt_gt_style(ans)
}
