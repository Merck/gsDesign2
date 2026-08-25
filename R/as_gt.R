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
#' `as_gt()` is deprecated in favor of [lt()], which produces a lightweight
#' HTML table without the heavy \pkg{gt} dependency. `as_gt()` is kept for one
#' release so existing code that customizes the output with \pkg{gt} functions
#' keeps working; it still returns a `gt_tbl` object and requires \pkg{gt} to be
#' installed. New code should use [lt()]; see [lt-methods] for the available
#' arguments, which mirror those of `as_gt()`.
#'
#' @param x A summary object of a fixed or group sequential design.
#' @param title,subtitle,colname_spanner,colname_spannersub,footnote,display_bound,display_columns,display_inf_bound
#'   See [lt-methods] for the meaning of these arguments.
#' @param ... Additional arguments (not used).
#'
#' @return A `gt_tbl` object.
#'
#' @seealso [lt()], [lt-methods]
#'
#' @export
as_gt <- function(x, ...) {
  .Deprecated("lt", package = "gsDesign2",
    msg = paste(
      "as_gt() is deprecated and will be removed in a future release;",
      "please use lt() instead."
    ))
  UseMethod("as_gt", x)
}

# stop with an informative message when gt is not installed, since it is only
# a suggested (optional) dependency now that as_gt() is deprecated
assert_gt_installed <- function() {
  if (!requireNamespace("gt", quietly = TRUE)) stop(
    "The 'gt' package is required by the deprecated as_gt(); ",
    "install it with install.packages('gt'), or use lt() instead.",
    call. = FALSE
  )
}

#' @rdname as_gt
#' @export
as_gt.fixed_design_summary <- function(x, title = NULL, footnote = NULL, ...) {
  assert_gt_installed()
  if (is.null(title)) title <- attr(x, "title")
  if (is.null(footnote)) footnote <- attr(x, "footnote")

  ans <- gt::gt(x) |>
    gt::tab_header(title = title)

  if (!isFALSE(footnote)) {
    ans <- ans |>
      gt::tab_footnote(
        footnote = footnote,
        locations = gt::cells_title(group = "title")
      )
  }

  return(ans)
}

#' @rdname as_gt
#' @export
as_gt.gs_design_summary <- function(
    x,
    title = NULL,
    subtitle = NULL,
    colname_spanner = "Cumulative boundary crossing probability",
    colname_spannersub = c("Alternate hypothesis", "Null hypothesis"),
    footnote = NULL,
    display_bound = c("Efficacy", "Futility", "Harm"),
    display_columns = NULL,
    display_inf_bound = FALSE,
    ...) {
  assert_gt_installed()

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

# Only purpose of the method below is to fix S3 redirection when gsDesign2 is
# loaded after simtrial, which masks the as_gt() generic from simtrial

#' @export
as_gt.simtrial_gs_wlr <- function(x, ...) {
  f <- getFromNamespace("as_gt.simtrial_gs_wlr", "simtrial")
  f(x, ...)
}
