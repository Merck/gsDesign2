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

#' Write summary table of a fixed or group sequential design object to an RTF file
#'
#' @param x A summary object of a fixed or group sequential design.
#' @param ... Additional arguments (not used).
#'
#' @return `as_rtf()` returns the input `x` invisibly.
#'
#' @export
as_rtf <- function(x, ...) {
  UseMethod("as_rtf", x)
}

#' @rdname as_rtf
#'
#' @param title A string to specify the title of the RTF table.
#' @param footnote A string to specify the footnote of the RTF table.
#' @inheritParams r2rtf::rtf_page
#' @inheritParams r2rtf::rtf_body
#' @param file File path for the output.
#'
#' @export
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
#' # AHR ----
#' # under fixed power
#' x <- fixed_design_ahr(
#'   alpha = alpha, power = 1 - beta,
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   study_duration = study_duration, ratio = ratio
#' ) |> summary()
#' x |> as_rtf(file = tempfile(fileext = ".rtf"))
#' x |> as_rtf(title = "Fixed design", file = tempfile(fileext = ".rtf"))
#' x |> as_rtf(
#'   footnote = "Power computed with average hazard ratio method given the sample size",
#'   file = tempfile(fileext = ".rtf")
#' )
#' x |> as_rtf(text_font_size = 10, file = tempfile(fileext = ".rtf"))
#'
#' # FH ----
#' # under fixed power
#' fixed_design_fh(
#'   alpha = alpha, power = 1 - beta,
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   study_duration = study_duration, ratio = ratio
#' ) |>
#'   summary() |>
#'   as_rtf(file = tempfile(fileext = ".rtf"))
as_rtf.fixed_design_summary <- function(
    x,
    title = NULL,
    footnote = NULL,
    col_rel_width = NULL,
    orientation = c("portrait", "landscape"),
    text_font_size = 9,
    file,
    ...) {
  orientation <- match.arg(orientation)
  if (is.null(title)) title <- attr(x, "title")
  if (is.null(footnote)) footnote <- attr(x, "footnote")
  title <- paste(title, "{^a}")
  footnote <- paste("{^a}", footnote)

  # set default column width
  n_row <- nrow(x)
  n_col <- ncol(x)
  check_rel_width(col_rel_width, n_col)

  # set column header
  colheader <- paste(names(x), collapse = " | ")

  # set relative width
  rel_width <- col_rel_width %||% c(2, rep(1, n_col - 1))

  # Column boarder
  border_top <- border_left <- rep("single", n_col)

  # Using order number to customize row format
  text_justification <- c("l", rep("c", n_col - 1))
  text_format <- rep("", n_col)
  text_indent <- matrix(0, nrow = n_row, ncol = n_col)

  # Use r2rtf
  ans <- x |>
    r2rtf::rtf_page(orientation = orientation) |>
    r2rtf::rtf_title(title) |>
    r2rtf::rtf_colheader(colheader, rel_width, text_font_size = text_font_size) |>
    r2rtf::rtf_body(
      rel_width,
      border_left = border_left,
      border_top = border_top,
      text_justification = text_justification,
      text_indent_first = text_indent,
      text_indent_left = text_indent,
      text_format = text_format,
      text_font_size = text_font_size
    )

  # Prepare output
  rtf_write(ans, file, footnote, text_font_size)

  invisible(x)
}

check_rel_width <- function(width, n_col) {
  if (!is.null(width) && n_col != length(width)) stop(
    "The length of 'col_rel_width' (", length(width), ") differs with ",
    "the number of columns in 'x' (", n_col, ")."
  )
}

#' @rdname  as_rtf
#'
#' @param title A string to specify the title of the RTF table.
#' @param subtitle A string to specify the subtitle of the RTF table.
#' @param colname_spanner A string to specify the spanner of the RTF table.
#' @param colname_spannersub A vector of strings to specify the spanner details of the RTF table.
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
#' @inheritParams r2rtf::rtf_page
#' @inheritParams r2rtf::rtf_body
#' @param file File path for the output.
#'
#' @export
#'
#' @examples
#' #'
#' \donttest{
#' # the default output
#'
#' gs_design_ahr() |>
#'   summary() |>
#'   as_rtf(file = tempfile(fileext = ".rtf"))
#'
#' gs_power_ahr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_rtf(file = tempfile(fileext = ".rtf"))
#'
#' gs_design_wlr() |>
#'   summary() |>
#'   as_rtf(file = tempfile(fileext = ".rtf"))
#'
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_rtf(file = tempfile(fileext = ".rtf"))
#'
#'
#' gs_power_combo() |>
#'   summary() |>
#'   as_rtf(file = tempfile(fileext = ".rtf"))
#'
#' gs_design_rd() |>
#'   summary() |>
#'   as_rtf(file = tempfile(fileext = ".rtf"))
#'
#' gs_power_rd() |>
#'   summary() |>
#'   as_rtf(file = tempfile(fileext = ".rtf"))
#'
#' # usage of title = ..., subtitle = ...
#' # to edit the title/subtitle
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_rtf(
#'     title = "Bound Summary",
#'     subtitle = "from gs_power_wlr",
#'     file = tempfile(fileext = ".rtf")
#'   )
#'
#' # usage of colname_spanner = ..., colname_spannersub = ...
#' # to edit the spanner and its sub-spanner
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_rtf(
#'     colname_spanner = "Cumulative probability to cross boundaries",
#'     colname_spannersub = c("under H1", "under H0"),
#'     file = tempfile(fileext = ".rtf")
#'   )
#'
#' # usage of footnote = ...
#' # to edit the footnote
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_rtf(
#'     footnote = list(
#'       content = c(
#'         "approximate weighted hazard ratio to cross bound.",
#'         "wAHR is the weighted AHR.",
#'         "the crossing probability.",
#'         "this table is generated by gs_power_wlr."
#'       ),
#'       location = c("~wHR at bound", NA, NA, NA),
#'       attr = c("colname", "analysis", "spanner", "title")
#'     ),
#'     file = tempfile(fileext = ".rtf")
#'   )
#'
#' # usage of display_bound = ...
#' # to either show efficacy bound or futility bound, or both(default)
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_rtf(
#'     display_bound = "Efficacy",
#'     file = tempfile(fileext = ".rtf")
#'   )
#'
#' # usage of display_columns = ...
#' # to select the columns to display in the summary table
#' gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
#'   summary() |>
#'   as_rtf(
#'     display_columns = c("Analysis", "Bound", "Nominal p", "Z", "Probability"),
#'     file = tempfile(fileext = ".rtf")
#'   )
#' }
as_rtf.gs_design_summary <- function(
    x,
    title = NULL,
    subtitle = NULL,
    colname_spanner = "Cumulative boundary crossing probability",
    colname_spannersub = c("Alternate hypothesis", "Null hypothesis"),
    footnote = NULL,
    display_bound = c("Efficacy", "Futility"),
    display_columns = NULL,
    display_inf_bound = TRUE,
    col_rel_width = NULL,
    orientation = c("portrait", "landscape"),
    text_font_size = 9,
    file,
    ...) {
  orientation <- match.arg(orientation)
  x_old <- x

  parts <- gsd_parts(
    x, title, subtitle, colname_spannersub, footnote,
    display_bound, display_columns, display_inf_bound,
    function(x) {
      x2 <- data.frame(lapply(x, function(x) trimws(formatC(x, flag = "-"), "r")))
      names(x2) <- names(x)
      x2
    }
  )
  x <- parts$x
  title <- parts$title
  subtitle <- parts$subtitle

  # Set rtf parameters ----
  n_col <- ncol(x)
  n_row <- nrow(x)
  check_rel_width(col_rel_width, n_col)
  colheader <- c(
    paste0(" | ", colname_spanner),
    paste(names(x)[-1], collapse = " | ")
  )

  # set relative width
  rel_width_body <- col_rel_width %||% rep(1, n_col)

  rel_width_head <- rel_width_body[-1]
  rel_width_head <- list(
    c(sum(rel_width_head[2:(n_col - 2)]), sum(tail(rel_width_head, 2))),
    rel_width_head
  )

  # column boarder
  border_top_body <- border_bottom <- border_left_body <- rep("single", n_col)
  border_top_head <- border_top_body[-1]
  border_left_head <- list(rep("single", 2), border_top_head)

  # Using order number to customize row format
  text_justification <- c("l", "l", rep("c", n_col - 2))
  text_format <- rep("", n_col)
  text_indent <- matrix(0, nrow = n_row, ncol = n_col)

  # Add footnotes ----
  # initialization for footnote
  footnotes <- NULL
  footnote <- parts$footnote
  # footnote markers (a, b, c, ... from letters[idx])
  idx <- 0L
  marker <- function() letters[idx <<- idx + 1L]

  for (i in seq_along(footnote$content)) {
    att <- footnote$attr[i]
    mkr <- marker()
    if (att == "colname") {
      colheader[2] <- sub(
        footnote$location[i],
        paste0(footnote$location[i], " {^", mkr, "}"),
        colheader[2]
      )
    } else if (att == "title") {
      title <- paste0(title, " \\super ", mkr)
    } else if (att == "subtitle") {
      subtitle <- paste0(subtitle, " {\\super ", mkr, "}")
    } else if (att == "analysis") {
      x["Analysis"] <- lapply(x["Analysis"], function(z) paste0(z, " {^", mkr, "}"))
    } else if (att == "spanner") {
      colheader[1] <- sub(
        colname_spanner,
        paste0(colname_spanner, " {^", mkr, "}"),
        colheader[1]
      )
    }
    marked_footnote <- paste0("{\\super ", mkr, "} ", footnote$content[i])
    footnotes <- c(footnotes, marked_footnote)
  }

  # add footnote for non-binding design
  footnote_nb <- gsd_footnote_nb(x_old, parts$alpha)
  if (!is.null(footnote_nb)) {
    mkr <- marker()
    i <- gsd_footnote_row(x, display_bound[1])
    j <- colname_spannersub[2]
    x[i, j] <- paste0(x[i, j], " {^", mkr, "}")
    footnotes <- c(footnotes, paste0("{\\super ", mkr, "} ", footnote_nb))
  }

  # Output ----
  # use r2rtf
  ans <- x |>
    r2rtf::rtf_page(orientation = orientation) |>
    r2rtf::rtf_title(title, subtitle, text_convert = FALSE) |>
    r2rtf::rtf_colheader(
      colheader[1], rel_width_head[[1]], border_left_head[[1]],
      text_font_size = text_font_size
    ) |>
    r2rtf::rtf_colheader(
      colheader[2], rel_width_head[[2]], border_left_head[[2]],
      border_top = border_top_head, text_font_size = text_font_size
    ) |>
    r2rtf::rtf_body(
      rel_width_body, page_by = "Analysis",
      border_left = border_left_body,
      border_top = border_top_body,
      border_bottom = border_bottom,
      text_justification = text_justification,
      text_indent_first = text_indent,
      text_indent_left = text_indent,
      text_format = text_format,
      text_font_size = text_font_size
    )

  # Prepare output
  rtf_write(ans, file, footnotes, text_font_size, text_convert = FALSE)

  invisible(x_old)
}

# write RTF with (optional) footnotes
rtf_write <- function(x, file, footnote = NULL, size, ...) {
  if (!is.null(footnote)) {
    footnote <- paste(footnote, collapse = "\\line")
    x <- r2rtf::rtf_footnote(x, footnote, text_font_size = size, ...)
  }
  r2rtf::write_rtf(r2rtf::rtf_encode(x), file)
}
