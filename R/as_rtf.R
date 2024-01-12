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
#' # AHR ----
#' # under fixed power
#' x <- fixed_design_ahr(
#'   alpha = alpha, power = 1 - beta,
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   study_duration = study_duration, ratio = ratio
#' ) %>% summary()
#' x %>% as_rtf(file = tempfile(fileext = ".rtf"))
#' x %>% as_rtf(title = "Fixed design", file = tempfile(fileext = ".rtf"))
#' x %>% as_rtf(
#'   footnote = "Power computed with average hazard ratio method given the sample size",
#'   file = tempfile(fileext = ".rtf")
#' )
#' x %>% as_rtf(text_font_size = 10, file = tempfile(fileext = ".rtf"))
#'
#' # FH ----
#' # under fixed power
#' fixed_design_fh(
#'   alpha = alpha, power = 1 - beta,
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   study_duration = study_duration, ratio = ratio
#' ) %>%
#'   summary() %>%
#'   as_rtf(file = tempfile(fileext = ".rtf"))
as_rtf.fixed_design <- function(
    x,
    title = NULL,
    footnote = NULL,
    col_rel_width = NULL,
    orientation = c("portrait", "landscape"),
    text_font_size = 9,
    file,
    ...) {
  orientation <- match.arg(orientation)

  # get the design method
  if ("ahr" %in% class(x)) {
    design_mtd <- "ahr"
  } else if ("fh" %in% class(x)) {
    design_mtd <- "fh"
  } else if ("mb" %in% class(x)) {
    design_mtd <- "mb"
  } else if ("lf" %in% class(x)) {
    design_mtd <- "lf"
  } else if ("rd" %in% class(x)) {
    design_mtd <- "rd"
  } else if ("maxcombo" %in% class(x)) {
    design_mtd <- "maxcombo"
  } else if ("milestone" %in% class(x)) {
    design_mtd <- "milestone"
  } else if ("rmst" %in% class(x)) {
    design_mtd <- "rmst"
  } else if ("rd" %in% class(x)) {
    design_mtd <- "rd"
  }

  # set the default title
  if (is.null(title)) {
    title <- switch(design_mtd,
      "ahr" = {
        paste0("Fixed Design under AHR Method", " {^a}")
      },
      "fh" = {
        paste0("Fixed Design under Fleming-Harrington Method", " {^a}")
      },
      "mb" = {
        paste0("Fixed Design under Magirr-Burman Method", " {^a}")
      },
      "lf" = {
        paste0("Fixed Design under Lachin and Foulkes Method", " {^a}")
      },
      "rd" = {
        paste0("Fixed Design of Risk Difference under Farrington-Manning Method", " {^a}")
      },
      "maxcombo" = {
        paste0("Fixed Design under MaxCombo Method", " {^a}")
      },
      "milestone" = {
        paste0("Fixed Design under Milestone Method", " {^a}")
      },
      "rmst" = {
        paste0("Fixed Design under Restricted Mean Survival Time Method", " {^a}")
      },
      "rd" = {
        paste0("Fixed Design of Risk Difference", " {^a}")
      }
    )
  }


  # set the default footnote
  if (is.null(footnote)) {
    footnote <- switch(design_mtd,
      "ahr" = {
        paste0("{^a} ", "Power computed with average hazard ratio method.")
      },
      "fh" = {
        paste0(
          "{^a} ",
          "Power for Fleming-Harrington test ",
          substr(x$Design, 19, nchar(x$Design)),
          " using method of Yung and Liu."
        )
      },
      "mb" = {
        paste0(
          "{^a} ",
          "Power for ",
          x$Design,
          " computed with method of Yung and Liu."
        )
      },
      "lf" = {
        paste0(
          "{^a} ",
          "Power using Lachin and Foulkes method applied
          using expected average hazard ratio (AHR) at time of planned analysis."
        )
      },
      "rd" = {
        paste0(
          "{^a} ",
          "Risk difference power without continuity correction using method of Farrington and Manning."
        )
      },
      "maxcombo" = {
        paste0(
          "{^a} ",
          "Power for MaxCombo test with Fleming-Harrington tests",
          substr(x$Design, 9, nchar(x$Design)), "."
        )
      },
      "milestone" = {
        paste0("{^a} ", "Power for ", x$Design, " computed with method of Yung and Liu.")
      },
      "rmst" = {
        paste0("{^a} ", "Power for ", x$Design, " computed with method of Yung and Liu.")
      }
    )
  }

  # set default column width
  n_row <- nrow(x)
  n_col <- ncol(x)
  if (!is.null(col_rel_width) && !(n_col == length(col_rel_width))) {
    stop(
      "col_rel_width must have the same length (has ",
      length(col_rel_width),
      ") as `x` has number of columns (has ",
      n_col, ").",
      call. = FALSE
    )
  }

  # set column header
  colheader <-
    paste0(paste(names(x), collapse = " | "))

  # set relative width
  if (is.null(col_rel_width)) {
    rel_width <- c(2, rep(1, (n_col - 1)))
  } else {
    rel_width <- col_rel_width
  }

  # Column boarder
  border_top <- rep("single", n_col)
  border_left <- rep("single", n_col)

  # Using order number to customize row format
  text_justification <- c("l", rep("c", n_col - 1))
  text_format <- rep("", n_col)
  text_indent <- matrix(0, nrow = n_row, ncol = n_col)

  # Use r2rtf
  ans <- x %>%
    r2rtf::rtf_page(orientation = orientation) %>%
    r2rtf::rtf_title(title) %>%
    r2rtf::rtf_colheader(
      colheader = colheader,
      col_rel_width = rel_width,
      text_font_size = text_font_size
    ) %>%
    r2rtf::rtf_body(
      col_rel_width = rel_width,
      border_left = border_left,
      border_top = border_top,
      text_justification = text_justification,
      text_indent_first = text_indent,
      text_indent_left = text_indent,
      text_format = text_format,
      text_font_size = text_font_size
    )

  if (!is.null(footnote)) {
    ans <- ans %>%
      r2rtf::rtf_footnote(footnote,
        text_font_size = text_font_size
      )
  }

  # Prepare output
  ans %>%
    r2rtf::rtf_encode() %>%
    r2rtf::write_rtf(file = file)

  invisible(x)
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
#' @param full_alpha The full alpha used in the design, the default is 0.025.
#    If the cumulative alpha for final analysis is less than the `full_alpha`
#'   when the futility bound is non-binding, a footnote will be displayed, saying
#'   the smaller value subtracts the probability of crossing a futility bound before
#'   crossing an efficacy bound at a later analysis under the null hypothesis.
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
#' library(dplyr)
#'
#' gs_design_ahr() %>%
#'   summary() %>%
#'   as_rtf(file = tempfile(fileext = ".rtf"))
#'
#' gs_power_ahr() %>%
#'   summary() %>%
#'   as_rtf(file = tempfile(fileext = ".rtf"))
#'
#' gs_design_wlr() %>%
#'   summary() %>%
#'   as_rtf(file = tempfile(fileext = ".rtf"))
#'
#' gs_power_wlr() %>%
#'   summary() %>%
#'   as_rtf(file = tempfile(fileext = ".rtf"))
#'
#'
#' gs_power_combo() %>%
#'   summary() %>%
#'   as_rtf(file = tempfile(fileext = ".rtf"))
#'
#' gs_design_rd() %>%
#'   summary() %>%
#'   as_rtf(file = tempfile(fileext = ".rtf"))
#'
#' gs_power_rd() %>%
#'   summary() %>%
#'   as_rtf(file = tempfile(fileext = ".rtf"))
#'
#' # usage of title = ..., subtitle = ...
#' # to edit the title/subtitle
#' gs_power_wlr() %>%
#'   summary() %>%
#'   as_rtf(
#'     title = "Bound Summary",
#'     subtitle = "from gs_power_wlr",
#'     file = tempfile(fileext = ".rtf")
#'   )
#'
#' # usage of colname_spanner = ..., colname_spannersub = ...
#' # to edit the spanner and its sub-spanner
#' gs_power_wlr() %>%
#'   summary() %>%
#'   as_rtf(
#'     colname_spanner = "Cumulative probability to cross boundaries",
#'     colname_spannersub = c("under H1", "under H0"),
#'     file = tempfile(fileext = ".rtf")
#'   )
#'
#' # usage of footnote = ...
#' # to edit the footnote
#' gs_power_wlr() %>%
#'   summary() %>%
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
#' gs_power_wlr() %>%
#'   summary() %>%
#'   as_rtf(
#'     display_bound = "Efficacy",
#'     file = tempfile(fileext = ".rtf")
#'   )
#'
#' # usage of display_columns = ...
#' # to select the columns to display in the summary table
#' gs_power_wlr() %>%
#'   summary() %>%
#'   as_rtf(
#'     display_columns = c("Analysis", "Bound", "Nominal p", "Z", "Probability"),
#'     file = tempfile(fileext = ".rtf")
#'   )
#' }
as_rtf.gs_design <- function(
    x,
    title = NULL,
    subtitle = NULL,
    colname_spanner = "Cumulative boundary crossing probability",
    colname_spannersub = c("Alternate hypothesis", "Null hypothesis"),
    footnote = NULL,
    display_bound = c("Efficacy", "Futility"),
    display_columns = NULL,
    display_inf_bound = TRUE,
    full_alpha = 0.025,
    col_rel_width = NULL,
    orientation = c("portrait", "landscape"),
    text_font_size = 9,
    file,
    ...) {
  orientation <- match.arg(orientation)

  method <- class(x)[class(x) %in% c("ahr", "wlr", "combo", "rd")]
  x_alpha <- max((x %>% dplyr::filter(Bound == display_bound[1]))[["Null hypothesis"]])
  x_non_binding <- "non_binding" %in% class(x)
  x_k <- lapply(x$Analysis, function(x) {
    return(as.numeric(substring(x, 11, 11)))
  }) %>% unlist()
  x_old <- x

  x <- data.frame(lapply(x, function(x) trimws(formatC(x, flag = "-"), "r")))
  names(x) <- names(x_old)

  # Set defaults ----
  # set different default title to different methods
  if (method == "ahr" && is.null(title)) {
    title <- "Bound summary for AHR design"
  }
  if (method == "wlr" && is.null(title)) {
    title <- "Bound summary for WLR design"
  }
  if (method == "combo" && is.null(title)) {
    title <- "Bound summary for MaxCombo design"
  }

  if (method == "rd" && is.null(title)) {
    title <- "Bound summary of Binary Endpoint"
  }

  # set different default subtitle to different methods
  if (method == "ahr" && is.null(subtitle)) {
    subtitle <- "AHR approximations of ~HR at bound"
  }
  if (method == "wlr" && is.null(subtitle)) {
    subtitle <- "WLR approximation of ~wHR at bound"
  }
  if (method == "combo" && is.null(subtitle)) {
    subtitle <- "MaxCombo approximation"
  }
  if (method == "rd" && is.null(subtitle)) {
    subtitle <- "measured by risk difference"
  }

  # set different default columns to display
  if (is.null(display_columns)) {
    if (method == "ahr") {
      display_columns <- c(
        "Analysis", "Bound", "Z", "Nominal p",
        "~HR at bound", "Alternate hypothesis", "Null hypothesis"
      )
    } else if (method == "wlr") {
      display_columns <- c(
        "Analysis", "Bound", "Z", "Nominal p",
        "~wHR at bound", "Alternate hypothesis", "Null hypothesis"
      )
    } else if (method == "combo") {
      display_columns <- c(
        "Analysis", "Bound", "Z", "Nominal p",
        "Alternate hypothesis", "Null hypothesis"
      )
    } else if (method == "rd") {
      display_columns <- c(
        "Analysis", "Bound", "Z", "Nominal p",
        "~Risk difference at bound", "Alternate hypothesis", "Null hypothesis"
      )
    }
  }
  # filter the columns to display as the output
  ## if `Probability` is selected to output, then transform it to `c("Alternate hypothesis", "Null hypothesis")`
  if ("Probability" %in% display_columns) {
    display_columns <- display_columns[!display_columns == "Probability"]
    display_columns <- c(display_columns, "Alternate hypothesis", "Null hypothesis")
  }
  ## check if the `display_columns` are included in `x` output
  if (sum(!(display_columns %in% names(x))) >= 1) {
    stop("as_rtf: the variable names in display_columns is not outputted in the summary_bound object!")
  } else {
    x <- x %>% dplyr::select(dplyr::all_of(display_columns))
  }

  # set different default footnotes to different methods
  if (method == "ahr" && is.null(footnote)) {
    footnote <- list(
      content = c(
        ifelse(
          "Nominal p" %in% display_columns,
          paste(
            "One-sided p-value for experimental vs control treatment.",
            "Value < 0.5 favors experimental, > 0.5 favors control."
          ),
          NA
        ),
        ifelse(
          "~HR at bound" %in% display_columns,
          "Approximate hazard ratio to cross bound.",
          NA
        )
      ),
      location = c(
        ifelse("Nominal p" %in% display_columns, "Nominal p", NA),
        ifelse("~HR at bound" %in% display_columns, "~HR at bound", NA)
      ),
      attr = c(
        ifelse("Nominal p" %in% display_columns, "colname", NA),
        ifelse("~HR at bound" %in% display_columns, "colname", NA)
      )
    )
    footnote <- lapply(footnote, function(x) x[!is.na(x)])
  }
  if (method == "wlr" && is.null(footnote)) {
    footnote <- list(
      content = c(
        ifelse(
          "Nominal p" %in% display_columns,
          paste(
            "One-sided p-value for experimental vs control treatment.",
            "Value < 0.5 favors experimental, > 0.5 favors control."
          ),
          NA
        ),
        ifelse(
          "~wHR at bound" %in% display_columns,
          "Approximate hazard ratio to cross bound.",
          NA
        ),
        "wAHR is the weighted AHR."
      ),
      location = c(
        ifelse("Nominal p" %in% display_columns, "Nominal p", NA),
        ifelse("~wHR at bound" %in% display_columns, "~wHR at bound", NA),
        NA
      ),
      attr = c(
        ifelse("Nominal p" %in% display_columns, "colname", NA),
        ifelse("~wHR at bound" %in% display_columns, "colname", NA),
        "analysis"
      )
    )
    footnote <- lapply(footnote, function(x) x[!is.na(x)])
  }
  if (method == "combo" && is.null(footnote)) {
    footnote <- list(
      content = c(
        ifelse(
          "Nominal p" %in% display_columns,
          paste(
            "One-sided p-value for experimental vs control treatment.",
            "Value < 0.5 favors experimental, > 0.5 favors control."
          ),
          NA
        ),
        "EF is event fraction. AHR  is under regular weighted log rank test."
      ),
      location = c(
        ifelse("Nominal p" %in% display_columns, "Nominal p", NA),
        NA
      ),
      attr = c(
        ifelse("Nominal p" %in% display_columns, "colname", NA),
        "analysis"
      )
    )
    footnote <- lapply(footnote, function(x) x[!is.na(x)])
  }
  if (method == "rd" && is.null(footnote)) {
    footnote <- list(
      content = c(ifelse(
        "Nominal p" %in% display_columns,
        paste(
          "One-sided p-value for experimental vs control treatment.",
          "Value < 0.5 favors experimental, > 0.5 favors control."
        ),
        NA
      )),
      location = c(ifelse("Nominal p" %in% display_columns, "Nominal p", NA)),
      attr = c(ifelse("Nominal p" %in% display_columns, "colname", NA))
    )
    footnote <- lapply(footnote, function(x) x[!is.na(x)])
  }

  # Filter out inf bound ----
  x <- x %>%
    subset(!is.na(`Alternate hypothesis`)) %>%
    subset(!is.na(`Null hypothesis`))

  # organize data
  x <- x %>%
    subset(Bound %in% display_bound) %>%
    dplyr::arrange(Analysis)

  # Set rtf parameters ----
  n_col <- ncol(x)
  n_row <- nrow(x)
  if (!is.null(col_rel_width) && !(n_col == length(col_rel_width))) {
    stop(
      "col_rel_width must have the same length (has ",
      length(col_rel_width),
      ") as `x` has number of columns (has ",
      n_col, ").",
      call. = FALSE
    )
  }

  # set column header
  names(x)[names(x) == "Alternate hypothesis"] <- colname_spannersub[1]
  names(x)[names(x) == "Null hypothesis"] <- colname_spannersub[2]

  colheader <- c(
    paste0(" | ", colname_spanner),
    paste(names(x)[-1], collapse = " | ")
  )

  # set relative width
  if (is.null(col_rel_width)) {
    rel_width_body <- rep(1, n_col)
  } else {
    rel_width_body <- col_rel_width
  }

  rel_width_head <- rel_width_body[2:length(rel_width_body)]
  rel_width_head <- list(
    c(
      sum(rel_width_head[2:(n_col - 2)]),
      sum(tail(rel_width_head, n = 2))
    ),
    rel_width_head
  )

  # column boarder
  border_top_head <- rep("single", (n_col - 1))
  border_top_body <- rep("single", n_col)
  border_bottom <- rep("single", n_col)
  border_left_head <- list(
    c("single", "single"),
    rep("single", n_col - 1)
  )
  border_left_body <- c("single", border_left_head[[2]])

  # Using order number to customize row format
  text_justification <- c("l", "l", rep("c", n_col - 2))
  text_format <- rep("", n_col)
  text_indent <- matrix(0, nrow = n_row, ncol = n_col)

  # Add footnotes ----
  # initialization for footnote
  footnotes <- NULL
  alpha_utf_int <- 96

  if (!is.null(footnote$content)) {
    if (length(footnote$content) > 0) {
      for (i in seq_along(footnote$content)) {
        alpha_utf_int <- alpha_utf_int + 1
        if (footnote$attr[i] == "colname") {
          colheader[2] <- sub(
            footnote$location[i],
            paste0(footnote$location[i], " {^", intToUtf8(alpha_utf_int), "}"),
            colheader[2]
          )
        } else if (footnote$attr[i] == "title") {
          title <- paste0(title, " \\super ", intToUtf8(alpha_utf_int))
        } else if (footnote$attr[i] == "subtitle") {
          subtitle <- paste0(subtitle, " {\\super ", intToUtf8(alpha_utf_int), "}")
        } else if (footnote$attr[i] == "analysis") {
          x["Analysis"] <- lapply(x["Analysis"], function(z) paste0(z, " {^", intToUtf8(alpha_utf_int), "}"))
        } else if (footnote$attr[i] == "spanner") {
          colheader[1] <- sub(
            colname_spanner,
            paste0(colname_spanner, " {^", intToUtf8(alpha_utf_int), "}"),
            colheader[1]
          )
        }
        marked_footnote <- paste0("{\\super ", intToUtf8(alpha_utf_int), "} ", footnote$content[i])
        if (!is.null(footnotes)) {
          footnotes <- paste0(footnotes, "\\line", marked_footnote)
        } else {
          footnotes <- marked_footnote
        }
      }
    }
  }

  ## if it is non-binding design
  if (x_non_binding && (x_alpha < full_alpha)) {
    alpha_utf_int <- alpha_utf_int + 1

    x[
      (substring(x$Analysis, 1, 11) == paste0("Analysis: ", max(x_k))) &
        x$Bound == display_bound[1], colname_spannersub[2]
    ] <- paste0(
      x[
        (substring(x$Analysis, 1, 11) == paste0("Analysis: ", max(x_k))) &
          x$Bound == display_bound[1], colname_spannersub[2]
      ],
      " {^", intToUtf8(alpha_utf_int), "}"
    )

    footnote_non_binding <- paste0(
      "{\\super ", intToUtf8(alpha_utf_int), "} ",
      "Cumulative alpha for final analysis ",
      "(", format(x_alpha, scientific = FALSE), ") ",
      "is less than the full alpha ",
      "(", format(full_alpha, scientific = FALSE), ") ",
      "when the futility bound is non-binding. ",
      "The smaller value subtracts the probability of ",
      "crossing a futility bound before ",
      "crossing an efficacy bound at a later analysis ",
      "(",
      format(full_alpha, scientific = FALSE),
      " - ",
      format(full_alpha - x_alpha, scientific = FALSE),
      " = ",
      format(x_alpha, scientific = FALSE),
      ") ",
      "under the null hypothesis."
    )

    if (!is.null(footnotes)) {
      footnotes <- paste0(footnotes, "\\line", footnote_non_binding)
    } else {
      footnotes <- footnote_non_binding
    }
  }

  # Output ----
  # use r2rtf
  ans <- x %>%
    r2rtf::rtf_page(orientation = orientation) %>%
    r2rtf::rtf_title(
      title = title,
      subtitle = subtitle,
      text_convert = FALSE
    ) %>%
    r2rtf::rtf_colheader(
      colheader = colheader[1],
      col_rel_width = rel_width_head[[1]],
      text_font_size = text_font_size,
      border_left = border_left_head[[1]]
    ) %>%
    r2rtf::rtf_colheader(
      colheader = colheader[2],
      border_top = border_top_head,
      border_left = border_left_head[[2]],
      col_rel_width = rel_width_head[[2]],
      text_font_size = text_font_size
    ) %>%
    r2rtf::rtf_body(
      page_by = "Analysis",
      col_rel_width = rel_width_body,
      border_left = border_left_body,
      border_top = border_top_body,
      border_bottom = border_bottom,
      text_justification = text_justification,
      text_indent_first = text_indent,
      text_indent_left = text_indent,
      text_format = text_format,
      text_font_size = text_font_size
    )

  if (!is.null(footnotes)) {
    ans <- ans %>%
      r2rtf::rtf_footnote(footnotes,
        text_font_size = text_font_size,
        text_convert = FALSE
      )
  }

  # Prepare output
  ans %>%
    r2rtf::rtf_encode() %>%
    r2rtf::write_rtf(file = file)

  invisible(x_old)
}
