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
#' library(dplyr)
#' library(tibble)
#'
#' # Enrollment rate
#' enroll_rate <- tibble(
#'   stratum = "All",
#'   duration = 18,
#'   rate = 20
#' )
#'
#' # Failure rates
#' fail_rate <- tibble(
#'   stratum = "All",
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
#'
#' # Type II error (1 - power)
#' beta <- 0.1
#'
#' # ------------------------- #
#' #        AHR                #
#' # ------------------------- #
#' # under fixed power
#' fixed_design(
#'   "ahr",
#'   alpha = alpha, power = 1 - beta,
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   study_duration = study_duration, ratio = ratio
#' ) %>%
#'   summary() %>%
#'   as_gt()
#'
#' # ------------------------- #
#' #        FH                 #
#' # ------------------------- #
#' # under fixed power
#' fixed_design(
#'   "fh",
#'   alpha = alpha, power = 1 - beta,
#'   enroll_rate = enroll_rate, fail_rate = fail_rate,
#'   study_duration = study_duration, ratio = ratio
#' ) %>%
#'   summary() %>%
#'   as_gt()
as_gt.fixed_design <- function(x, title = NULL, footnote = NULL, ...) {
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
        "Fixed Design under AHR Method"
      },
      "fh" = {
        "Fixed Design under Fleming-Harrington Method"
      },
      "mb" = {
        "Fixed Design under Magirr-Burman Method"
      },
      "lf" = {
        "Fixed Design under Lachin and Foulkes Method"
      },
      "rd" = {
        "Fixed Design of Risk Difference under Farrington-Manning Method"
      },
      "maxcombo" = {
        "Fixed Design under MaxCombo Method"
      },
      "milestone" = {
        "Fixed Design under Milestone Method"
      },
      "rmst" = {
        "Fixed Design under Restricted Mean Survival Time Method"
      },
      "rd" = {
        "Fixed Design of Risk Difference"
      }
    )
  }


  # set the default footnote
  if (is.null(footnote)) {
    footnote <- switch(design_mtd,
      "ahr" = {
        "Power computed with average hazard ratio method."
      },
      "fh" = {
        paste0(
          "Power for Fleming-Harrington test ",
          substr(x$design, 19, nchar(x$design)),
          " using method of Yung and Liu."
        )
      },
      "mb" = {
        paste0(
          "Power for ",
          x$design,
          " computed with method of Yung and Liu."
        )
      },
      "lf" = {
        "Power using Lachin and Foulkes method applied
        using expected average hazard ratio (AHR) at time of planned analysis."
      },
      "rd" = {
        "Risk difference power without continuity correction using method of Farrington and Manning."
      },
      "maxcombo" = {
        paste0(
          "Power for MaxCombo test with Fleming-Harrington tests",
          substr(x$design, 9, nchar(x$design)), "."
        )
      },
      "milestone" = {
        paste0("Power for ", x$design, " computed with method of Yung and Liu.")
      },
      "rmst" = {
        paste0("Power for ", x$design, " computed with method of Yung and Liu.")
      }
    )
  }

  ans <- x %>%
    dplyr::mutate(design = design_mtd) %>%
    gt::gt() %>%
    gt::tab_header(title = title) %>%
    gt::tab_footnote(footnote = footnote, locations = gt::cells_title(group = "title"))

  return(ans)
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
#' @param full_alpha The full alpha used in the design, the default is 0.025.
#    If the cumulative alpha for final analysis is less than the `full_alpha`
#'   when the futility bound is non-binding, a footnote will be displayed, saying
#'   the smaller value subtracts the probability of crossing a futility bound before
#'   crossing an efficacy bound at a later analysis under the null hypothesis.
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
#' # the default output
#' library(dplyr)
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
#' # usage of title = ..., subtitle = ...
#' # to edit the title/subtitle
#' gs_power_wlr() %>%
#'   summary() %>%
#'   as_gt(
#'     title = "Bound Summary",
#'     subtitle = "from gs_power_wlr"
#'   )
#'
#' # usage of colname_spanner = ..., colname_spannersub = ...
#' # to edit the spanner and its sub-spanner
#' gs_power_wlr() %>%
#'   summary() %>%
#'   as_gt(
#'     colname_spanner = "Cumulative probability to cross boundaries",
#'     colname_spannersub = c("under H1", "under H0")
#'   )
#'
#' # usage of footnote = ...
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
#' # usage of display_bound = ...
#' # to either show efficacy bound or futility bound, or both(default)
#' gs_power_wlr() %>%
#'   summary() %>%
#'   as_gt(display_bound = "Efficacy")
#'
#' # usage of display_columns = ...
#' # to select the columns to display in the summary table
#' gs_power_wlr() %>%
#'   summary() %>%
#'   as_gt(display_columns = c("Analysis", "Bound", "Nominal p", "Z", "Probability"))
#' }
as_gt.gs_design <- function(x,
                            title = NULL,
                            subtitle = NULL,
                            colname_spanner = "Cumulative boundary crossing probability",
                            colname_spannersub = c("Alternate hypothesis", "Null hypothesis"),
                            footnote = NULL,
                            display_bound = c("Efficacy", "Futility"),
                            display_columns = NULL,
                            display_inf_bound = TRUE,
                            full_alpha = 0.025,
                            ...) {
  method <- class(x)[class(x) %in% c("ahr", "wlr", "combo", "rd")]
  x_alpha <- max((x %>% dplyr::filter(Bound == display_bound[1]))[[colname_spannersub[2]]])
  x_non_binding <- "non_binding" %in% class(x)
  x_k <- lapply(x$Analysis, function(x) {
    return(as.numeric(substring(x, 11, 11)))
  }) %>% unlist()
  x_old <- x

  # --------------------------------------------- #
  #     set defaults                              #
  # --------------------------------------------- #
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
    stop("as_gt: the variable names in display_columns is not outputted in the summary_bound object!")
  } else {
    x <- x %>% dplyr::select(dplyr::all_of(display_columns))
  }

  # set different default footnotes to different methods
  if (method == "ahr" && is.null(footnote)) {
    footnote <- list(
      content = c(
        ifelse("~HR at bound" %in% display_columns,
          "Approximate hazard ratio to cross bound.", NA
        ),
        ifelse("Nominal p" %in% display_columns,
          "One-sided p-value for experimental vs control treatment.
          Value < 0.5 favors experimental, > 0.5 favors control.", NA
        )
      ),
      location = c(
        ifelse("~HR at bound" %in% display_columns, "~HR at bound", NA),
        ifelse("Nominal p" %in% display_columns, "Nominal p", NA)
      ),
      attr = c(
        ifelse("~HR at bound" %in% display_columns, "colname", NA),
        ifelse("Nominal p" %in% display_columns, "colname", NA)
      )
    )
    footnote <- lapply(footnote, function(x) x[!is.na(x)])
  }
  if (method == "wlr" && is.null(footnote)) {
    footnote <- list(
      content = c(
        ifelse("~wHR at bound" %in% display_columns,
          "Approximate hazard ratio to cross bound.", NA
        ),
        ifelse("Nominal p" %in% display_columns,
          "One-sided p-value for experimental vs control treatment.
          Value < 0.5 favors experimental, > 0.5 favors control.", NA
        ),
        "wAHR is the weighted AHR."
      ),
      location = c(
        ifelse("~wHR at bound" %in% display_columns, "~wHR at bound", NA),
        ifelse("Nominal p" %in% display_columns, "Nominal p", NA),
        NA
      ),
      attr = c(
        ifelse("~wHR at bound" %in% display_columns, "colname", NA),
        ifelse("Nominal p" %in% display_columns, "colname", NA),
        "analysis"
      )
    )
    footnote <- lapply(footnote, function(x) x[!is.na(x)])
  }
  if (method == "combo" && is.null(footnote)) {
    footnote <- list(
      content = c(
        ifelse("Nominal p" %in% display_columns,
          "One-sided p-value for experimental vs control treatment.
               Value < 0.5 favors experimental, > 0.5 favors control.", NA
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
      content = c(ifelse("Nominal p" %in% display_columns,
        "One-sided p-value for experimental vs control treatment.
                         Value < 0.5 favors experimental, > 0.5 favors control.", NA
      )),
      location = c(ifelse("Nominal p" %in% display_columns, "Nominal p", NA)),
      attr = c(ifelse("Nominal p" %in% display_columns, "colname", NA))
    )
    footnote <- lapply(footnote, function(x) x[!is.na(x)])
  }
  # --------------------------------------------- #
  #     filter out inf bound                      #
  # --------------------------------------------- #
  x <- x %>%
    subset(!is.na(`Alternate hypothesis`)) %>%
    subset(!is.na(`Null hypothesis`))

  # --------------------------------------------- #
  #     add spanner                               #
  # --------------------------------------------- #
  names(x)[names(x) == "Alternate hypothesis"] <- colname_spannersub[1]
  names(x)[names(x) == "Null hypothesis"] <- colname_spannersub[2]

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

  # --------------------------------------------- #
  #     add footnotes                             #
  # --------------------------------------------- #
  if (!is.null(footnote$content)) {
    if (length(footnote$content) != 0) {
      for (i in seq_along(footnote$content)) {
        # if the footnotes is added on the colnames
        if (footnote$attr[i] == "colname") {
          x <- x %>%
            gt::tab_footnote(
              footnote = footnote$content[i],
              locations = gt::cells_column_labels(columns = footnote$location[i])
            )
        }
        # if the footnotes is added on the title/subtitle
        if (footnote$attr[i] == "title" || footnote$attr[i] == "subtitle") {
          x <- x %>%
            gt::tab_footnote(
              footnote = footnote$content[i],
              locations = gt::cells_title(group = footnote$attr[i])
            )
        }
        # if the footnotes is added on the analysis summary row, which is a grouping variable, i.e., Analysis
        if (footnote$attr[i] == "analysis") {
          x <- x %>%
            gt::tab_footnote(
              footnote = footnote$content[i],
              locations = gt::cells_row_groups(groups = dplyr::starts_with("Analysis"))
            )
        }
        # if the footnotes is added on the column spanner
        if (footnote$attr[i] == "spanner") {
          x <- x %>%
            gt::tab_footnote(
              footnote = footnote$content[i],
              locations = gt::cells_column_spanners(spanners = colname_spanner)
            )
        }
      }
    }
  }

  ## if it is non-binding design
  if (x_non_binding && (x_alpha < full_alpha)) {
    x <- x %>%
      gt::tab_footnote(
        footnote = paste0(
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
        ),
        locations = gt::cells_body(
          columns = colname_spannersub[2],
          rows = (substring(x_old$Analysis, 1, 11) == paste0("Analysis: ", max(x_k))) &
            (x_old$Bound == display_bound[1])
        )
      )
  }

  return(x)
}
