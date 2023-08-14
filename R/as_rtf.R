as_rtf <- function(x, ...) {
  UseMethod("as_rtf", x)
}

# fixed_design class
as_rtf.fixed_design <- function(x, 
                                title = NULL, 
                                footnote = NULL, 
                                col_rel_width = NULL,
                                orientation = "portrait",
                                text_font_size = 9,
                                path_outtable = NULL,   
                                # Need decimals?
                                ...) {
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
  footnote_mark <- "{^a}"
  if (is.null(title)) {
    title <- switch(design_mtd,
                    "ahr" = {
                      paste0("Fixed Design under AHR Method", " ", footnote_mark)
                    },
                    "fh" = {
                      paste0("Fixed Design under Fleming-Harrington Method", " ", footnote_mark)
                    },
                    "mb" = {
                      paste0("Fixed Design under Magirr-Burman Method", " ", footnote_mark)
                    },
                    "lf" = {
                      paste0("Fixed Design under Lachin and Foulkes Method", " ", footnote_mark)
                    },
                    "rd" = {
                      paste0("Fixed Design of Risk Difference under Farrington-Manning Method", " ", footnote_mark)
                    },
                    "maxcombo" = {
                      paste0("Fixed Design under MaxCombo Method", " ", footnote_mark)
                    },
                    "milestone" = {
                      paste0("Fixed Design under Milestone Method", " ", footnote_mark)
                    },
                    "rmst" = {
                      paste0("Fixed Design under Restricted Mean Survival Time Method", " ", footnote_mark)
                    },
                    "rd" = {
                      paste0("Fixed Design of Risk Difference", " ", footnote_mark)
                    }
    )
  }
  
  
  # set the default footnote
  if (is.null(footnote)) {
    footnote <- switch(design_mtd,
                       "ahr" = {
                         paste0(footnote_mark, " ", "Power computed with average hazard ratio method.")
                       },
                       "fh" = {
                         paste0(
                           footnote_mark, " ", 
                           "Power for Fleming-Harrington test ",
                           substr(x$Design, 19, nchar(x$Design)),
                           " using method of Yung and Liu."
                         )
                       },
                       "mb" = {
                         paste0(
                           footnote_mark, " ", 
                           "Power for ",
                           x$Design,
                           " computed with method of Yung and Liu."
                         )
                       },
                       "lf" = {
                         paste0(
                           footnote_mark, " ", 
                           "Power using Lachin and Foulkes method applied
          using expected average hazard ratio (AHR) at time of planned analysis."
                         )
                       },
                       "rd" = {
                         paste0(
                           footnote_mark, " ", 
                           "Risk difference power without continuity correction using method of Farrington and Manning."
                         )
                       },
                       "maxcombo" = {
                         paste0(
                           footnote_mark, " ", 
                           "Power for MaxCombo test with Fleming-Harrington tests",
                           substr(x$Design, 9, nchar(x$Design)), "."
                         )
                       },
                       "milestone" = {
                         paste0(footnote_mark, " ", "Power for ", x$Design, " computed with method of Yung and Liu.")
                       },
                       "rmst" = {
                         paste0(footnote_mark, " ", "Power for ", x$Design, " computed with method of Yung and Liu.")
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
      ") as as `outdata$tbl` has number of columns (has ",
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
  ans <- x |>
    r2rtf::rtf_page(orientation = orientation) |>
    r2rtf::rtf_title(title) |>
    r2rtf::rtf_colheader(
      colheader = colheader,
      col_rel_width = rel_width,
      text_font_size = text_font_size
    ) |>
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
    ans <- ans |>
      r2rtf::rtf_footnote(footnote,
                          text_font_size = text_font_size
      )
  }
  
  # Prepare output
  ans |>
    rtf_encode() |>
    write_rtf(path_outtable)
}


# gs_design class
as_rtf.gs_design <- function(x,
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
                             orientation = "portrait",
                             text_font_size = 9,
                             path_outtable = NULL,   
                             # Need decimals?
                             ...) {
  
  method <- class(x)[class(x) %in% c("ahr", "wlr", "combo", "rd")]
  x_alpha <- max((x |> dplyr::filter(Bound == display_bound[1]))[[colname_spannersub[2]]])
  x_non_binding <- "non_binding" %in% class(x)
  x_k <- lapply(x$Analysis, function(x) {
    return(as.numeric(substring(x, 11, 11)))
  }) |> unlist()
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
    stop("as_rtf: the variable names in display_columns is not outputted in the summary_bound object!")
  } else {
    x <- x |> dplyr::select(dplyr::all_of(display_columns))
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
  x <- x |>
    subset(!is.na(`Alternate hypothesis`)) |>
    subset(!is.na(`Null hypothesis`))
  
  # organize data
  x <- x |>
    subset(Bound %in% display_bound) |>
    dplyr::arrange(Analysis)
    
  # --------------------------------------------- #
  #     set rtf parameters                        #
  # --------------------------------------------- #
  n_col = ncol(x)
  n_row = nrow(x)
  if (!is.null(col_rel_width) && !(n_col == length(col_rel_width))) {
    stop(
      "col_rel_width must have the same length (has ",
      length(col_rel_width),
      ") as as `outdata$tbl` has number of columns (has ",
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
  border_top_head <- c(rep("", (n_col - 3)), "single", "single")
  border_top_body <- c("single", rep("", n_col - 1))
  border_bottom <- c("single", rep("", n_col - 1))
  border_left_head <- list(
    c("single", ""),
    c("single", rep("", n_col - 2))
  )
  border_left_body <- c("single", border_left_head[[2]])
  
  # Using order number to customize row format
  text_justification <- c("l", "l", rep("c", n_col - 2))
  text_format <- rep("", n_col)
  text_indent <- matrix(0, nrow = n_row, ncol = n_col)
  
  
  # --------------------------------------------- #
  #     add footnotes                             #
  # --------------------------------------------- #
  
  # ----- Need update -----

  # --------------------------------------------- #
  #     output                                    #
  # --------------------------------------------- #
  # use r2rtf
  ans <- x |>
    r2rtf::rtf_page(orientation = orientation) |>
    r2rtf::rtf_title(
      title = title,
      subtitle = subtitle,
      text_convert = FALSE
    ) |>
    r2rtf::rtf_colheader(
      colheader = colheader[1],
      col_rel_width = rel_width_head[[1]],
      text_font_size = text_font_size,
      border_left = border_left_head[[1]]
    ) |>
    r2rtf::rtf_colheader(
      colheader = colheader[2],
      border_top = border_top_head,
      border_left = border_left_head[[2]],
      col_rel_width = rel_width_head[[2]],
      text_font_size = text_font_size
    ) |>
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
  
  # ----- Need update -----
  # if (!is.null(footnote)) {
  #   outdata$rtf <- outdata$rtf |>
  #     r2rtf::rtf_footnote(footnotes,
  #                         text_font_size = text_font_size
  #     )
  # }
  
  # Prepare output
  ans |>
    rtf_encode() |>
    write_rtf(path_outtable)
}


