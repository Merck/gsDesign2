#  Copyright (c) 2021 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co., Inc., Kenilworth, NJ, USA.
#
#  This file is part of the gsdmvn program.
#
#  gsdmvn is free software: you can redistribute it and/or modify
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

#' S3 class method to get summary table into a gt table
#'
#' @param x a summary of fixed or group sequential design
#' @param ... additional arguments
#'
#' @return a gt table
#' @export
#'
as_gt <- function(x, ...) {
  UseMethod("as_gt", x)
}

#' This is the function to format the bounds summary table of fixed design into gt style.
#' @rdname as_gt.fixed_design
#' 
#' @param x a summary object of group sequential design
#' @param title title to be displayed
#' @param footnote footnotes to be displayed
#' @param ... additional arguments
#' 
#' @return a gt table
#' 
#' @export as_gt
#' @exportS3Method
#'  
#' @method as_gt fixed_design
#' 
#' @examples
#' library(dplyr)
#' library(tibble)
#' 
#' # Enrollment rate
#' enrollRates <- tibble(
#'   Stratum = "All", 
#'   duration = 18, 
#'   rate = 20)
#' 
#' # Failure rates
#' failRates <- tibble(
#'   Stratum = "All", 
#'   duration = c(4, 100), 
#'   failRate = log(2) / 12,
#'   hr = c(1, .6), 
#'   dropoutRate = .001)
#' 
#' # Study duration in months
#' studyDuration <- 36
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
#'   x = "AHR", 
#'   alpha = alpha, power = 1 - beta, 
#'   enrollRates = enrollRates, failRates = failRates, 
#'   studyDuration = studyDuration, ratio = ratio
#'   ) %>% 
#'   summary() %>% 
#'   as_gt()
#'   
#' # ------------------------- #
#' #        FH                 #
#' # ------------------------- #
#' # under fixed power
#' fixed_design(
#'   x = "FH", 
#'   alpha = alpha, power = 1 - beta, 
#'   enrollRates = enrollRates, failRates = failRates, 
#'   studyDuration = studyDuration, ratio = ratio
#'   ) %>% 
#'   summary() %>% 
#'   as_gt()
#'   
as_gt.fixed_design <- function(x, title = NULL, footnote = NULL, ...){
  # get the design method 
  if("AHR" %in% class(x)){
    design_mtd <- "AHR"
  }else if("FH" %in% class(x)){
    design_mtd <- "FH"
  }else if("MB" %in% class(x)){
    design_mtd <- "MB"
  }else if("LF" %in% class(x)){
    design_mtd <- "LF"
  }else if("RD" %in% class(x)){
    design_mtd <- "RD"
  }else if("MaxCombo" %in% class(x)){
    design_mtd <- "MaxCombo"
  }else if("Milestone" %in% class(x)){
    design_mtd <- "Milestone"
  }else if("RMST" %in% class(x)){
    design_mtd <- "RMST"
  }else if("RD" %in% class(x)){
    design_mtd <- "RD"
  }
  
  
  # set the default title
  if(is.null(title)){
    title <- switch (design_mtd,
                     "AHR" = {"Fixed Design under AHR Method"},
                     "FH" = {"Fixed Design under Fleming-Harrington Method"},
                     "MB" = {"Fixed Design under Magirr-Burman Method"},
                     "LF" = {"Fixed Design under Lachin and Foulkes Method"},
                     "RD" = {"Fixed Design of Risk Difference under Farrington-Manning Method"},
                     "MaxCombo" = {"Fixed Design under Max Combo Method"},
                     "Milestone" = {"Fixed Design under Milestone Method"},
                     "RMST" = {"Fixed Design under Restricted Mean Survival Time Method"},
                     "RD" = {"Fixed Design of Risk Difference"}
    )
  }
  
  
  # set the default footnote
  if(is.null(footnote)){
    footnote <- switch (design_mtd,
                        "AHR" = {"Power computed with average hazard ratio method."},
                        "FH" = {paste0("Power for Fleming-Harrington test ",
                                       substr(x$Design, 19, nchar(x$Design)),
                                       " using method of Yung and Liu.")},
                        "MB" = {paste0("Power for ",
                                       x$Design,
                                       " computed with method of Yung and Liu.")},
                        "LF" = {"Power using Lachin and Foulkes method applied using expected average hazard ratio (AHR) at time of planned analysis."},
                        "RD" = {"Risk difference power without continuity correction using method of Farrington and Manning."},
                        "MaxCombo" = {paste0("Power for MaxCombo test with Fleming-Harrington tests",
                                             substr(x$Design, 9, nchar(x$Design)), "."
                                             # paste(apply(do.call(rbind, x$design_par), 2 , paste , collapse = "," ), collapse = ") and ("),
                        )},
                        "Milestone" = {paste0("Power for ", x$Design, " computed with method of Yung and Liu.")},
                        "RMST" = {paste0("Power for ", x$Design, " computed with method of Yung and Liu.")}
    )  
  }
  
  ans <- x %>% 
    mutate(Design = design_mtd) %>% 
    gt::gt() %>% 
    gt::tab_header(title = title) %>%     
    gt::tab_footnote(footnote = footnote, locations = gt::cells_title(group = "title"))
  
  return(ans)
}


#' This is the function to format the bounds summary table into gt style.
#' @rdname as_gt.gs_design
#' 
#' @param x an object returned by \code{summary_bound}
#' @param title a string to specify the title of the gt table
#' @param subtitle a string to specify the subtitle of the gt table
#' @param colname_spanner a string to specify the spanner of the gt table
#' @param colname_spannersub a vector of strings to specify the spanner details of the gt table
#' @param footnote a list containing \code{content}, \code{location}, and \code{attr}. 
#'                 the \code{content} is a vector of string to specify the footnote text;
#'                 the \code{location} is a vector of string to specify the locations to put the superscript of the footnote index;
#'                 the \code{attr} is a vector of string to specify the attributes of the footnotes, e.g., c("colname", "title", "subtitle", "analysis", "spanner");
#'                 users can use the functions in the \code{gt} package to custom themselves.
#' @param display_bound a vector of strings specifying the label of the bounds. The default is \code{c("Efficacy", "Futility")}
#' @param display_columns a vector of strings specifying the variables to be displayed in the summary table
#' @param display_inf_bound a logic value (TRUE or FALSE) whether to display the +-inf bound
#' @param ... additional arguments
#' 
#' @return a gt table summarizing the bounds table in group sequential designs
#' 
#' @export as_gt
#' @exportS3Method 
#' 
#' @method as_gt gs_design
#' @examples 
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
#' \dontrun{
#' gs_design_combo() %>% 
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
#' } 
#' # usage of title = ..., subtitle = ...
#' # to edit the title/subtitle 
#' gs_power_wlr() %>% 
#'   summary() %>%
#'   as_gt(
#'     title = "Bound Summary",
#'     subtitle = "from gs_power_wlr")
#'
#'# usage of colname_spanner = ..., colname_spannersub = ...
#'# to edit the spanner and its sub-spanner
#' gs_power_wlr() %>% 
#'   summary() %>%
#'   as_gt(
#'     colname_spanner = "Cumulative probability to cross boundaries",
#'     colname_spannersub = c("under H1", "under H0"))
#'     
#'# usage of footnote = ...
#'# to edit the footnote
#' gs_power_wlr() %>% 
#'   summary() %>%
#'   as_gt(
#'     footnote = list(content = c("approximate weighted hazard ratio to cross bound.", 
#'                                 "wAHR is the weighted AHR.",
#'                                 "the crossing probability.",
#'                                 "this table is generated by gs_power_wlr."),
#'                    location = c("~wHR at bound", NA, NA, NA),
#'                    attr = c("colname", "analysis", "spanner", "title")))
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
  display_inf_bound = TRUE,
  ...
){
  method <- class(x)[class(x) %in% c("ahr", "wlr", "combo", "rd")]
  
  
  # --------------------------------------------- #
  #     set defaults                              #
  # --------------------------------------------- #
  # set different default title to different methods
  if(method == "ahr" && is.null(title)){
    title <- "Bound summary for AHR design"
  }
  if(method == "wlr" && is.null(title)){
    title <- "Bound summary for WLR design"
  }
  if(method == "combo" && is.null(title)){
    title <- "Bound summary for Max Combo design"
  }
  
  if(method == "rd" && is.null(title)){
    title <- "Bound summary of Binary Endpoint"
  }
  
  # set different default subtitle to different methods
  if(method == "ahr" && is.null(subtitle)){
    subtitle <- "AHR approximations of ~HR at bound"
  }
  if(method == "wlr" && is.null(subtitle)){
    subtitle <- "WLR approximation of ~wHR at bound"
  }
  if(method == "combo" && is.null(subtitle)){
    subtitle <- "Max Combo approximation"
  }
  if(method == "rd" && is.null(subtitle)){
    subtitle <- "measured by risk difference"
  }
  
  # set different default columns to display
  if(is.null(display_columns)){
    if(method == "ahr"){
      display_columns <- c("Analysis", "Bound", "Nominal p", "~HR at bound", "Alternate hypothesis", "Null hypothesis")
    }else if(method == "wlr"){
      display_columns <- c("Analysis", "Bound", "Nominal p", "~wHR at bound", "Alternate hypothesis", "Null hypothesis")
    }else if(method == "combo"){
      display_columns <- c("Analysis", "Bound", "Nominal p", "Alternate hypothesis", "Null hypothesis")
    }else if(method == "rd"){
      display_columns <- c("Analysis", "Bound", "Nominal p", "~Risk difference at bound", "Alternate hypothesis", "Null hypothesis")
    }
  }
  # filter the columns to display as the output
  ## if `Probability` is selected to output, then transform it to `c("Alternate hypothesis", "Null hypothesis")`
  if("Probability" %in% display_columns){
    display_columns <- display_columns[!display_columns == "Probability"]
    display_columns <- c(display_columns, "Alternate hypothesis", "Null hypothesis")
  }
  ## check if the `display_columns` are included in `x` output
  if(sum(!(display_columns %in% names(x))) >= 1){
    stop("as_gt: the variable names in display_columns is not outputted in the summary_bound object!")
  }else{
    x <- x %>% dplyr::select(all_of(display_columns))
  }
  
  # set different default footnotes to different methods
  if(method == "ahr" && is.null(footnote)){
    footnote <- list(content = c(ifelse("~HR at bound" %in% display_columns, "Approximate hazard ratio to cross bound.", NA),
                                 ifelse("Nominal p" %in% display_columns, "One-sided p-value for experimental vs control treatment. Values < 0.5 favor experimental, > 0.5 favor control.", NA)),
                     location = c(ifelse("~HR at bound" %in% display_columns, "~HR at bound", NA),
                                  ifelse("Nominal p" %in% display_columns, "Nominal p", NA)),
                     attr = c(ifelse("~HR at bound" %in% display_columns, "colname", NA),
                              ifelse("Nominal p" %in% display_columns, "colname", NA)))
    footnote <- lapply(footnote, function(x) x[!is.na(x)])
  }
  if(method == "wlr" && is.null(footnote)){
    footnote <- list(content = c(ifelse("~wHR at bound" %in% display_columns, "Approximate hazard ratio to cross bound.", NA),
                                 ifelse("Nominal p" %in% display_columns, "One-sided p-value for experimental vs control treatment. Values < 0.5 favor experimental, > 0.5 favor control.", NA),
                                 "wAHR is the weighted AHR."),
                     location = c(ifelse("~wHR at bound" %in% display_columns, "~wHR at bound", NA),
                                  ifelse("Nominal p" %in% display_columns, "Nominal p", NA),
                                  NA),
                     attr = c(ifelse("~wHR at bound" %in% display_columns, "colname", NA),
                              ifelse("Nominal p" %in% display_columns, "colname", NA),
                              "analysis"))
    footnote <- lapply(footnote, function(x) x[!is.na(x)])
  }
  if(method == "combo" && is.null(footnote)){
    
    footnote <- list(content = c(ifelse("Nominal p" %in% display_columns, "One-sided p-value for experimental vs control treatment. Values < 0.5 favor experimental, > 0.5 favor control.", NA),
                                 "EF is event fraction. AHR  is under regular weighted log rank test."),
                     location = c(ifelse("Nominal p" %in% display_columns, "Nominal p", NA),
                                  NA),
                     attr = c(ifelse("Nominal p" %in% display_columns, "colname", NA),
                              "analysis"))
    footnote <- lapply(footnote, function(x) x[!is.na(x)])
  }
  if(method == "rd" && is.null(footnote)){
    
    footnote <- list(content = c(ifelse("Nominal p" %in% display_columns, "One-sided p-value for experimental vs control treatment. Values < 0.5 favor experimental, > 0.5 favor control.", NA)),
                     location = c(ifelse("Nominal p" %in% display_columns, "Nominal p", NA)),
                     attr = c(ifelse("Nominal p" %in% display_columns, "colname", NA)))
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
      columns = all_of(colname_spannersub),
      label = colname_spanner) %>% 
    gt::tab_header(title = title, subtitle = subtitle)
  
  # --------------------------------------------- #
  #     add footnotes                             #
  # --------------------------------------------- # 
  if(!is.null(footnote$content)){
    if(length(footnote$content) != 0){
      for (i in 1:length(footnote$content)) {
        # if the footnotes is added on the colnames
        if(footnote$attr[i] == "colname"){
          x <- x %>% 
            gt::tab_footnote(
              footnote = footnote$content[i],
              locations = gt::cells_column_labels(columns = footnote$location[i]))
        }
        # if the footnotes is added on the title/subtitle
        if(footnote$attr[i] == "title" || footnote$attr[i] == "subtitle"){
          x <- x %>% 
            gt::tab_footnote(
              footnote = footnote$content[i],
              locations = gt::cells_title(group = footnote$attr[i]))
        }
        # if the footnotes is added on the analysis summary row, which is a grouping variable, i.e., Analysis
        if(footnote$attr[i] == "analysis"){
          x <- x %>% 
            gt::tab_footnote(
              footnote = footnote$content[i],
              locations = gt::cells_row_groups(groups = dplyr::starts_with("Analysis")))
        }
        # if the footnotes is added on the column spanner
        if(footnote$attr[i] == "spanner"){
          x <- x %>% 
            gt::tab_footnote(
              footnote = footnote$content[i],
              locations = gt::cells_column_spanners(spanners = colname_spanner)
            )
        }
      }
    }
  }
  return(x)
}