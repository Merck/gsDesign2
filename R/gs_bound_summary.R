#' Bound summary table
#'
#' Summarizes the efficacy and futility bounds for each analysis.
#'
#' @param x design object
#'
#' @return A data frame
#'
#' @seealso [gsDesign::gsBoundSummary()]
#'
#' @examples
#' library(gsDesign2)
#'
#' x <- gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
#' gs_bound_summary(x)
#'
#' x <- gs_design_wlr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
#' gs_bound_summary(x)
#'
#' @export
gs_bound_summary <- function(x) {
  # Input
  analysis <- x$analysis
  bound <- x$bound
  final <- max(analysis$analysis)

  # Output
  col_analysis <- character()
  col_value <- character()
  col_efficacy <- numeric()
  col_futility <- numeric()

  for (i in seq_len(nrow(analysis))) {

    # Analysis column
    label <- paste0("IA ", i, ":")
    info <- analysis$info_frac0[i]
    info <- round(info * 100)
    info <- paste0(info, "%")
    n <- round(analysis$n[i])
    events <- round(analysis$event[i])
    month <- analysis$time[i]

    col_analysis <- c(
      col_analysis,
      if (i == final) "Final" else paste(label, info),
      paste("N:", n),
      paste("Events:", events),
      paste("Month:", month),
      ""
    )

    # Value column
    hr <- analysis[analysis$analysis == i, "ahr"]
    hr <- round(hr, 1)
    hr_label <- "HR"
    # logrank test (gs_xxx_ahr): HR -> AHR
    if (inherits(x, "ahr")) hr_label <- "AHR"
    # weighted logrank test (gs_xxx_wlr): HR -> wAHR
    if (inherits(x, "wlr")) hr_label <- "wAHR"
    col_value <- c(
      col_value,
      "Z", "p (1-sided)", "~HR at bound", "P(Cross) if HR=1",
      paste0("P(Cross) if ", hr_label, "=", hr)
    )

    # Efficacy column
    row_efficacy <- bound[
      bound$analysis == i & bound$bound == "upper",
      c("z", "nominal p", "~hr at bound", "probability0", "probability")
    ]
    col_efficacy <- c(col_efficacy, as.numeric(row_efficacy))

    # Futility column
    row_futility <- bound[
      bound$analysis == i & bound$bound == "lower",
      c("z", "nominal p", "~hr at bound", "probability0", "probability")
    ]
    col_futility <- c(col_futility, as.numeric(row_futility))
  }

  col_futility <- round(col_futility, 4)
  col_efficacy <- round(col_efficacy, 4)

  data.frame(
    Analysis = col_analysis,
    Value = col_value,
    Efficacy = col_efficacy,
    Futility = col_futility
  )
}
