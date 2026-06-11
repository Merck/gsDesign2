#' Bound summary table
#'
#' Summarizes the efficacy, futility, and harm bounds for each analysis.
#'
#' @param x Design object.
#' @param alpha Vector of alpha values to compute additional efficacy columns.
#' @inheritParams gsDesign::gsBoundSummary
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
#' # Report multiple efficacy bounds (only supported for AHR designs)
#' x <- gs_design_ahr(analysis_time = 1:3*12, alpha = 0.0125)
#' gs_bound_summary(x, alpha = c(0.025, 0.05))
#'
#' @export
gs_bound_summary <- function(x, digits = 4, ddigits = 2, tdigits = 0, timename = "Month", alpha = NULL) {
  if (is.null(alpha)) {
    out <- gs_bound_summary_single(x, digits = digits, ddigits = ddigits,
                                   tdigits = tdigits, timename = timename)
    return(out)
  }
  if (!x$design == "ahr") stop("The argument `alpha` is only supported for AHR design objects")
  if (!is.numeric(alpha)) stop("The argument `alpha` must be a numeric vector")

  # Support multiple alphas
  alpha_original <- x[["input"]][["alpha"]]
  alpha_new <- unique(sort(c(alpha, alpha_original)))
  col_efficacy_name <- paste0("\u03b1=", alpha_new)
  outlist <- vector("list", length = length(alpha_new))
  for (i in seq_along(alpha_new)) {
    if (alpha_new[i] == alpha_original) {
      outlist[[i]] <- gs_bound_summary_single(x, col_efficacy_name[i],
                                              digits = digits, ddigits = ddigits,
                                              tdigits = tdigits, timename = timename)
    } else {
      x_updated <- gs_update_ahr(x, alpha = alpha_new[i])
      x_updated_bounds <- gs_bound_summary_single(x_updated, col_efficacy_name[i],
                                                  digits = digits, ddigits = ddigits,
                                                  tdigits = tdigits, timename = timename)
      outlist[[i]] <- x_updated_bounds[col_efficacy_name[i]]
    }
  }
  out <- Reduce(cbind, outlist)
  # Use of union() allows placement of columns "Futility" and "Harm" at the far
  # right, but only if they are returned by gs_bound_summary_single(). This is
  # because one-sided designs do not produce a Futility column, and designs
  # without harm bounds do not produce a Harm column.
  column_order <- union(c("Analysis", "Value", col_efficacy_name), colnames(out))
  out <- out[, column_order]
  return(out)
}

gs_bound_summary_values <- function(bound, analysis, bound_name, columns) {
  row_bound <- bound[
    bound$analysis == analysis & bound$bound == bound_name,
    columns,
    drop = FALSE
  ]
  if (nrow(row_bound) == 0) return(rep(NA_real_, length(columns)))
  as.numeric(unlist(row_bound[1, columns], use.names = FALSE))
}

gs_bound_summary_single <- function(x, col_efficacy_name = "Efficacy", digits,
                                    ddigits, tdigits, timename) {
  # Input
  analysis <- x$analysis
  bound <- x$bound
  final <- max(analysis$analysis)

  # Output
  col_analysis <- character()
  col_value <- character()
  col_efficacy <- numeric()
  col_futility <- numeric()
  col_harm <- numeric()
  bound_columns <- c("z", "nominal p", "~hr at bound", "probability0", "probability")

  for (i in seq_len(nrow(analysis))) {

    # Analysis column
    label <- paste0("IA ", i, ":")
    info <- analysis$info_frac0[i]
    info <- round(info * 100)
    info <- paste0(info, "%")
    n <- round(analysis$n[i])
    events <- round(analysis$event[i])
    time_value <- analysis$time[i]
    if (!is.null(time_value)) {
      time_value <- round(time_value, tdigits)
      # If the value is an integer, force it to have the correct number of decimal places
      time_value <- format(time_value, nsmall = tdigits)
    }

    col_analysis <- c(
      col_analysis,
      if (i == final) "Final" else paste(label, info),
      paste("N:", n),
      paste("Events:", events),
      if (is.null(time_value)) "" else paste0(timename, ": ", time_value),
      ""
    )

    # Value column
    hr <- analysis[analysis$analysis == i, "ahr"]
    hr <- round(hr, ddigits)
    hr_label <- "HR"
    # logrank test (gs_xxx_ahr): HR -> AHR
    if (x$design == "ahr") hr_label <- "AHR"
    # weighted logrank test (gs_xxx_wlr): HR -> wAHR
    if (x$design == "wlr") hr_label <- "wAHR"
    col_value <- c(
      col_value,
      "Z", "p (1-sided)", "~HR at bound", "P(Cross) if HR=1",
      paste0("P(Cross) if ", hr_label, "=", hr)
    )

    # Efficacy column
    col_efficacy <- c(col_efficacy, gs_bound_summary_values(bound, i, "upper", bound_columns))

    # Futility column
    col_futility <- c(col_futility, gs_bound_summary_values(bound, i, "lower", bound_columns))

    # Harm column
    col_harm <- c(col_harm, gs_bound_summary_values(bound, i, "harm", bound_columns))
  }

  col_efficacy <- round(col_efficacy, digits)
  col_futility <- round(col_futility, digits)
  col_harm <- round(col_harm, digits)

  out <- data.frame(
    Analysis = col_analysis,
    Value = col_value,
    Efficacy = col_efficacy,
    Futility = col_futility,
    Harm = col_harm
  )
  colnames(out)[3] <- col_efficacy_name

  # One-sided design should not include Futility column
  if (all(is.na(out[["Futility"]]))) out[["Futility"]] <- NULL
  if (all(is.na(out[["Harm"]]))) out[["Harm"]] <- NULL

  return(out)
}
