#' Bound summary table
#'
#' Summarizes the efficacy and futility bounds for each analysis.
#'
#' @param x design object
#' @param alpha vector of alpha values to compute additional efficacy columns
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
gs_bound_summary <- function(x, alpha = NULL) {
  if (is.null(alpha)) return(gs_bound_summary_single(x))
  if (!inherits(x, "ahr")) stop("The argument `alpha` is only supported for AHR design objects")
  if (!is.numeric(alpha)) stop("The argument `alpha` must be a numeric vector")

  # Support multiple alphas
  alpha_original <- x[["input"]][["alpha"]]
  alpha_new <- unique(sort(c(alpha, alpha_original)))
  col_efficacy_name <- paste0("\u03b1=", alpha_new)
  outlist <- vector("list", length = length(alpha_new))
  for (i in seq_along(alpha_new)) {
    if (alpha_new[i] == alpha_original) {
      outlist[[i]] <- gs_bound_summary_single(x, col_efficacy_name[i])
    } else {
      x_updated <- gs_update_ahr(x, alpha = alpha_new[i])
      x_updated_bounds <- gs_bound_summary_single(x_updated, col_efficacy_name[i])
      outlist[[i]] <- x_updated_bounds[col_efficacy_name[i]]
    }
  }
  out <- Reduce(cbind, outlist)
  out <- out[, c("Analysis", "Value", col_efficacy_name, "Futility")]
  return(out)
}

gs_bound_summary_single <- function(x, col_efficacy_name = "Efficacy") {
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

  col_efficacy <- round(col_efficacy, 4)
  col_futility <- round(col_futility, 4)

  out <- data.frame(
    Analysis = col_analysis,
    Value = col_value,
    Efficacy = col_efficacy,
    Futility = col_futility
  )
  colnames(out)[3] <- col_efficacy_name
  return(out)
}
