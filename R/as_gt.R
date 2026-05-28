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

# get different default columns to display
gsd_columns <- function(columns, method, x) {
  # set different default columns to display
  if (is.null(columns)){
    columns <- c(
      "Analysis", "Bound", "Z", "Nominal p",
      sprintf("%s at bound", switch(method, ahr = "~HR", wlr = "~wHR", rd = "~Risk difference")),
      "Alternate hypothesis", "Null hypothesis")

    if ("Spending time" %in% names(x)) {
      columns <- c(columns[1:3], "Spending time", columns[4:length(columns)])
    }
  }

  # filter the columns to display as the output: if `Probability` is selected to
  # output, transform it to `c("Alternate hypothesis", "Null hypothesis")`
  if (any(i <- columns == "Probability"))
    columns <- c(columns[!i], "Alternate hypothesis", "Null hypothesis")
  ## check if the `display_columns` are included in `x` output
  if (!all(columns %in% names(x))) stop(
    "not all variable names in 'display_columns' are in the summary_bound object!"
  )
  columns
}

# default footnotes for 'gs_design' tables
gsd_footnote <- function(method, columns) {
  n <- c("Nominal p", "~HR at bound", "~wHR at bound")
  i <- n %in% columns
  res <- if (i[1]) list(
    content = paste(
      "One-sided p-value for experimental vs control treatment.",
      "Value < 0.5 favors experimental, > 0.5 favors control."
    ),
    location = n[1], attr = "colname"
  ) else {
    list(content = NULL, location = NULL, attr = NULL)
  }
  x <- "Approximate hazard ratio to cross bound."
  switch(
    method,
    ahr = res %+% if (i[2]) list(x, n[2], "colname"),
    wlr = res %+% (if (i[3]) list(x, n[3], "colname")) %+%
      list("wAHR is the weighted AHR.", NULL, "analysis"),
    combo = res %+% list(
      "EF is event fraction. AHR is under regular weighted log rank test.",
      NULL, "analysis"
    ),
    rd = res
  )
}

# footnote for non-binding designs
gsd_footnote_nb <- function(x, x_alpha) {
  full_alpha <- attr(x, "full_alpha")
  if (attr(x, "binding") || x_alpha >= full_alpha) return()
  a1 <- format(x_alpha, scientific = FALSE)
  a2 <- format(full_alpha, scientific = FALSE)
  a3 <- format(full_alpha - x_alpha, scientific = FALSE)
  paste0(
    "Cumulative alpha for final analysis ",
    "(", a1, ") ", "is less than the full alpha ", "(", a2, ") ",
    "when the futility bound is non-binding. ",
    "The smaller value subtracts the probability of crossing a futility bound ",
    "before crossing an efficacy bound at a later analysis ",
    "(", a2, " - ", a3, " = ", a1, ") ", "under the null hypothesis."
  )
}

# where to add the non-binding design footnote
gsd_footnote_row <- function(x, bound) {
  # for a vector of "Analysis: N", get a logical vector `i`, in which `TRUE`
  # indicates the position of the largest `N`
  a <- x$Analysis
  r <- "^Analysis: ([0-9]+).*"
  i <- grepl(r, a)
  k <- as.numeric(sub(r, '\\1', a[i]))
  i[i] <- k == max(k)
  i & x$Bound == bound
}

# a list of information for `as_[lt|rtf].gs_design()` methods: the transformed
# data, title, and footnote, etc.
gsd_parts <- function(
  x, title, subtitle, spannersub, footnote, bound, columns, inf_bound,
  transform = identity
) {
  method <- attr(x, "design")
  if (!inf_bound) x <- filter(x, !is.infinite(Z))
  # `x` needs a custom transformation in as_rtf()
  x2 <- transform(x)

  columns <- gsd_columns(columns, method, x2)
  x2 <- x2[, columns]
  x2 <- subset(x2, !is.na(`Alternate hypothesis`) & !is.na(`Null hypothesis`))
  x2 <- subset(x2, Bound %in% bound)

  i <- match(c("Alternate hypothesis", "Null hypothesis"), names(x2))
  names(x2)[i] <- spannersub

  title <- title %||% paste("Bound summary", switch(
    method,
    ahr = "for AHR design", wlr = "for WLR design",
    combo = "for MaxCombo design", rd = "of Binary Endpoint"
  ))
  subtitle <- subtitle %||% switch(
    method,
    ahr = "AHR approximations of ~HR at bound",
    wlr = "WLR approximation of ~wHR at bound",
    combo = "MaxCombo approximation",
    rd = "measured by risk difference"
  )

  list(
    x = arrange(x2, Analysis),
    title = title, subtitle = subtitle,
    footnote = if (!isFALSE(footnote)) footnote %||% gsd_footnote(method, columns),
    alpha = max(filter(x, Bound == bound[1])[["Null hypothesis"]])
  )
}
