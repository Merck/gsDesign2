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

#' One-to-many table merge for presentation
#'
#' A table is desired based on a one-to-many mapping.
#' The data frame in `table_a` maps into `table_b` with the by variable `byvar`
#' Examples show how to use with the gt package for printing a compact combined table.
#'
#' @param table_a A data frame with one record for each value of `byvar`.
#' @param table_b A data frame with one or more records for each value of `byvar`.
#' @param byvar A mapping a one-to-many relation from `table_a` to `table_b`.
#' @param decimals A vector with the number of decimals to be displayed for variables in `table_a`.
#' @param aname The text label for the index `byvar`
#'
#' @return A data frame merging data frames `table_a` and `table_b` with the name from
#'   `aname` and a character string concatenating variables from `table_a` (appropriately rounded).
#'   The columns of `table_b` are also included. This is intended for use with `gt()` grouping by
#'   rows in a.
#'
#' @noRd
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(gt)
#' a <- data.frame(Index = 1:2, a1 = c(1.1234, 5.9876), a2 = c("text 1", "text 2"), a3 = c(3.12, 4.98))
#' b <- data.frame(
#'   Index = c(1, 2, 2),
#'   b1 = c("apple", "table", "penny"),
#'   b2 = 1:3 * (9 / 8),
#'   b3 = (10:8) / 3
#' )
#' table_ab(a, b, byvar = "Index", decimals = c(0, 2, 0, 1), aname = "Index") %>%
#'   group_by(Index) %>%
#'   gt() %>%
#'   fmt_number(b3, decimals = 2) %>%
#'   tab_header(title = "Grouped data table") %>%
#'   tab_footnote(
#'     "The table a variables have been concatenated into a text string, rounded appropriately.",
#'     cells_row_groups(groups = 1)
#'   ) %>%
#'   tab_footnote(
#'     "Note that footnotes cannot be made for individual variables in the row groups generated using table a.",
#'     cells_row_groups(groups = 2)
#'   )
table_ab <- function(table_a, table_b, byvar, decimals = 1, aname = names(table_a)[1]) {
  anames <- names(table_a)
  # Round values in table_a
  table_a <- round_df(table_a, decimals)
  # Unite table_a's names with values
  astring <- apply(as.matrix(table_a), 1, function(row) {
    paste(anames, row, sep = ": ", collapse = " ")
  })
  table_a <- cbind(table_a[byvar], `_alab` = astring)
  # Left-join a with b
  ab <- merge(table_a, table_b, by = byvar, all.x = TRUE, suffixes = c(".a", ""))
  ab <- ab[, c("_alab", names(table_b)), drop = FALSE]
  ab[[byvar]] <- NULL
  # Use the new column name from the argument `aname`
  ab <- replace_names(ab, c(`_alab` = aname))
  return(ab)
}

round_df <- function(x, digits = 2) {
  n1 <- ncol(x); n2 <- length(digits)
  if (n2 != 1 && n2 != n1) {
    warning("'digits' is recycled to the length of ncol(x)")
  }
  digits <- rep(digits, length.out = n1)
  for (i in seq_len(n1)) x[, i] <- round2(x[, i, drop = TRUE], digits[i])
  return(x)
}
