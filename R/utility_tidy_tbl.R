#' One-to-many table merge for presentation
#' @description
#' A table is desired based on a one-to-many mapping.
#' The data frame in `table_a` maps into `table_b` with the by variable `by_a`
#' Examples show how to use with the *gt* package for printing a compact combined table.
#'
#' @param table_a A data frame with one record for each value of `byvar`.
#' @param table_b A data frame with one or more records for each value of `byvar`.
#' @param byvar A mapping a one-to-many relation from `table_a` to `table_b`.
#' @param decimals A vector with the number of decimals to be displayed for variables in `table_a`.
#' @param aname The text label for the index `byvar`
#'
#' @return A data frame merging data frames `table_a` and `table_b` with the name from
#' `aname` and a character string concatenating variables from `table_a` (appropriately rounded).
#' The columns of `table_b` are also included. This is intended for use with gt() grouping by
#' rows in a.
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(gt)
#' a <- data.frame(Index = 1:2, a1 = c(1.1234, 5.9876), a2 = c("text 1", "text 2"), a3 = c(3.12, 4.98) )
#' b <- data.frame(Index = c(1, 2, 2),
#'                 b1 = c("apple", "table", "penny"),
#'                 b2 = 1:3 * (9 / 8),
#'                 b3 = (10:8) / 3
#'                )
#' table_ab(a, b, byvar = "Index", decimals = c(0, 2, 0, 1), aname = "Index") %>%
#'   group_by(Index) %>% gt() %>% fmt_number(b3, decimals = 2) %>%
#'   tab_header(title = "Grouped data table") %>%
#'   tab_footnote("The table a variables have been concatenated into a text string, rounded appropriately.",
#'                cells_row_groups(groups = 1)) %>%
#'   tab_footnote("Note that footnotes cannot be made for individual variables in the row groups generated using table a.",
#'                cells_row_groups(groups = 2))
#'                
#' @noRd            
table_ab <- function(table_a, table_b, byvar, decimals = 1, aname = names(table_a)[1]){
  # Convert tibbles to data frames, if needed
  if(tibble::is_tibble(table_a)) table_a <- data.frame(table_a, check.names = FALSE)
  if(tibble::is_tibble(table_b)) table_b <- data.frame(table_b, check.names = FALSE)
  # Round values in table_a
  table_a <- table_a %>% rounddf(digits = decimals)
  # Put names from table_a in a data frame
  anames <- data.frame(t(paste0(names(table_a), ":")))
  # Bind columns from these 2 data frames together
  xx <- cbind(table_a, anames)
  # Get order of names to unite table_a columns together with names into a string
  col_order <- c(rbind(names(anames), names(table_a)))
  # Now unite columns of table_a into a string
  astring <- xx %>% tidyr::unite("_alab", col_order, sep = " ")
  # Bind this together with the byvar column
  astring <- cbind(table_a %>% select(all_of(byvar)), astring)
  # Now merge with table_b
  ab <- left_join(astring, table_b, by = byvar) %>%
    select(-one_of(!!byvar)) %>%
    # select(-!!byvar) %>%
    dplyr::rename(!!aname := !!"_alab")
  return(ab)
  # return(ab %>% group_by(!!aname)) # This grouping is not working
}

#' From https://github.com/sashahafner/jumbled/blob/master/rounddf.R
#' @noRd 
rounddf <- function(x, digits = rep(2, ncol(x)), func = round) {
  if (length(digits) == 1) {
    digits <- rep(digits, ncol(x))
  } else if (length(digits) != ncol(x)) {
    digits <- c(digits, rep(digits[1], ncol(x) - length(digits)))
    warning('First value in digits repeated to match length.')
  }
  
  for(i in 1:ncol(x)) {
    if(class(x[, i, drop = TRUE])[1] == 'numeric') x[, i] <- func(x[, i], digits[i])
  }
  
  return(x)
}