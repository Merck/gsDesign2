# %||% was introduced in base R 4.4.0
if (!exists('%||%', baseenv(), inherits = FALSE)) `%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# append elements from y to x
`%+%` <- function(x, y) {
  # append by names if available, otherwise append by integer indices
  idx <- names(y) %||% seq_along(y)
  for (i in idx) x[[i]] <- c(x[[i]], y[[i]])
  x
}

# add more classes to an object
add_class <- function(x, ...) {
  class(x) <- c(..., class(x))
  x
}

# capitalize initial letters
cap_initial <- function(x) {
  sub("^(.)", "\\U\\1", x, perl = TRUE)
}

# replace elements with values transformed by new()
replace_values <- function(x, old, new = identity) {
  i <- x %in% old
  x[i] <- new(x[i])
  x
}

# round only if input is numeric
round2 <- function(x, ...) {
  if (is.numeric(x)) round(x, ...) else x
}
