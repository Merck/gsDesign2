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

# replace elements with values from a named vector `old` (of the form
# `c(old_value = new_value)`)
replace_values <- function(x, map) {
  i <- x %in% names(map)
  x[i] <- map[x[i]]
  x
}

# a shorthand based on replace_values() to rename an object
replace_names <- function(x, ...) {
  names(x) <- replace_values(names(x), ...)
  x
}

# round only if input is numeric and digits is provided
round2 <- function(x, digits, ...) {
  if (is.numeric(x) && !is.na(digits)) round(x, digits, ...) else x
}

# test if it is whole number
is_wholenumber <- function (x, tol = .Machine$double.eps^0.5)  {
  abs(x - round(x)) < tol
}

