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
# `c(old_value = new_value)`); if `old` is unnamed, apply a function new() to
# the old values
replace_values <- function(x, old, new = identity) {
  if (is.null(names(old))) {
    i <- x %in% old
    x[i] <- new(x[i])
  } else {
    i <- x %in% names(old)
    x[i] <- old[x[i]]
  }
  x
}

# a shorthand based on replace_values() to rename an object
rename_to <- function(x, ...) {
  names(x) <- replace_values(names(x), ...)
  x
}

# round only if input is numeric
round2 <- function(x, ...) {
  if (is.numeric(x)) round(x, ...) else x
}
