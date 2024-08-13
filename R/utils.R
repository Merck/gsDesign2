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
