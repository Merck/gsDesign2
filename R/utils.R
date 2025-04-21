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

# a faster version of stats::stepfun() since we don't need to consider interpolation
stepfun2 <- function(x0, y, right = FALSE) {
  function(x) {
    i <- findInterval(x, x0, left.open = right)
    y[i + 1]
  }
}

#' Find the "previous" values in a vector
#'
#' Fast replacement of \code{dplyr::lag} for the simple case of \code{n = 1L}
#' and always supplying a new value to insert at the beginning of the vector.
#'
#' Important: this function is fast because it provides minimal safety checks.
#' It relies on the
#' \href{https://adv-r.hadley.nz/vectors-chap.html#testing-and-coercion}{coercion
#' rules} of \code{\link[base]{c}}. For best results, \code{x} and \code{first}
#' should be the same type of atomic vector, though it should be fine to mix
#' \code{numeric} and \code{integer} vectors as long as your own code also
#' doesn't rely on this distinction. It can also work on lists if needed.
#'
#' @param x A vector (\code{length(x) > 0})
#' @param first A single value (\code{length(first) == 1})
#'
#' @return a vector that begins with \code{first} and is followed by \code{x}
#' with its final value removed
#'
#' @examples
#' gsDesign2:::fastlag(1:5, first = 100) == c(100, 1:4)
#'
#' @keywords internal
fastlag <- function(x, first = 0) c(first, head(x, -1))
