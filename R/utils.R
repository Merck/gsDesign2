# %||% was introduced in base R 4.4.0
if (!exists('%||%', baseenv(), inherits = FALSE)) `%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

last_ <- function(x) tail(x, 1)

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
  x0; y  # avoid lazy evaluation: evaluate right now
  function(x) {
    i <- findInterval(x, x0, left.open = right)
    y[i + 1]
  }
}

# append 0 to the beginning of a vector and calculate (first-order) diff
diff_one <- function(x) diff(c(0, x))

# append a `first` value to the first `n - 1` values in a vector
fastlag <- function(x, first = 0) c(first, head(x, -1))

# a (hierarchical) hash table in which keys are functions, and values are hash
# tables in which keys are lists of function arguments and values are returned
# values of functions
fun_hash <- hashtab()

cache_fun <- function(fun, ...) {
  # obtain the hash table corresponding to `fun`
  if (is.null(h <- gethash(fun_hash, fun))) {
    h <- hashtab()
    sethash(fun_hash, fun, h)
  }
  prune_hash(h)
  args <- list(...)
  if (is.null(res <- gethash(h, args))) {
    res <- fun(...)
    sethash(h, args, res)
  }
  # we can set this option to TRUE to get a report on the cache size
  if (getOption("gsDesign2.cache.report", FALSE)) message(
    "Current cache size: ", format(object.size(fun_hash), units = "auto")
  )
  res
}

# prune a hash table to prevent it from growing too big to hog memory (by
# default, we use an arbitrary limit of ~8Mb)
prune_hash <- function(h, size = 2^23) {
  n <- object.size(h)
  if (n <= size) return()

  # get all keys
  keys <- list(); i <- 0
  maphash(h, function(k, v) keys[[i <<- i + 1]] <<- k)

  # remove entries until the size is below limit
  for (k in keys) if (n > size) {
    remhash(h, k)
    n <- object.size(h)
  }
}

# Require exact matching by default when retrieving attributes
attr = function(...) base::attr(..., exact = TRUE)
