# Simple print methods to suppress printing the list element `input` to the
# console

#' @export
print.gs_design <- function(x, ...) {
  print(x[names(x) != "input"])
  invisible(x)
}

#' @export
print.fixed_design <- print.gs_design
