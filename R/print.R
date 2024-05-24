# Simple print methods to suppress printing the list element `input` to the
# console

#' @export
print.gs_design <- function(x, ...) {
  x_names <- names(x)
  for (i in seq_along(x)) {
    if (x_names[i] != "input") {
      print(x[i])
    }
  }
}

#' @export
print.fixed_design <- print.gs_design
