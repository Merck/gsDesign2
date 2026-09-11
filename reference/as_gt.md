# Convert summary table of a fixed or group sequential design object to a gt object

`as_gt()` is deprecated in favor of
[`lt()`](https://rdrr.io/pkg/lt/man/lt.html), which produces a
lightweight HTML table without the heavy gt dependency. `as_gt()` is
kept for one release so existing code that customizes the output with gt
functions keeps working; it still returns a `gt_tbl` object and requires
gt to be installed. New code should use
[`lt()`](https://rdrr.io/pkg/lt/man/lt.html); see
[lt-methods](https://merck.github.io/gsDesign2/reference/lt-methods.md)
for the available arguments, which mirror those of `as_gt()`.

## Usage

``` r
as_gt(x, ...)

# S3 method for class 'fixed_design_summary'
as_gt(x, title = NULL, footnote = NULL, ...)

# S3 method for class 'gs_design_summary'
as_gt(
  x,
  title = NULL,
  subtitle = NULL,
  colname_spanner = "Cumulative boundary crossing probability",
  colname_spannersub = c("Alternate hypothesis", "Null hypothesis"),
  footnote = NULL,
  display_bound = c("Efficacy", "Futility", "Harm"),
  display_columns = NULL,
  display_inf_bound = FALSE,
  ...
)
```

## Arguments

- x:

  A summary object of a fixed or group sequential design.

- ...:

  Additional arguments (not used).

- title, subtitle, colname_spanner, colname_spannersub, footnote,
  display_bound, display_columns, display_inf_bound:

  See
  [lt-methods](https://merck.github.io/gsDesign2/reference/lt-methods.md)
  for the meaning of these arguments.

## Value

A `gt_tbl` object.

## See also

[`lt()`](https://rdrr.io/pkg/lt/man/lt.html),
[lt-methods](https://merck.github.io/gsDesign2/reference/lt-methods.md)
