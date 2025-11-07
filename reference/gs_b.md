# Default boundary generation

`gs_b()` is the simplest version of a function to be used with the
`upper` and `lower` arguments in
[`gs_power_npe()`](https://merck.github.io/gsDesign2/reference/gs_power_design_npe.md)
and
[`gs_design_npe()`](https://merck.github.io/gsDesign2/reference/gs_power_design_npe.md)
or the `upper_bound` and `lower_bound` arguments in `gs_prob_combo()`
and `pmvnorm_combo()`. It simply returns the vector of Z-values in the
input vector `par` or, if `k` is specified, `par[k]` is returned. Note
that if bounds need to change with changing information at analyses,
`gs_b()` should not be used. For instance, for spending function bounds
use
[`gs_spending_bound()`](https://merck.github.io/gsDesign2/reference/gs_spending_bound.md).

## Usage

``` r
gs_b(par = NULL, k = NULL, ...)
```

## Arguments

- par:

  For `gs_b()`, this is just Z-values for the boundaries; can include
  infinite values.

- k:

  Is `NULL` (default), return `par`, else return `par[k]`.

- ...:

  Further arguments passed to or from other methods.

## Value

Returns the vector input `par` if `k` is `NULL`, otherwise, `par[k]`.

## Specification

The contents of this section are shown in PDF user manual only.

## Examples

``` r
# Simple: enter a vector of length 3 for bound
gs_b(par = 4:2)
#> [1] 4 3 2

# 2nd element of par
gs_b(par = 4:2, k = 2)
#> [1] 3

# Generate an efficacy bound using a spending function
# Use Lan-DeMets spending approximation of O'Brien-Fleming bound
# as 50%, 75% and 100% of final spending
# Information fraction
IF <- c(.5, .75, 1)
gs_b(par = gsDesign::gsDesign(
  alpha = .025, k = length(IF),
  test.type = 1, sfu = gsDesign::sfLDOF,
  timing = IF
)$upper$bound)
#> [1] 2.962588 2.359018 2.014084
```
