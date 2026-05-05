# Derive spending bound for group sequential boundary

Computes one bound at a time based on spending under given
distributional assumptions. While user specifies `gs_spending_bound()`
for use with other functions, it is not intended for use on its own.
Most important user specifications are made through a list provided to
functions using `gs_spending_bound()`. Function uses numerical
integration and Newton-Raphson iteration to derive an individual bound
for a group sequential design that satisfies a targeted boundary
crossing probability. Algorithm is a simple extension of that in Chapter
19 of Jennison and Turnbull (2000).

## Usage

``` r
gs_spending_bound(
  k = 1,
  par = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL,
    max_info = NULL),
  hgm1 = NULL,
  theta = 0.1,
  info = 1:3,
  efficacy = TRUE,
  test_bound = TRUE,
  r = 18,
  tol = 1e-06
)
```

## Arguments

- k:

  Analysis for which bound is to be computed.

- par:

  A list with the following items:

  - `sf` (class spending function).

  - `total_spend` (total spend).

  - `param` (any parameters needed by the spending function `sf()`).

  - `timing` (a vector containing values at which spending function is
    to be evaluated or `NULL` if information-based spending is used).

  - `max_info` (when `timing` is `NULL`, this can be input as positive
    number to be used with `info` for information fraction at each
    analysis).

- hgm1:

  Subdensity grid from `h1()` (k=2) or `hupdate()` (k\>2) for analysis
  k-1; if k=1, this is not used and may be `NULL`.

- theta:

  Natural parameter used for lower bound only spending; represents
  average drift at each time of analysis at least up to analysis k;
  upper bound spending is always set under null hypothesis (theta = 0).

- info:

  Statistical information at all analyses, at least up to analysis k.

- efficacy:

  `TRUE` (default) for efficacy bound, `FALSE` otherwise.

- test_bound:

  A logical vector of the same length as `info` should indicate which
  analyses will have a bound.

- r:

  Integer value controlling grid for numerical integration as in
  Jennison and Turnbull (2000); default is 18, range is 1 to 80. Larger
  values provide larger number of grid points and greater accuracy.
  Normally `r` will not be changed by the user.

- tol:

  Tolerance parameter for convergence (on Z-scale).

## Value

Returns a numeric bound (possibly infinite) or, upon failure, generates
an error message.

## Specification

The contents of this section are shown in PDF user manual only.

## References

Jennison C and Turnbull BW (2000), *Group Sequential Methods with
Applications to Clinical Trials*. Boca Raton: Chapman and Hall.

## Author

Keaven Anderson <keaven_anderson@merck.com>

## Examples

``` r
gs_power_ahr(
  analysis_time = c(12, 24, 36),
  event = c(30, 40, 50),
  binding = TRUE,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
)
#> $design
#> [1] "ahr"
#> 
#> $enroll_rate
#> # A tibble: 3 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All            2     3
#> 2 All            2     6
#> 3 All           10     9
#> 
#> $fail_rate
#> # A tibble: 2 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 All            3    0.0770        0.001   0.9
#> 2 All          100    0.0385        0.001   0.6
#> 
#> $bound
#>   analysis bound  probability probability0          z ~hr at bound    nominal p
#> 1        1 upper 0.0070632195 0.0008667172  3.1324678    0.3186015 0.0008667172
#> 2        1 lower 0.0009350778 0.0065831967 -2.4792366    2.4726440 0.9934168033
#> 3        2 upper 0.1145732865 0.0092093035  2.3687212    0.5084965 0.0089248527
#> 4        2 lower 0.0091236881 0.1128819225 -1.2137128    1.4141503 0.8875713014
#> 5        3 upper 0.3237353751 0.0250000000  2.0108828    0.6100896 0.0221689193
#> 6        3 lower 0.0250643871 0.3231894108 -0.4743371    1.1236277 0.6823702128
#> 
#> $analysis
#>   analysis     time   n    event       ahr     theta      info     info0
#> 1        1 14.90817 108 30.00008 0.7865726 0.2400702  7.373433  7.500019
#> 2        2 24.00000 108 49.06966 0.7151566 0.3352538 11.999266 12.267415
#> 3        3 36.00000 108 66.23948 0.6833395 0.3807634 16.267921 16.559870
#>   info_frac info_frac0
#> 1 0.4532499  0.4529033
#> 2 0.7376029  0.7407917
#> 3 1.0000000  1.0000000
#> 
```
