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
#> # A tibble: 6 × 7
#>   analysis bound probability probability0      z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl>  <dbl>          <dbl>       <dbl>
#> 1        1 upper    0.00706      0.000867  3.13           0.319    0.000867
#> 2        1 lower    0.000935     0.00658  -2.48           2.47     0.993   
#> 3        2 upper    0.115        0.00921   2.37           0.508    0.00892 
#> 4        2 lower    0.00912      0.113    -1.21           1.41     0.888   
#> 5        3 upper    0.324        0.0250    2.01           0.610    0.0222  
#> 6        3 lower    0.0251       0.323    -0.474          1.12     0.682   
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
