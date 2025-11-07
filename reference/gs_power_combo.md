# Group sequential design power using MaxCombo test under non-proportional hazards

Group sequential design power using MaxCombo test under non-proportional
hazards

## Usage

``` r
gs_power_combo(
  enroll_rate = define_enroll_rate(duration = 12, rate = 500/12),
  fail_rate = define_fail_rate(duration = c(4, 100), fail_rate = log(2)/15, hr = c(1,
    0.6), dropout_rate = 0.001),
  fh_test = rbind(data.frame(rho = 0, gamma = 0, tau = -1, test = 1, analysis = 1:3,
    analysis_time = c(12, 24, 36)), data.frame(rho = c(0, 0.5), gamma = 0.5, tau = -1,
    test = 2:3, analysis = 3, analysis_time = 36)),
  ratio = 1,
  binding = FALSE,
  upper = gs_b,
  upar = c(3, 2, 1),
  lower = gs_b,
  lpar = c(-1, 0, 1),
  algorithm = mvtnorm::GenzBretz(maxpts = 1e+05, abseps = 1e-05),
  ...
)
```

## Arguments

- enroll_rate:

  Enrollment rates defined by
  [`define_enroll_rate()`](https://merck.github.io/gsDesign2/reference/define_enroll_rate.md).

- fail_rate:

  Failure and dropout rates defined by
  [`define_fail_rate()`](https://merck.github.io/gsDesign2/reference/define_fail_rate.md).

- fh_test:

  A data frame to summarize the test in each analysis. See examples for
  its data structure.

- ratio:

  Experimental:Control randomization ratio.

- binding:

  Indicator of whether futility bound is binding; default of `FALSE` is
  recommended.

- upper:

  Function to compute upper bound.

  - [`gs_spending_bound()`](https://merck.github.io/gsDesign2/reference/gs_spending_bound.md):
    alpha-spending efficacy bounds.

  - [`gs_b()`](https://merck.github.io/gsDesign2/reference/gs_b.md):
    fixed efficacy bounds.

- upar:

  Parameters passed to `upper`.

  - If `upper = gs_b`, then `upar` is a numerical vector specifying the
    fixed efficacy bounds per analysis.

  - If `upper = gs_spending_bound`, then `upar` is a list including

    - `sf` for the spending function family.

    - `total_spend` for total alpha spend.

    - `param` for the parameter of the spending function.

    - `timing` specifies spending time if different from
      information-based spending; see details.

- lower:

  Function to compute lower bound, which can be set up similarly as
  `upper`. See [this
  vignette](https://merck.github.io/gsDesign2/articles/story-seven-test-types.html).

- lpar:

  Parameters passed to `lower`, which can be set up similarly as `upar.`

- algorithm:

  an object of class
  [`GenzBretz`](https://rdrr.io/pkg/mvtnorm/man/algorithms.html),
  [`Miwa`](https://rdrr.io/pkg/mvtnorm/man/algorithms.html) or
  [`TVPACK`](https://rdrr.io/pkg/mvtnorm/man/algorithms.html) specifying
  both the algorithm to be used as well as the associated hyper
  parameters.

- ...:

  Additional parameters passed to
  [mvtnorm::pmvnorm](https://rdrr.io/pkg/mvtnorm/man/pmvnorm.html).

## Value

A list with input parameters, enrollment rate, analysis, and bound.

## Specification

The contents of this section are shown in PDF user manual only.

## Examples

``` r
library(mvtnorm)
library(gsDesign)
library(gsDesign2)

enroll_rate <- define_enroll_rate(
  duration = 12,
  rate = 500 / 12
)

fail_rate <- define_fail_rate(
  duration = c(4, 100),
  fail_rate = log(2) / 15, # median survival 15 month
  hr = c(1, .6),
  dropout_rate = 0.001
)

fh_test <- rbind(
  data.frame(rho = 0, gamma = 0, tau = -1, test = 1, analysis = 1:3, analysis_time = c(12, 24, 36)),
  data.frame(rho = c(0, 0.5), gamma = 0.5, tau = -1, test = 2:3, analysis = 3, analysis_time = 36)
)

# Example 1 ----
# Minimal Information Fraction derived bound
# \donttest{
gs_power_combo(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  fh_test = fh_test,
  upper = gs_spending_combo,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lower = gs_spending_combo,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2)
)
#> $design
#> [1] "combo"
#> 
#> $enroll_rate
#> # A tibble: 1 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All           12  41.7
#> 
#> $fail_rate
#> # A tibble: 2 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 All            4    0.0462        0.001   1  
#> 2 All          100    0.0462        0.001   0.6
#> 
#> $bound
#>   analysis bound  probability probability0         z    nominal p
#> 1        1 upper 6.329275e-08 3.299865e-10  6.175397 3.299865e-10
#> 2        1 lower 3.269613e-04 0.000000e+00 -2.516527 9.940741e-01
#> 3        2 upper 4.260145e-01 2.565830e-03  2.798651 2.565830e-03
#> 4        2 lower 8.468664e-02 0.000000e+00  1.237721 1.079098e-01
#> 5        3 upper 9.015996e-01 2.504519e-02  2.097368 1.798051e-02
#> 6        3 lower 1.999970e-01 0.000000e+00  2.958869 1.543854e-03
#> 
#> $analysis
#>   analysis time        n    event event_frac       ahr
#> 1        1   12 500.0001 107.3943  0.3241690 0.8418858
#> 2        2   24 500.0001 246.2834  0.7434051 0.7164215
#> 3        3   36 500.0001 331.2910  1.0000000 0.6831740
#> 
# }
```
