# Information and effect size for MaxCombo test

Information and effect size for MaxCombo test

## Usage

``` r
gs_info_combo(
  enroll_rate = define_enroll_rate(duration = c(2, 2, 10), rate = c(3, 6, 9)),
  fail_rate = define_fail_rate(duration = c(3, 100), fail_rate = log(2)/c(9, 18), hr =
    c(0.9, 0.6), dropout_rate = 0.001),
  ratio = 1,
  event = NULL,
  analysis_time = NULL,
  rho,
  gamma,
  tau = rep(-1, length(rho)),
  approx = "asymptotic"
)
```

## Arguments

- enroll_rate:

  An `enroll_rate` data frame with or without stratum created by
  [`define_enroll_rate()`](https://merck.github.io/gsDesign2/reference/define_enroll_rate.md).

- fail_rate:

  A `fail_rate` data frame with or without stratum created by
  [`define_fail_rate()`](https://merck.github.io/gsDesign2/reference/define_fail_rate.md).

- ratio:

  Experimental:Control randomization ratio (not yet implemented).

- event:

  Targeted events at each analysis.

- analysis_time:

  Minimum time of analysis.

- rho:

  Weighting parameters.

- gamma:

  Weighting parameters.

- tau:

  Weighting parameters.

- approx:

  Approximation method.

## Value

A tibble with columns as test index, analysis index, analysis time,
sample size, number of events, ahr, delta, sigma2, theta, and
statistical information.

## Examples

``` r
gs_info_combo(rho = c(0, 0.5), gamma = c(0.5, 0), analysis_time = c(12, 24))
#>   test analysis time         n    event       ahr        delta      sigma2
#> 1    1        1   12  89.99998 20.40451 0.7739222 -0.004130002 0.007603332
#> 2    1        2   24 107.99998 49.06966 0.6744758 -0.020174155 0.026179847
#> 3    2        1   12  89.99998 20.40451 0.8182558 -0.008800844 0.049057930
#> 4    2        2   24 107.99998 49.06966 0.7278445 -0.031421204 0.087095093
#>       theta      info     info0
#> 1 0.5431833 0.6842997 0.6880157
#> 2 0.7705987 2.8274229 2.8855151
#> 3 0.1793970 4.4152129 4.4234382
#> 4 0.3607689 9.4062683 9.4737329
```
