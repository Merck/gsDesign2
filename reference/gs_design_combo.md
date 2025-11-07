# Group sequential design using MaxCombo test under non-proportional hazards

Group sequential design using MaxCombo test under non-proportional
hazards

## Usage

``` r
gs_design_combo(
  enroll_rate = define_enroll_rate(duration = 12, rate = 500/12),
  fail_rate = define_fail_rate(duration = c(4, 100), fail_rate = log(2)/15, hr = c(1,
    0.6), dropout_rate = 0.001),
  fh_test = rbind(data.frame(rho = 0, gamma = 0, tau = -1, test = 1, analysis = 1:3,
    analysis_time = c(12, 24, 36)), data.frame(rho = c(0, 0.5), gamma = 0.5, tau = -1,
    test = 2:3, analysis = 3, analysis_time = 36)),
  ratio = 1,
  alpha = 0.025,
  beta = 0.2,
  binding = FALSE,
  upper = gs_b,
  upar = c(3, 2, 1),
  lower = gs_b,
  lpar = c(-1, 0, 1),
  algorithm = mvtnorm::GenzBretz(maxpts = 1e+05, abseps = 1e-05),
  n_upper_bound = 1000,
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

- alpha:

  One-sided Type I error.

- beta:

  Type II error.

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

- n_upper_bound:

  A numeric value of upper limit of sample size.

- ...:

  Additional parameters passed to
  [mvtnorm::pmvnorm](https://rdrr.io/pkg/mvtnorm/man/pmvnorm.html).

## Value

A list with input parameters, enrollment rate, analysis, and bound.

## Examples

``` r
# The example is slow to run
library(mvtnorm)
library(gsDesign)

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
  data.frame(
    rho = 0, gamma = 0, tau = -1,
    test = 1, analysis = 1:3, analysis_time = c(12, 24, 36)
  ),
  data.frame(
    rho = c(0, 0.5), gamma = 0.5, tau = -1,
    test = 2:3, analysis = 3, analysis_time = 36
  )
)

x <- gsSurv(
  k = 3,
  test.type = 4,
  alpha = 0.025,
  beta = 0.2,
  astar = 0,
  timing = 1,
  sfu = sfLDOF,
  sfupar = 0,
  sfl = sfLDOF,
  sflpar = 0,
  lambdaC = 0.1,
  hr = 0.6,
  hr0 = 1,
  eta = 0.01,
  gamma = 10,
  R = 12,
  S = NULL,
  T = 36,
  minfup = 24,
  ratio = 1
)

# Example 1 ----
# User-defined boundary
# \donttest{
gs_design_combo(
  enroll_rate,
  fail_rate,
  fh_test,
  alpha = 0.025, beta = 0.2,
  ratio = 1,
  binding = FALSE,
  upar = x$upper$bound,
  lpar = x$lower$bound
)
#> $design
#> [1] "combo"
#> 
#> $enroll_rate
#> # A tibble: 1 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All           12  37.1
#> 
#> $fail_rate
#> # A tibble: 2 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 All            4    0.0462        0.001   1  
#> 2 All          100    0.0462        0.001   0.6
#> 
#> $bounds
#>   analysis bound probability probability0          z    nominal p
#> 1        1 upper 0.002057038 0.0001035057  3.7103029 0.0001035057
#> 2        1 lower 0.140693160 0.4066436377 -0.2361874 0.5933563623
#> 3        2 upper 0.469133797 0.0060406872  2.5114070 0.0060125477
#> 4        2 lower 0.185582585 0.8846152138  1.1703638 0.1209273043
#> 5        3 upper 0.799998180 0.0254957743  1.9929702 0.0231323552
#> 6        3 lower 0.199999706 0.9745044347  1.9929702 0.0231323552
#> 
#> $analysis
#>   analysis time        n     event event_frac       ahr
#> 1        1   12 444.8074  95.53952  0.3241690 0.8418858
#> 2        2   24 444.8074 219.09733  0.7434051 0.7164215
#> 3        3   36 444.8074 294.72130  1.0000000 0.6831740
#> 
# }
# Example 2 ----
# \donttest{
# Boundary derived by spending function
gs_design_combo(
  enroll_rate,
  fail_rate,
  fh_test,
  alpha = 0.025,
  beta = 0.2,
  ratio = 1,
  binding = FALSE,
  upper = gs_spending_combo,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025), # alpha spending
  lower = gs_spending_combo,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2), # beta spending
)
#> $design
#> [1] "combo"
#> 
#> $enroll_rate
#> # A tibble: 1 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All           12  25.1
#> 
#> $fail_rate
#> # A tibble: 2 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 All            4    0.0462        0.001   1  
#> 2 All          100    0.0462        0.001   0.6
#> 
#> $bounds
#>   analysis bound  probability probability0          z    nominal p
#> 1        1 upper 2.088089e-08 3.299865e-10  6.1753973 3.299865e-10
#> 2        1 lower 3.269764e-04 3.303517e-03 -2.7160279 9.966965e-01
#> 3        2 upper 2.203551e-01 2.565830e-03  2.7986508 2.565830e-03
#> 4        2 lower 8.469149e-02 7.432155e-01  0.6532878 2.567854e-01
#> 5        3 upper 8.000000e-01 2.372246e-02  2.0973680 1.798051e-02
#> 6        3 lower 2.000159e-01 9.762994e-01  2.0974331 1.797763e-02
#> 
#> $analysis
#>   analysis time        n     event event_frac       ahr
#> 1        1   12 301.2814  64.71178  0.3241690 0.8418858
#> 2        2   24 301.2814 148.40119  0.7434051 0.7164215
#> 3        3   36 301.2814 199.62357  1.0000000 0.6831740
#> 
# }
```
