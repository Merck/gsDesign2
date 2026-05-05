# Group sequential design using weighted log-rank test under non-proportional hazards

Group sequential design using weighted log-rank test under
non-proportional hazards

## Usage

``` r
gs_design_wlr(
  enroll_rate = define_enroll_rate(duration = c(2, 2, 10), rate = c(3, 6, 9)),
  fail_rate = tibble(stratum = "All", duration = c(3, 100), fail_rate = log(2)/c(9, 18),
    hr = c(0.9, 0.6), dropout_rate = rep(0.001, 2)),
  weight = "logrank",
  approx = "asymptotic",
  alpha = 0.025,
  beta = 0.1,
  ratio = 1,
  info_frac = NULL,
  info_scale = c("h0_h1_info", "h0_info", "h1_info"),
  analysis_time = 36,
  binding = FALSE,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = alpha),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = beta),
  test_upper = TRUE,
  test_lower = TRUE,
  h1_spending = TRUE,
  r = 18,
  tol = 1e-06,
  interval = c(0.01, 1000)
)
```

## Arguments

- enroll_rate:

  An `enroll_rate` data frame with or without stratum created by
  [`define_enroll_rate()`](https://merck.github.io/gsDesign2/reference/define_enroll_rate.md).

- fail_rate:

  A `fail_rate` data frame with or without stratum created by
  [`define_fail_rate()`](https://merck.github.io/gsDesign2/reference/define_fail_rate.md).

- weight:

  Weight of weighted log rank test:

  - `"logrank"` = regular logrank test.

  - `list(method = "fh", param = list(rho = ..., gamma = ...))` =
    Fleming-Harrington weighting functions.

  - `list(method = "mb", param = list(tau = ..., w_max = ...))` = Magirr
    and Burman weighting functions.

- approx:

  Approximate estimation method for Z statistics.

  - `"event_driven"` = only work under proportional hazard model with
    log rank test.

  - `"asymptotic"`.

- alpha:

  One-sided Type I error.

- beta:

  Type II error.

- ratio:

  Experimental:Control randomization ratio.

- info_frac:

  Targeted information fraction for analyses. See details.

- info_scale:

  Information scale for calculation. Options are:

  - `"h0_h1_info"` (default): variance under both null and alternative
    hypotheses is used.

  - `"h0_info"`: variance under null hypothesis is used. This is often
    used for testing methods that use local alternatives, such as the
    Schoenfeld method.

  - `"h1_info"`: variance under alternative hypothesis is used.

- analysis_time:

  Targeted calendar timing of analyses. See details.

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

- test_upper:

  Indicator of which analyses should include an upper (efficacy) bound;
  single value of `TRUE` (default) indicates all analyses; otherwise, a
  logical vector of the same length as `info` should indicate which
  analyses will have an efficacy bound.

- test_lower:

  Indicator of which analyses should include a lower bound; single value
  of `TRUE` (default) indicates all analyses; single value of `FALSE`
  indicated no lower bound; otherwise, a logical vector of the same
  length as `info` should indicate which analyses will have a lower
  bound.

- h1_spending:

  Indicator that lower bound to be set by spending under alternate
  hypothesis (input `fail_rate`) if spending is used for lower bound. If
  this is `FALSE`, then the lower bound spending is under the null
  hypothesis. This is for two-sided symmetric or asymmetric testing
  under the null hypothesis; See [this
  vignette](https://merck.github.io/gsDesign2/articles/story-seven-test-types.html).

- r:

  Integer value controlling grid for numerical integration as in
  Jennison and Turnbull (2000); default is 18, range is 1 to 80. Larger
  values provide larger number of grid points and greater accuracy.
  Normally, `r` will not be changed by the user.

- tol:

  Tolerance parameter for boundary convergence (on Z-scale); normally
  not changed by the user.

- interval:

  An interval presumed to include the times at which expected event
  count is equal to targeted event. Normally, this can be ignored by the
  user as it is set to `c(.01, 1000)`.

## Value

A list with input parameters, enrollment rate, analysis, and bound.

## Specification

The contents of this section are shown in PDF user manual only.

## Examples

``` r
library(mvtnorm)
library(gsDesign)
library(gsDesign2)

# set enrollment rates
enroll_rate <- define_enroll_rate(duration = 12, rate = 1)

# set failure rates
fail_rate <- define_fail_rate(
  duration = c(4, 100),
  fail_rate = log(2) / 15, # median survival 15 month
  hr = c(1, .6),
  dropout_rate = 0.001
)

# Example 1 ----
# Information fraction driven design
gs_design_wlr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  ratio = 1,
  alpha = 0.025, beta = 0.2,
  weight = list(method = "mb", param = list(tau = Inf, w_max = 2)),
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2),
  analysis_time = 36,
  info_frac = c(0.6, 1)
)
#> $design
#> [1] "wlr"
#> 
#> $enroll_rate
#> # A tibble: 1 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All           12  22.3
#> 
#> $fail_rate
#> # A tibble: 2 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 All            4    0.0462        0.001   1  
#> 2 All          100    0.0462        0.001   0.6
#> 
#> $bounds
#>   analysis bound probability probability0        z ~hr at bound   nominal p
#> 1        1 upper  0.64849767  0.003808068 2.668630    0.6286465 0.003808068
#> 2        1 lower  0.09755484  0.959449751 1.744336    0.7382942 0.040550249
#> 3        2 upper  0.79999961  0.014100210 1.980967    0.7424480 0.023797474
#> 4        2 lower  0.19672399  0.984554626 1.911120    0.7502850 0.027994609
#> 
#> $analysis
#>   analysis     time        n    event       ahr     theta      info     info0
#> 1        1 24.13302 267.1275 132.2063 0.6900748 0.3709552  66.32949  67.78105
#> 2        2 36.00000 267.1275 176.9938 0.6571048 0.2593958 108.73427 112.96839
#>   info_frac info_frac0
#> 1 0.6100146  0.6000002
#> 2 1.0000000  1.0000000
#> 

# Example 2 ----
# Calendar time driven design
gs_design_wlr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  ratio = 1,
  alpha = 0.025, beta = 0.2,
  weight = list(method = "mb", param = list(tau = Inf, w_max = 2)),
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2),
  analysis_time = c(24, 36),
  info_frac = NULL
)
#> $design
#> [1] "wlr"
#> 
#> $enroll_rate
#> # A tibble: 1 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All           12  25.0
#> 
#> $fail_rate
#> # A tibble: 2 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 All            4    0.0462        0.001   1  
#> 2 All          100    0.0462        0.001   0.6
#> 
#> $bounds
#>   analysis bound probability probability0         z ~hr at bound   nominal p
#> 1        1 upper  0.32626250  0.003661778 2.6817619    0.6433691 0.003661778
#> 2        1 lower  0.09772849  0.820715674 0.9180959    0.8598580 0.179284326
#> 3        2 upper  0.79999972  0.022769009 1.9801832    0.7551913 0.023841476
#> 4        2 lower  0.19829993  0.974937472 1.9309710    0.7604795 0.026743319
#> 
#> $analysis
#>   analysis time        n    event       ahr     theta      info     info0
#> 1        1   24 300.2519 147.8941 0.6902269 0.2561850  73.94117  75.54355
#> 2        2   36 300.2519 198.9415 0.6571048 0.2593958 122.21758 126.97674
#>   info_frac info_frac0
#> 1 0.6049962    0.59494
#> 2 1.0000000    1.00000
#> 

# Example 3 ----
# Both calendar time and information fraction driven design
gs_design_wlr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  ratio = 1,
  alpha = 0.025, beta = 0.2,
  weight = list(method = "mb", param = list(tau = Inf, w_max = 2)),
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2),
  analysis_time = c(24, 36),
  info_frac = c(0.6, 1)
)
#> $design
#> [1] "wlr"
#> 
#> $enroll_rate
#> # A tibble: 1 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All           12  22.3
#> 
#> $fail_rate
#> # A tibble: 2 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 All            4    0.0462        0.001   1  
#> 2 All          100    0.0462        0.001   0.6
#> 
#> $bounds
#>   analysis bound probability probability0        z ~hr at bound   nominal p
#> 1        1 upper  0.64849271  0.003807973 2.668638    0.6286456 0.003807973
#> 2        1 lower  0.09755399  0.959448880 1.744326    0.7382954 0.040551120
#> 3        2 upper  0.79999962  0.014100260 1.980967    0.7424484 0.023797502
#> 4        2 lower  0.19672405  0.984554570 1.911120    0.7502853 0.027994592
#> 
#> $analysis
#>   analysis     time        n    event       ahr     theta      info     info0
#> 1        1 24.13293 267.1282 132.2063 0.6900749 0.3709551  66.32933  67.78088
#> 2        2 36.00000 267.1282 176.9943 0.6571048 0.2593958 108.73458 112.96871
#>   info_frac info_frac0
#> 1 0.6100114  0.5999969
#> 2 1.0000000  1.0000000
#> 
```
