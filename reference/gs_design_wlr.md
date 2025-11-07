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

  Enrollment rates defined by
  [`define_enroll_rate()`](https://merck.github.io/gsDesign2/reference/define_enroll_rate.md).

- fail_rate:

  Failure and dropout rates defined by
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

  - `"h0_info"`: variance under null hypothesis is used.

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

  Indicator of which analyses should include an lower bound; single
  value of `TRUE` (default) indicates all analyses; single value `FALSE`
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
#> # A tibble: 4 × 7
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper      0.648       0.00381  2.67          0.629     0.00381
#> 2        1 lower      0.0976      0.959    1.74          0.738     0.0406 
#> 3        2 upper      0.800       0.0141   1.98          0.742     0.0238 
#> 4        2 lower      0.197       0.985    1.91          0.750     0.0280 
#> 
#> $analysis
#> # A tibble: 2 × 10
#>   analysis  time     n event   ahr theta  info info0 info_frac info_frac0
#>      <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1  24.1  267.  132. 0.690 0.371  66.3  67.8     0.610      0.600
#> 2        2  36    267.  177. 0.657 0.259 109.  113.      1          1    
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
#> # A tibble: 4 × 7
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper      0.326       0.00366 2.68           0.643     0.00366
#> 2        1 lower      0.0977      0.821   0.918          0.860     0.179  
#> 3        2 upper      0.800       0.0228  1.98           0.755     0.0238 
#> 4        2 lower      0.198       0.975   1.93           0.760     0.0267 
#> 
#> $analysis
#> # A tibble: 2 × 10
#>   analysis  time     n event   ahr theta  info info0 info_frac info_frac0
#>      <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1    24  300.  148. 0.690 0.256  73.9  75.5     0.605      0.595
#> 2        2    36  300.  199. 0.657 0.259 122.  127.      1          1    
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
#> # A tibble: 4 × 7
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper      0.648       0.00381  2.67          0.629     0.00381
#> 2        1 lower      0.0976      0.959    1.74          0.738     0.0406 
#> 3        2 upper      0.800       0.0141   1.98          0.742     0.0238 
#> 4        2 lower      0.197       0.985    1.91          0.750     0.0280 
#> 
#> $analysis
#> # A tibble: 2 × 10
#>   analysis  time     n event   ahr theta  info info0 info_frac info_frac0
#>      <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1  24.1  267.  132. 0.690 0.371  66.3  67.8     0.610      0.600
#> 2        2  36    267.  177. 0.657 0.259 109.  113.      1          1    
#> 
```
