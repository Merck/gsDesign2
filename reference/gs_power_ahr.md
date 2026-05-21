# Group sequential design power using average hazard ratio under non-proportional hazards

Calculate power given the sample size in group sequential design power
using average hazard ratio under non-proportional hazards.

## Usage

``` r
gs_power_ahr(
  enroll_rate = define_enroll_rate(duration = c(2, 2, 10), rate = c(3, 6, 9)),
  fail_rate = define_fail_rate(duration = c(3, 100), fail_rate = log(2)/c(9, 18), hr =
    c(0.9, 0.6), dropout_rate = rep(0.001, 2)),
  event = c(30, 40, 50),
  analysis_time = NULL,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = NULL),
  test_lower = TRUE,
  test_upper = TRUE,
  ratio = 1,
  binding = FALSE,
  h1_spending = TRUE,
  info_scale = c("h0_h1_info", "h0_info", "h1_info"),
  r = 18,
  tol = 1e-06,
  interval = c(0.01, 1000),
  integer = FALSE
)
```

## Arguments

- enroll_rate:

  An `enroll_rate` data frame with or without stratum created by
  [`define_enroll_rate()`](https://merck.github.io/gsDesign2/reference/define_enroll_rate.md).

- fail_rate:

  A `fail_rate` data frame with or without stratum created by
  [`define_fail_rate()`](https://merck.github.io/gsDesign2/reference/define_fail_rate.md).

- event:

  A numerical vector specifying the targeted events at each analysis.
  See details.

- analysis_time:

  Targeted calendar timing of analyses. See details.

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

- test_lower:

  Indicator of which analyses should include a lower bound; single value
  of `TRUE` (default) indicates all analyses; single value of `FALSE`
  indicated no lower bound; otherwise, a logical vector of the same
  length as `info` should indicate which analyses will have a lower
  bound.

- test_upper:

  Indicator of which analyses should include an upper (efficacy) bound;
  single value of `TRUE` (default) indicates all analyses; otherwise, a
  logical vector of the same length as `info` should indicate which
  analyses will have an efficacy bound.

- ratio:

  Experimental:Control randomization ratio.

- binding:

  Indicator of whether futility bound is binding; default of `FALSE` is
  recommended.

- h1_spending:

  Indicator that lower bound to be set by spending under alternate
  hypothesis (input `fail_rate`) if spending is used for lower bound. If
  this is `FALSE`, then the lower bound spending is under the null
  hypothesis. This is for two-sided symmetric or asymmetric testing
  under the null hypothesis; See [this
  vignette](https://merck.github.io/gsDesign2/articles/story-seven-test-types.html).

- info_scale:

  Information scale for calculation. Options are:

  - `"h0_h1_info"` (default): variance under both null and alternative
    hypotheses is used.

  - `"h0_info"`: variance under null hypothesis is used. This is often
    used for testing methods that use local alternatives, such as the
    Schoenfeld method.

  - `"h1_info"`: variance under alternative hypothesis is used.

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

- integer:

  Indicator of whether integer sample size and events are intended. This
  argument is used when using
  [`to_integer()`](https://merck.github.io/gsDesign2/reference/to_integer.md).

## Value

A list with input parameters, enrollment rate, analysis, and bound.

- `$input` a list including `alpha`, `beta`, `ratio`, etc.

- `$enroll_rate` a table showing the enrollment, which is the same as
  input.

- `$fail_rate` a table showing the failure and dropout rates, which is
  the same as input.

- `$bound` a table summarizing the efficacy and futility bound at each
  analysis.

- `analysis` a table summarizing the analysis time, sample size, events,
  average HR, treatment effect and statistical information at each
  analysis.

## Details

Note that time units are arbitrary, but should be the same for all rate
parameters in `enroll_rate`, `fail_rate`, and `analysis_time`.

Computed bounds satisfy input upper bound specification in `upper`,
`upar`, and lower bound specification in `lower`, `lpar`.
[`ahr()`](https://merck.github.io/gsDesign2/reference/ahr.md) computes
statistical information at targeted event times. The
[`expected_time()`](https://merck.github.io/gsDesign2/reference/expected_time.md)
function is used to get events and average HR at targeted
`analysis_time`.

The parameters `event` and `analysis_time` are used to determine the
timing for interim and final analyses.

- If analysis timing is to be determined by targeted events, then
  `event` is a numerical vector specifying the targeted events for each
  analysis; note that this can be NULL.

- If interim analysis is determined by targeted calendar timing relative
  to start of enrollment, then `analysis_time` will be a vector
  specifying the calendar time from start of study for each analysis;
  note that this can be NULL.

- A corresponding element of `event` or `analysis_time` should be
  provided for each analysis.

- If both `event[i]` and `analysis[i]` are provided for analysis `i`,
  then the time corresponding to the later of these is used for analysis
  `i`.

## Specification

The contents of this section are shown in PDF user manual only.

## Examples

``` r
library(gsDesign2)

# Example 1 ----
# The default output of `gs_power_ahr()` is driven by events,
# i.e., `event = c(30, 40, 50)`, `analysis_time = NULL`
# \donttest{
gs_power_ahr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1))
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
#>   analysis bound probability probability0          z ~hr at bound   nominal p
#> 1        1 upper  0.02306940  0.003808109  2.6686261    0.3774024 0.003808109
#> 2        1 lower  0.03491806  0.120797743 -1.1710082    1.5335590 0.879202257
#> 3        2 upper  0.08972624  0.012211793  2.2887245    0.4849264 0.011047682
#> 4        2 lower  0.06678169  0.265485575 -0.6627182    1.2331476 0.746244479
#> 5        3 upper  0.20699328  0.024999954  2.0307067    0.5630591 0.021142377
#> 6        3 lower  0.10075852  0.430308332 -0.2266532    1.0662067 0.589653279
#> 
#> $analysis
#>   analysis     time   n    event       ahr     theta      info    info0
#> 1        1 14.90817 108 30.00008 0.7865726 0.2400702  7.373433  7.50002
#> 2        2 19.16437 108 40.00000 0.7442008 0.2954444  9.789940 10.00000
#> 3        3 24.54264 108 50.00000 0.7128241 0.3385206 12.227632 12.50000
#>   info_frac info_frac0
#> 1 0.6030140  0.6000016
#> 2 0.8006407  0.8000001
#> 3 1.0000000  1.0000000
#> 
# }
# Example 2 ----
# 2-sided symmetric O'Brien-Fleming spending bound, driven by analysis time,
# i.e., `event = NULL`, `analysis_time = c(12, 24, 36)`

gs_power_ahr(
  analysis_time = c(12, 24, 36),
  event = NULL,
  binding = TRUE,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025)
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
#> 1        1 upper 3.696216e-04 5.380432e-05  3.8727626    0.1800180 5.380432e-05
#> 2        1 lower 6.119278e-05 3.429196e-04 -3.3951783    4.4962519 9.996571e-01
#> 3        2 upper 1.157981e-01 9.209304e-03  2.3578702    0.5100743 9.190059e-03
#> 4        2 lower 9.065088e-03 1.145743e-01 -1.2026786    1.4097022 8.854496e-01
#> 5        3 upper 3.238741e-01 2.500000e-02  2.0095985    0.6102822 2.223685e-02
#> 6        3 lower 2.500579e-02 3.237821e-01 -0.4730657    1.1232767 6.819168e-01
#> 
#> $analysis
#>   analysis time   n    event       ahr     theta      info     info0 info_frac
#> 1        1   12  90 20.40451 0.8107539 0.2097907  5.028327  5.101127 0.3090946
#> 2        2   24 108 49.06966 0.7151566 0.3352538 11.999266 12.267415 0.7376029
#> 3        3   36 108 66.23948 0.6833395 0.3807634 16.267921 16.559870 1.0000000
#>   info_frac0
#> 1  0.3080415
#> 2  0.7407917
#> 3  1.0000000
#> 

# Example 3 ----
# 2-sided symmetric O'Brien-Fleming spending bound, driven by event,
# i.e., `event = c(20, 50, 70)`, `analysis_time = NULL`
# Note that this assumes targeted final events for the design is 70 events.
# If actual targeted final events were 65, then `timing = c(20, 50, 70) / 65`
# would be added to `upar` and `lpar` lists.
# NOTE: at present the computed information fraction in output `analysis` is based
# on 70 events rather than 65 events when the `timing` argument is used in this way.
# A vignette on this topic will be forthcoming.
# \donttest{
gs_power_ahr(
  analysis_time = NULL,
  event = c(20, 50, 70),
  binding = TRUE,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025)
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
#> 1        1 upper 1.984165e-04 2.749491e-05  4.0333392    0.1646779 2.749491e-05
#> 2        1 lower 3.117637e-05 1.809140e-04 -3.5664657    4.9281909 9.998191e-01
#> 3        2 upper 1.102017e-01 8.000150e-03  2.4093654    0.5058721 7.990145e-03
#> 4        2 lower 7.822504e-03 1.086183e-01 -1.2339754    1.4176789 8.913940e-01
#> 5        3 upper 3.516872e-01 2.500000e-02  2.0032206    0.6194884 2.257681e-02
#> 6        3 lower 2.500317e-02 3.516547e-01 -0.3933009    1.0985783 6.529514e-01
#> 
#> $analysis
#>   analysis     time        n event       ahr     theta      info info0
#> 1        1 11.87087  88.8378    20 0.8119328 0.2083377  4.929331   5.0
#> 2        2 24.54264 108.0000    50 0.7128241 0.3385206 12.227632  12.5
#> 3        3 39.39207 108.0000    70 0.6785816 0.3877506 17.218358  17.5
#>   info_frac info_frac0
#> 1 0.2862834  0.2857143
#> 2 0.7101509  0.7142857
#> 3 1.0000000  1.0000000
#> 
# }
# Example 4 ----
# 2-sided symmetric O'Brien-Fleming spending bound,
# driven by both `event` and `analysis_time`, i.e.,
# both `event` and `analysis_time` are not `NULL`,
# then the analysis will driven by the maximal one, i.e.,
# Time = max(analysis_time, calculated Time for targeted event)
# Events = max(events, calculated events for targeted analysis_time)
# \donttest{
gs_power_ahr(
  analysis_time = c(12, 24, 36),
  event = c(30, 40, 50), h1_spending = FALSE,
  binding = TRUE,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025)
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
#>   analysis bound  probability probability0         z ~hr at bound    nominal p
#> 1        1 upper 7.063221e-03 0.0008667173  3.132468    0.3186016 0.0008667173
#> 2        1 lower 8.570402e-05 0.0008667173 -3.132468    3.1387166 0.9991332827
#> 3        2 upper 1.145733e-01 0.0092093035  2.368721    0.5084965 0.0089248526
#> 4        2 lower 2.721433e-04 0.0092093035 -2.368721    1.9665818 0.9910751474
#> 5        3 upper 3.237355e-01 0.0250000000  2.010883    0.6100896 0.0221689184
#> 6        3 lower 4.052127e-04 0.0250000000 -2.010883    1.6391035 0.9778310816
#> 
#> $analysis
#>   analysis     time   n    event       ahr     theta      info    info0
#> 1        1 14.90817 108 30.00008 0.7865726 0.2400702  7.373433  7.50002
#> 2        2 24.00000 108 49.06966 0.7151566 0.3352538 11.999266 12.26741
#> 3        3 36.00000 108 66.23948 0.6833395 0.3807634 16.267921 16.55987
#>   info_frac info_frac0
#> 1 0.4532499  0.4529033
#> 2 0.7376029  0.7407917
#> 3 1.0000000  1.0000000
#> 
# }
```
