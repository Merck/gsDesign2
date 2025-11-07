# Group sequential design power using weighted log rank test under non-proportional hazards

Group sequential design power using weighted log rank test under
non-proportional hazards

## Usage

``` r
gs_power_wlr(
  enroll_rate = define_enroll_rate(duration = c(2, 2, 10), rate = c(3, 6, 9)),
  fail_rate = tibble(stratum = "All", duration = c(3, 100), fail_rate = log(2)/c(9, 18),
    hr = c(0.9, 0.6), dropout_rate = rep(0.001, 2)),
  event = c(30, 40, 50),
  analysis_time = NULL,
  binding = FALSE,
  h1_spending = TRUE,
  upper = gs_spending_bound,
  lower = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lpar = list(sf = gsDesign::sfLDOF, total_spend = NULL),
  test_upper = TRUE,
  test_lower = TRUE,
  ratio = 1,
  weight = "logrank",
  info_scale = c("h0_h1_info", "h0_info", "h1_info"),
  approx = "asymptotic",
  r = 18,
  tol = 1e-06,
  interval = c(0.01, 1000),
  integer = FALSE
)
```

## Arguments

- enroll_rate:

  Enrollment rates defined by
  [`define_enroll_rate()`](https://merck.github.io/gsDesign2/reference/define_enroll_rate.md).

- fail_rate:

  Failure and dropout rates defined by
  [`define_fail_rate()`](https://merck.github.io/gsDesign2/reference/define_fail_rate.md).

- event:

  A numerical vector specifying the targeted events at each analysis.
  See details.

- analysis_time:

  Targeted calendar timing of analyses. See details.

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

- upper:

  Function to compute upper bound.

  - [`gs_spending_bound()`](https://merck.github.io/gsDesign2/reference/gs_spending_bound.md):
    alpha-spending efficacy bounds.

  - [`gs_b()`](https://merck.github.io/gsDesign2/reference/gs_b.md):
    fixed efficacy bounds.

- lower:

  Function to compute lower bound, which can be set up similarly as
  `upper`. See [this
  vignette](https://merck.github.io/gsDesign2/articles/story-seven-test-types.html).

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

- ratio:

  Experimental:Control randomization ratio.

- weight:

  Weight of weighted log rank test:

  - `"logrank"` = regular logrank test.

  - `list(method = "fh", param = list(rho = ..., gamma = ...))` =
    Fleming-Harrington weighting functions.

  - `list(method = "mb", param = list(tau = ..., w_max = ...))` = Magirr
    and Burman weighting functions.

- info_scale:

  Information scale for calculation. Options are:

  - `"h0_h1_info"` (default): variance under both null and alternative
    hypotheses is used.

  - `"h0_info"`: variance under null hypothesis is used.

  - `"h1_info"`: variance under alternative hypothesis is used.

- approx:

  Approximate estimation method for Z statistics.

  - `"event_driven"` = only work under proportional hazard model with
    log rank test.

  - `"asymptotic"`.

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

## Specification

The contents of this section are shown in PDF user manual only.

## Examples

``` r
library(gsDesign)
library(gsDesign2)

# set enrollment rates
enroll_rate <- define_enroll_rate(duration = 12, rate = 500 / 12)

# set failure rates
fail_rate <- define_fail_rate(
  duration = c(4, 100),
  fail_rate = log(2) / 15, # median survival 15 month
  hr = c(1, .6),
  dropout_rate = 0.001
)

# set the targeted number of events and analysis time
target_events <- c(30, 40, 50)
target_analysisTime <- c(10, 24, 30)

# Example 1 ----
# \donttest{
# fixed bounds and calculate the power for targeted number of events
gs_power_wlr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  event = target_events,
  analysis_time = NULL,
  upper = gs_b,
  upar = gsDesign(
    k = length(target_events),
    test.type = 1,
    n.I = target_events,
    maxn.IPlan = max(target_events),
    sfu = sfLDOF,
    sfupar = NULL
  )$upper$bound,
  lower = gs_b,
  lpar = c(qnorm(.1), rep(-Inf, 2))
)
#> $design
#> [1] "wlr"
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
#> $bounds
#> # A tibble: 4 × 7
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper     0.00441      0.00381  2.67          0.377     0.00381
#> 2        1 lower     0.0915       0.100   -1.28          1.60      0.9    
#> 3        2 upper     0.0161       0.0122   2.29          0.485     0.0110 
#> 4        3 upper     0.0376       0.0250   2.03          0.563     0.0211 
#> 
#> $analysis
#>   analysis     time        n    event       ahr      theta      info     info0
#> 1        1 5.893949 245.5812 29.99999 0.9636346 0.01819417  7.500165  7.500984
#> 2        2 6.900922 287.5384 40.00003 0.9373448 0.03720287  9.998995 10.001907
#> 3        3 7.808453 325.3522 50.00000 0.9155821 0.05738180 12.499564 12.506093
#>   info_frac info_frac0
#> 1 0.6000341  0.5997863
#> 2 0.7999475  0.7997627
#> 3 1.0000000  1.0000000
#> 
# }
# Example 2 ----
# fixed bounds and calculate the power for targeted analysis time
# \donttest{
gs_power_wlr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  event = NULL,
  analysis_time = target_analysisTime,
  upper = gs_b,
  upar = gsDesign(
    k = length(target_events),
    test.type = 1,
    n.I = target_events,
    maxn.IPlan = max(target_events),
    sfu = sfLDOF,
    sfupar = NULL
  )$upper$bound,
  lower = gs_b,
  lpar = c(qnorm(.1), rep(-Inf, 2))
)
#> $design
#> [1] "wlr"
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
#> $bounds
#> # A tibble: 4 × 7
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper      0.0152      0.00381  2.67          0.546     0.00381
#> 2        1 lower      0.0372      0.100   -1.28          1.34      0.9    
#> 3        2 upper      0.622       0.0140   2.29          0.747     0.0110 
#> 4        3 upper      0.842       0.0262   2.03          0.789     0.0211 
#> 
#> $analysis
#>   analysis time        n     event       ahr     theta     info    info0
#> 1        1   10 416.6667  77.80361 0.8720599 0.1140863 19.44920 19.47416
#> 2        2   24 500.0000 246.28341 0.7164215 0.3334865 61.35217 62.08666
#> 3        3   30 500.0000 293.69568 0.6955693 0.3630247 72.91885 74.25144
#>   info_frac info_frac0
#> 1 0.2667239  0.2622732
#> 2 0.8413760  0.8361677
#> 3 1.0000000  1.0000000
#> 
# }
# Example 3 ----
# fixed bounds and calculate the power for targeted analysis time & number of events
# \donttest{
gs_power_wlr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  event = target_events,
  analysis_time = target_analysisTime,
  upper = gs_b,
  upar = gsDesign(
    k = length(target_events),
    test.type = 1,
    n.I = target_events,
    maxn.IPlan = max(target_events),
    sfu = sfLDOF,
    sfupar = NULL
  )$upper$bound,
  lower = gs_b,
  lpar = c(qnorm(.1), rep(-Inf, 2))
)
#> $design
#> [1] "wlr"
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
#> $bounds
#> # A tibble: 4 × 7
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper      0.0152      0.00381  2.67          0.546     0.00381
#> 2        1 lower      0.0372      0.100   -1.28          1.34      0.9    
#> 3        2 upper      0.622       0.0140   2.29          0.747     0.0110 
#> 4        3 upper      0.842       0.0262   2.03          0.789     0.0211 
#> 
#> $analysis
#>   analysis time        n     event       ahr     theta     info    info0
#> 1        1   10 416.6667  77.80361 0.8720599 0.1140863 19.44920 19.47416
#> 2        2   24 500.0000 246.28341 0.7164215 0.3334865 61.35217 62.08666
#> 3        3   30 500.0000 293.69568 0.6955693 0.3630247 72.91885 74.25144
#>   info_frac info_frac0
#> 1 0.2667239  0.2622732
#> 2 0.8413760  0.8361677
#> 3 1.0000000  1.0000000
#> 
# }
# Example 4 ----
# spending bounds and calculate the power for targeted number of events
# \donttest{
gs_power_wlr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  event = target_events,
  analysis_time = NULL,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2)
)
#> $design
#> [1] "wlr"
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
#> $bounds
#> # A tibble: 6 × 7
#>   analysis bound probability probability0      z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl>  <dbl>          <dbl>       <dbl>
#> 1        1 upper     0.00441      0.00380  2.67           0.377     0.00380
#> 2        1 lower     0.0981       0.107   -1.24           1.57      0.893  
#> 3        2 upper     0.0161       0.0122   2.29           0.485     0.0110 
#> 4        2 lower     0.152        0.175   -1.03           1.39      0.849  
#> 5        3 upper     0.0376       0.0250   2.03           0.563     0.0211 
#> 6        3 lower     0.200        0.246   -0.823          1.26      0.795  
#> 
#> $analysis
#>   analysis     time        n    event       ahr      theta      info     info0
#> 1        1 5.893949 245.5812 29.99999 0.9636346 0.01819417  7.500165  7.500984
#> 2        2 6.900922 287.5384 40.00003 0.9373448 0.03720287  9.998995 10.001907
#> 3        3 7.808453 325.3522 50.00000 0.9155821 0.05738180 12.499564 12.506093
#>   info_frac info_frac0
#> 1 0.6000341  0.5997863
#> 2 0.7999475  0.7997627
#> 3 1.0000000  1.0000000
#> 
# }
# Example 5 ----
# spending bounds and calculate the power for targeted analysis time
# \donttest{
gs_power_wlr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  event = NULL,
  analysis_time = target_analysisTime,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2)
)
#> $design
#> [1] "wlr"
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
#> $bounds
#> # A tibble: 6 × 7
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper    0.000101    0.0000121  4.22          0.384   0.0000121
#> 2        1 lower    0.0131      0.0427    -1.72          1.48    0.957    
#> 3        2 upper    0.662       0.0142     2.19          0.756   0.0142   
#> 4        2 lower    0.162       0.946      1.61          0.814   0.0536   
#> 5        3 upper    0.810       0.0226     2.04          0.789   0.0209   
#> 6        3 lower    0.200       0.980      2.13          0.780   0.0167   
#> 
#> $analysis
#>   analysis time        n     event       ahr     theta     info    info0
#> 1        1   10 416.6667  77.80361 0.8720599 0.1140863 19.44920 19.47416
#> 2        2   24 500.0000 246.28341 0.7164215 0.3334865 61.35217 62.08666
#> 3        3   30 500.0000 293.69568 0.6955693 0.3630247 72.91885 74.25144
#>   info_frac info_frac0
#> 1 0.2667239  0.2622732
#> 2 0.8413760  0.8361677
#> 3 1.0000000  1.0000000
#> 
# }
# Example 6 ----
# spending bounds and calculate the power for targeted analysis time & number of events
# \donttest{
gs_power_wlr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  event = target_events,
  analysis_time = target_analysisTime,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2)
)
#> $design
#> [1] "wlr"
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
#> $bounds
#> # A tibble: 6 × 7
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper    0.000101    0.0000121  4.22          0.384   0.0000121
#> 2        1 lower    0.0131      0.0427    -1.72          1.48    0.957    
#> 3        2 upper    0.662       0.0142     2.19          0.756   0.0142   
#> 4        2 lower    0.162       0.946      1.61          0.814   0.0536   
#> 5        3 upper    0.810       0.0226     2.04          0.789   0.0209   
#> 6        3 lower    0.200       0.980      2.13          0.780   0.0167   
#> 
#> $analysis
#>   analysis time        n     event       ahr     theta     info    info0
#> 1        1   10 416.6667  77.80361 0.8720599 0.1140863 19.44920 19.47416
#> 2        2   24 500.0000 246.28341 0.7164215 0.3334865 61.35217 62.08666
#> 3        3   30 500.0000 293.69568 0.6955693 0.3630247 72.91885 74.25144
#>   info_frac info_frac0
#> 1 0.2667239  0.2622732
#> 2 0.8413760  0.8361677
#> 3 1.0000000  1.0000000
#> 
# }
```
