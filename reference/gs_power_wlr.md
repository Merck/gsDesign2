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

  Indicator of which analyses should include a lower bound; single value
  of `TRUE` (default) indicates all analyses; single value of `FALSE`
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

  - `"h0_info"`: variance under null hypothesis is used. This is often
    used for testing methods that use local alternatives, such as the
    Schoenfeld method.

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
#>   analysis bound probability probability0         z ~hr at bound   nominal p
#> 1        1 upper  0.00441383  0.003808063  2.668630    0.3774013 0.003808063
#> 2        1 lower  0.09154369  0.100000000 -1.281552    1.5967280 0.900000000
#> 3        2 upper  0.01610019  0.012212156  2.288719    0.4849273 0.011047838
#> 4        3 upper  0.03762726  0.025003975  2.030702    0.5630598 0.021142598
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
#>   analysis bound probability probability0         z ~hr at bound   nominal p
#> 1        1 upper  0.01524044  0.003808063  2.668630    0.5460264 0.003808063
#> 2        1 lower  0.03722282  0.100000000 -1.281552    1.3372033 0.900000000
#> 3        2 upper  0.62184674  0.013972843  2.288719    0.7470084 0.011047838
#> 4        3 upper  0.84166072  0.026176770  2.030702    0.7890003 0.021142598
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
#>   analysis bound probability probability0         z ~hr at bound   nominal p
#> 1        1 upper  0.01524044  0.003808063  2.668630    0.5460264 0.003808063
#> 2        1 lower  0.03722282  0.100000000 -1.281552    1.3372033 0.900000000
#> 3        2 upper  0.62184674  0.013972843  2.288719    0.7470084 0.011047838
#> 4        3 upper  0.84166072  0.026176770  2.030702    0.7890003 0.021142598
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
#>   analysis bound probability probability0         z ~hr at bound   nominal p
#> 1        1 upper 0.004406704  0.003801818  2.669181    0.3773254 0.003801818
#> 2        1 lower 0.098052129  0.106939244 -1.242971    1.5743916 0.893060756
#> 3        2 upper 0.016083715  0.012198958  2.289102    0.4848686 0.011036707
#> 4        2 lower 0.151908085  0.175484064 -1.032357    1.3860527 0.849047639
#> 5        3 upper 0.037624764  0.024999973  2.030628    0.5630717 0.021146372
#> 6        3 lower 0.200011733  0.246299344 -0.822957    1.2620879 0.794733770
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
#>   analysis bound  probability probability0         z ~hr at bound    nominal p
#> 1        1 upper 0.0001007196 1.205109e-05  4.223047    0.3838367 1.205109e-05
#> 2        1 lower 0.0131221657 4.266668e-02 -1.720545    1.4771554 9.573333e-01
#> 3        2 upper 0.6617929637 1.423798e-02  2.190753    0.7563933 1.423484e-02
#> 4        2 lower 0.1624075802 9.464630e-01  1.611318    0.8143626 5.355520e-02
#> 5        3 upper 0.8101441292 2.258665e-02  2.035169    0.7885891 2.091694e-02
#> 6        3 lower 0.2000371763 9.796421e-01  2.127943    0.7800971 1.667092e-02
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
#>   analysis bound  probability probability0         z ~hr at bound    nominal p
#> 1        1 upper 0.0001007196 1.205109e-05  4.223047    0.3838367 1.205109e-05
#> 2        1 lower 0.0131221657 4.266668e-02 -1.720545    1.4771554 9.573333e-01
#> 3        2 upper 0.6617929637 1.423798e-02  2.190753    0.7563933 1.423484e-02
#> 4        2 lower 0.1624075802 9.464630e-01  1.611318    0.8143626 5.355520e-02
#> 5        3 upper 0.8101441292 2.258665e-02  2.035169    0.7885891 2.091694e-02
#> 6        3 lower 0.2000371763 9.796421e-01  2.127943    0.7800971 1.667092e-02
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
