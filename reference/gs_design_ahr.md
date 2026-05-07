# Calculate sample size and bounds given targeted power and Type I error in group sequential design using average hazard ratio under non-proportional hazards

Calculate sample size and bounds given targeted power and Type I error
in group sequential design using average hazard ratio under
non-proportional hazards

## Usage

``` r
gs_design_ahr(
  enroll_rate = define_enroll_rate(duration = c(2, 2, 10), rate = c(3, 6, 9)),
  fail_rate = define_fail_rate(duration = c(3, 100), fail_rate = log(2)/c(9, 18), hr =
    c(0.9, 0.6), dropout_rate = 0.001),
  alpha = 0.025,
  beta = 0.1,
  info_frac = NULL,
  analysis_time = 36,
  ratio = 1,
  binding = FALSE,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = alpha),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = beta),
  h1_spending = TRUE,
  test_upper = TRUE,
  test_lower = TRUE,
  info_scale = c("h0_h1_info", "h0_info", "h1_info"),
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

- alpha:

  One-sided Type I error.

- beta:

  Type II error.

- info_frac:

  Targeted information fraction for analyses. See details.

- analysis_time:

  Targeted calendar timing of analyses. See details.

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

- h1_spending:

  Indicator that lower bound to be set by spending under alternate
  hypothesis (input `fail_rate`) if spending is used for lower bound. If
  this is `FALSE`, then the lower bound spending is under the null
  hypothesis. This is for two-sided symmetric or asymmetric testing
  under the null hypothesis; See [this
  vignette](https://merck.github.io/gsDesign2/articles/story-seven-test-types.html).

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

## Value

A list with input parameters, enrollment rate, analysis, and bound.

- The `$input` is a list including `alpha`, `beta`, `ratio`, etc.

- The `$enroll_rate` is a table showing the enrollment for arriving the
  targeted power (`1 - beta`).

- The `$fail_rate` is a table showing the failure and dropout rates,
  which is the same as input.

- The `$bound` is a table summarizing the efficacy and futility bound
  per analysis.

- The `analysis` is a table summarizing the analysis time, sample size,
  events, average HR, treatment effect and statistical information per
  analysis.

## Details

The parameters `info_frac` and `analysis_time` are used to determine the
timing for interim and final analyses.

- If the interim analysis is determined by targeted information fraction
  and the study duration is known, then `info_frac` is a numerical
  vector where each element (greater than 0 and less than or equal to 1)
  represents the information fraction for each analysis. The
  `analysis_time`, which defaults to 36, indicates the time for the
  final analysis.

- If interim analyses are determined solely by the targeted calendar
  analysis timing from start of study, then `analysis_time` will be a
  vector specifying the time for each analysis.

- If both the targeted analysis time and the targeted information
  fraction are utilized for a given analysis, then timing will be the
  maximum of the two with both `info_frac` and `analysis_time` provided
  as vectors.

## Specification

The contents of this section are shown in PDF user manual only.

## Examples

``` r
library(gsDesign)
library(gsDesign2)

# Example 1 ----
# call with defaults
gs_design_ahr()
#> $design
#> [1] "ahr"
#> 
#> $enroll_rate
#> # A tibble: 3 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All            2  13.2
#> 2 All            2  26.4
#> 3 All           10  39.7
#> 
#> $fail_rate
#> # A tibble: 2 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 All            3    0.0770        0.001   0.9
#> 2 All          100    0.0385        0.001   0.6
#> 
#> $bound
#> # A tibble: 1 × 8
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <dbl> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper         0.9        0.025  1.96          0.795      0.0250
#> # ℹ 1 more variable: spending_time <dbl>
#> 
#> $analysis
#> # A tibble: 1 × 10
#>   analysis  time     n event   ahr theta  info info0 info_frac info_frac0
#>      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1    36  476.  292. 0.683 0.381  71.7  73.0         1          1
#> 

# Example 2 ----
# Single analysis
gs_design_ahr(analysis_time = 40)
#> $design
#> [1] "ahr"
#> 
#> $enroll_rate
#> # A tibble: 3 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All            2  11.9
#> 2 All            2  23.8
#> 3 All           10  35.6
#> 
#> $fail_rate
#> # A tibble: 2 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 All            3    0.0770        0.001   0.9
#> 2 All          100    0.0385        0.001   0.6
#> 
#> $bound
#> # A tibble: 1 × 8
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <dbl> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper         0.9        0.025  1.96          0.791      0.0250
#> # ℹ 1 more variable: spending_time <dbl>
#> 
#> $analysis
#> # A tibble: 1 × 10
#>   analysis  time     n event   ahr theta  info info0 info_frac info_frac0
#>      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1    40  428.  280. 0.678 0.389  68.8  69.9         1          1
#> 

# Example 3 ----
# Multiple analysis_time
gs_design_ahr(analysis_time = c(12, 24, 36))
#> $design
#> [1] "ahr"
#> 
#> $enroll_rate
#> # A tibble: 3 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All            2  14.5
#> 2 All            2  29.1
#> 3 All           10  43.6
#> 
#> $fail_rate
#> # A tibble: 2 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 All            3    0.0770        0.001   0.9
#> 2 All          100    0.0385        0.001   0.6
#> 
#> $bound
#>   analysis bound probability probability0          z ~hr at bound    nominal p
#> 1        1 upper 0.002479915 5.380432e-05  3.8727626    0.4588306 5.380432e-05
#> 2        1 lower 0.003207242 4.432143e-02 -1.7026005    1.4084756 9.556786e-01
#> 3        2 upper 0.578701034 9.209255e-03  2.3578702    0.7364837 9.190059e-03
#> 4        2 lower 0.055582503 8.297907e-01  0.9530459    0.8837056 1.702834e-01
#> 5        3 upper 0.899999961 2.442393e-02  2.0095985    0.7990165 2.223685e-02
#> 6        3 lower 0.100116520 9.755007e-01  2.0078723    0.7991706 2.232843e-02
#>   spending_time
#> 1     0.3080415
#> 2     0.3090946
#> 3     0.7407917
#> 4     0.7376029
#> 5     1.0000000
#> 6     1.0000000
#> 
#> $analysis
#>   analysis time        n    event       ahr     theta     info    info0
#> 1        1   12 435.9740  98.8426 0.8107539 0.2097907 24.35800 24.71065
#> 2        2   24 523.1688 237.7010 0.7151566 0.3352538 58.12631 59.42526
#> 3        3   36 523.1688 320.8743 0.6833395 0.3807634 78.80434 80.21858
#>   info_frac info_frac0
#> 1 0.3090946  0.3080415
#> 2 0.7376029  0.7407917
#> 3 1.0000000  1.0000000
#> 

# Example 4 ----
# Specified information fraction
# \donttest{
gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = 36)
#> $design
#> [1] "ahr"
#> 
#> $enroll_rate
#> # A tibble: 3 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All            2  14.6
#> 2 All            2  29.1
#> 3 All           10  43.7
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
#> 1        1 upper 0.0002945223 7.366808e-06  4.332634    0.3804673 7.366808e-06
#> 2        1 lower 0.0010823223 1.345927e-02 -2.212697    1.6380756 9.865407e-01
#> 3        2 upper 0.5993463111 9.649295e-03  2.339820    0.7398520 9.646511e-03
#> 4        2 lower 0.0570422553 8.432739e-01  1.007926    0.8782768 1.567449e-01
#> 5        3 upper 0.8999999973 2.436367e-02  2.011797    0.7990301 2.212066e-02
#> 6        3 lower 0.1000503718 9.756261e-01  2.011548    0.7990523 2.213380e-02
#>   spending_time
#> 1     0.2500000
#> 2     0.2512264
#> 3     0.7500000
#> 4     0.7468041
#> 5     1.0000000
#> 6     1.0000000
#> 
#> $analysis
#>   analysis     time        n     event       ahr     theta     info    info0
#> 1        1 10.73156 381.5646  80.40639 0.8229230 0.1948927 19.84408 20.10160
#> 2        2 24.35467 524.3936 241.21917 0.7136166 0.3374094 58.98919 60.30479
#> 3        3 36.00000 524.3936 321.62557 0.6833395 0.3807634 78.98884 80.40639
#>   info_frac info_frac0
#> 1 0.2512264       0.25
#> 2 0.7468041       0.75
#> 3 1.0000000       1.00
#> 
# }

# Example 5 ----
# multiple analysis times & info_frac
# driven by times
gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
#> $design
#> [1] "ahr"
#> 
#> $enroll_rate
#> # A tibble: 3 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All            2  14.6
#> 2 All            2  29.3
#> 3 All           10  43.9
#> 
#> $fail_rate
#> # A tibble: 2 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 All            3    0.0770        0.001   0.9
#> 2 All          100    0.0385        0.001   0.6
#> 
#> $bound
#>   analysis bound probability probability0         z ~hr at bound    nominal p
#> 1        1 upper 0.002505682 5.380432e-05  3.872763    0.4599776 5.380432e-05
#> 2        1 lower 0.003207011 4.463400e-02 -1.699272    1.4059916 9.553660e-01
#> 3        2 upper 0.634806350 1.046196e-02  2.310046    0.7455333 1.044282e-02
#> 4        2 lower 0.059857340 8.619541e-01  1.088909    0.8707301 1.380971e-01
#> 5        3 upper 0.900000428 2.428875e-02  2.016112    0.7990118 2.189413e-02
#> 6        3 lower 0.100116288 9.756359e-01  2.014278    0.7991749 2.199015e-02
#>   spending_time
#> 1     0.3080415
#> 2     0.3090946
#> 3     0.7664817
#> 4     0.7632948
#> 5     1.0000000
#> 6     1.0000000
#> 
#> $analysis
#>   analysis time        n     event       ahr     theta     info    info0
#> 1        1   12 438.7817  99.47917 0.8107539 0.2097907 24.51487 24.86979
#> 2        2   25 526.5381 247.52823 0.7109605 0.3411384 60.53833 61.88206
#> 3        3   36 526.5381 322.94083 0.6833395 0.3807634 79.31186 80.73521
#>   info_frac info_frac0
#> 1 0.3090946  0.3080415
#> 2 0.7632948  0.7664817
#> 3 1.0000000  1.0000000
#> 
# driven by info_frac
# \donttest{
gs_design_ahr(info_frac = c(1 / 3, .8, 1), analysis_time = c(12, 25, 36))
#> $design
#> [1] "ahr"
#> 
#> $enroll_rate
#> # A tibble: 3 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All            2  14.7
#> 2 All            2  29.5
#> 3 All           10  44.2
#> 
#> $fail_rate
#> # A tibble: 2 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 All            3    0.0770        0.001   0.9
#> 2 All          100    0.0385        0.001   0.6
#> 
#> $bound
#>   analysis bound probability probability0         z ~hr at bound    nominal p
#> 1        1 upper 0.005102901 0.0001035057  3.710303    0.4903590 0.0001035057
#> 2        1 lower 0.004590196 0.0664882373 -1.502467    1.3345186 0.9335117627
#> 3        2 upper 0.701184007 0.0122114654  2.251552    0.7564332 0.0121752899
#> 4        2 lower 0.065541312 0.8955906719  1.256569    0.8557424 0.1044548880
#> 5        3 upper 0.900000000 0.0240966693  2.025216    0.7988572 0.0214226084
#> 6        3 lower 0.100147410 0.9757621525  2.021450    0.7991908 0.0216165769
#>   spending_time
#> 1     0.3333333
#> 2     0.3342920
#> 3     0.7999993
#> 4     0.7969173
#> 5     1.0000000
#> 6     1.0000000
#> 
#> $analysis
#>   analysis     time        n    event       ahr     theta     info    info0
#> 1        1 12.52524 465.2054 108.4341 0.8060952 0.2155534 26.70719 27.10852
#> 2        2 26.35614 530.3883 260.2416 0.7059240 0.3482478 63.66716 65.06039
#> 3        3 36.00000 530.3883 325.3022 0.6833395 0.3807634 79.89180 81.32556
#>   info_frac info_frac0
#> 1 0.3342920  0.3333333
#> 2 0.7969173  0.7999993
#> 3 1.0000000  1.0000000
#> 
# }

# Example 6 ----
# 2-sided symmetric design with O'Brien-Fleming spending
# \donttest{
gs_design_ahr(
  analysis_time = c(12, 24, 36),
  binding = TRUE,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
  h1_spending = FALSE
)
#> $design
#> [1] "ahr"
#> 
#> $enroll_rate
#> # A tibble: 3 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All            2  13.7
#> 2 All            2  27.5
#> 3 All           10  41.2
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
#> 1        1 upper 2.264189e-03 5.380432e-05  3.872763    0.4485852 5.380432e-05
#> 2        1 lower 6.134009e-07 5.380432e-05 -3.872763    2.2292311 9.999462e-01
#> 3        2 upper 5.503774e-01 9.209304e-03  2.357870    0.7299828 9.190059e-03
#> 4        2 lower 1.246335e-06 9.209304e-03 -2.357870    1.3698952 9.908099e-01
#> 5        3 upper 8.999998e-01 2.500000e-02  2.009598    0.7938368 2.223685e-02
#> 6        3 lower 1.282770e-06 2.500000e-02 -2.009598    1.2597048 9.777631e-01
#>   spending_time
#> 1     0.3080415
#> 2     0.3080415
#> 3     0.7407917
#> 4     0.7407917
#> 5     1.0000000
#> 6     1.0000000
#> 
#> $analysis
#>   analysis time        n     event       ahr     theta     info    info0
#> 1        1   12 411.7572  93.35226 0.8107539 0.2097907 23.00500 23.33807
#> 2        2   24 494.1087 224.49763 0.7151566 0.3352538 54.89761 56.12441
#> 3        3   36 494.1087 303.05094 0.6833395 0.3807634 74.42705 75.76273
#>   info_frac info_frac0
#> 1 0.3090946  0.3080415
#> 2 0.7376029  0.7407917
#> 3 1.0000000  1.0000000
#> 
# }
# 2-sided asymmetric design with O'Brien-Fleming upper spending
# Pocock lower spending under H1 (NPH)
# \donttest{
gs_design_ahr(
  analysis_time = c(12, 24, 36),
  binding = TRUE,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDPocock, total_spend = 0.1, param = NULL, timing = NULL),
  h1_spending = TRUE
)
#> $design
#> [1] "ahr"
#> 
#> $enroll_rate
#> # A tibble: 3 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All            2  16.5
#> 2 All            2  32.9
#> 3 All           10  49.4
#> 
#> $fail_rate
#> # A tibble: 2 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 All            3    0.0770        0.001   0.9
#> 2 All          100    0.0385        0.001   0.6
#> 
#> $bound
#>   analysis bound probability probability0          z ~hr at bound    nominal p
#> 1        1 upper 0.003046091 5.380432e-05  3.8727626    0.4810316 5.380432e-05
#> 2        1 lower 0.043002963 2.679463e-01 -0.6190362    1.1240937 7.320537e-01
#> 3        2 upper 0.637893715 9.209304e-03  2.3573828    0.7503185 9.202133e-03
#> 4        2 lower 0.082267363 8.735656e-01  1.1319087    0.8711614 1.288364e-01
#> 5        3 upper 0.900000169 2.500000e-02  1.9770563    0.8127335 2.401763e-02
#> 6        3 lower 0.100403556 9.748223e-01  1.9729193    0.8130862 2.425238e-02
#>   spending_time
#> 1     0.3080415
#> 2     0.3090946
#> 3     0.7407917
#> 4     0.7376029
#> 5     1.0000000
#> 6     1.0000000
#> 
#> $analysis
#>   analysis time        n    event       ahr     theta     info    info0
#> 1        1   12 494.0910 112.0187 0.8107539 0.2097907 27.60501 28.00468
#> 2        2   24 592.9092 269.3875 0.7151566 0.3352538 65.87477 67.34688
#> 3        3   36 592.9092 363.6481 0.6833395 0.3807634 89.30926 90.91203
#>   info_frac info_frac0
#> 1 0.3090946  0.3080415
#> 2 0.7376029  0.7407917
#> 3 1.0000000  1.0000000
#> 
# }

# Example 7 ----
# \donttest{
gs_design_ahr(
  alpha = 0.0125,
  analysis_time = c(12, 24, 36),
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.0125, param = NULL, timing = NULL),
  lower = gs_b,
  lpar = rep(-Inf, 3)
)
#> $design
#> [1] "ahr"
#> 
#> $enroll_rate
#> # A tibble: 3 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All            2  16.1
#> 2 All            2  32.2
#> 3 All           10  48.3
#> 
#> $fail_rate
#> # A tibble: 2 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 All            3    0.0770        0.001   0.9
#> 2 All          100    0.0385        0.001   0.6
#> 
#> $bound
#>   analysis bound  probability probability0        z ~hr at bound    nominal p
#> 1        1 upper 0.0006187876 6.787365e-06 4.350629    0.4352554 6.787365e-06
#> 2        2 upper 0.5046620214 3.708221e-03 2.677778    0.7188168 3.705614e-03
#> 3        3 upper 0.8999996284 1.250000e-02 2.278051    0.7852612 1.136178e-02
#>   spending_time
#> 1     0.3080415
#> 2     0.7407917
#> 3     1.0000000
#> 
#> $analysis
#>   analysis time        n    event       ahr     theta     info    info0
#> 1        1   12 482.6356 109.4216 0.8107539 0.2097907 26.96500 27.35539
#> 2        2   24 579.1627 263.1418 0.7151566 0.3352538 64.34748 65.78546
#> 3        3   36 579.1627 355.2170 0.6833395 0.3807634 87.23865 88.80425
#>   info_frac info_frac0
#> 1 0.3090946  0.3080415
#> 2 0.7376029  0.7407917
#> 3 1.0000000  1.0000000
#> 

gs_design_ahr(
  alpha = 0.0125,
  analysis_time = c(12, 24, 36),
  upper = gs_b,
  upar = gsDesign::gsDesign(
    k = 3, test.type = 1, n.I = c(.25, .75, 1),
    sfu = sfLDOF, sfupar = NULL, alpha = 0.0125
  )$upper$bound,
  lower = gs_b,
  lpar = rep(-Inf, 3)
)
#> $design
#> [1] "ahr"
#> 
#> $enroll_rate
#> # A tibble: 3 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All            2  16.1
#> 2 All            2  32.2
#> 3 All           10  48.3
#> 
#> $fail_rate
#> # A tibble: 2 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 All            3    0.0770        0.001   0.9
#> 2 All          100    0.0385        0.001   0.6
#> 
#> $bound
#>   analysis bound  probability probability0        z ~hr at bound    nominal p
#> 1        1 upper 9.381159e-05 5.871061e-07 4.859940    0.3950765 5.871061e-07
#> 2        2 upper 5.129257e-01 3.925339e-03 2.658446    0.7206655 3.925096e-03
#> 3        3 upper 8.999996e-01 1.254926e-02 2.280095    0.7851982 1.130103e-02
#> 
#> $analysis
#>   analysis time        n    event       ahr     theta     info    info0
#> 1        1   12 483.1812 109.5453 0.8107539 0.2097907 26.99548 27.38632
#> 2        2   24 579.8174 263.4393 0.7151566 0.3352538 64.42022 65.85982
#> 3        3   36 579.8174 355.6186 0.6833395 0.3807634 87.33726 88.90464
#>   info_frac info_frac0
#> 1 0.3090946  0.3080415
#> 2 0.7376029  0.7407917
#> 3 1.0000000  1.0000000
#> 
# }
```
