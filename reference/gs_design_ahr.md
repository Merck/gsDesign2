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
#> 
#> Attaching package: ‘gsDesign’
#> The following objects are masked from ‘package:gsDesign2’:
#> 
#>     as_gt, as_rtf
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
#> # A tibble: 1 × 7
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <dbl> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper         0.9        0.025  1.96          0.795      0.0250
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
#> # A tibble: 1 × 7
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <dbl> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper         0.9        0.025  1.96          0.791      0.0250
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
#> # A tibble: 6 × 7
#>   analysis bound probability probability0      z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl>  <dbl>          <dbl>       <dbl>
#> 1        1 upper     0.00248    0.0000538  3.87           0.459   0.0000538
#> 2        1 lower     0.00321    0.0443    -1.70           1.41    0.956    
#> 3        2 upper     0.579      0.00921    2.36           0.736   0.00919  
#> 4        2 lower     0.0556     0.830      0.953          0.884   0.170    
#> 5        3 upper     0.900      0.0244     2.01           0.799   0.0222   
#> 6        3 lower     0.100      0.976      2.01           0.799   0.0223   
#> 
#> $analysis
#> # A tibble: 3 × 10
#>   analysis  time     n event   ahr theta  info info0 info_frac info_frac0
#>      <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1    12  436.  98.8 0.811 0.210  24.4  24.7     0.309      0.308
#> 2        2    24  523. 238.  0.715 0.335  58.1  59.4     0.738      0.741
#> 3        3    36  523. 321.  0.683 0.381  78.8  80.2     1          1    
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
#> # A tibble: 6 × 7
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper    0.000295   0.00000737  4.33          0.380  0.00000737
#> 2        1 lower    0.00108    0.0135     -2.21          1.64   0.987     
#> 3        2 upper    0.599      0.00965     2.34          0.740  0.00965   
#> 4        2 lower    0.0570     0.843       1.01          0.878  0.157     
#> 5        3 upper    0.900      0.0244      2.01          0.799  0.0221    
#> 6        3 lower    0.100      0.976       2.01          0.799  0.0221    
#> 
#> $analysis
#> # A tibble: 3 × 10
#>   analysis  time     n event   ahr theta  info info0 info_frac info_frac0
#>      <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1  10.7  382.  80.4 0.823 0.195  19.8  20.1     0.251      0.250
#> 2        2  24.4  524. 241.  0.714 0.337  59.0  60.3     0.747      0.750
#> 3        3  36    524. 322.  0.683 0.381  79.0  80.4     1          1    
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
#> # A tibble: 6 × 7
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper     0.00251    0.0000538  3.87          0.460   0.0000538
#> 2        1 lower     0.00321    0.0446    -1.70          1.41    0.955    
#> 3        2 upper     0.635      0.0105     2.31          0.746   0.0104   
#> 4        2 lower     0.0599     0.862      1.09          0.871   0.138    
#> 5        3 upper     0.900      0.0243     2.02          0.799   0.0219   
#> 6        3 lower     0.100      0.976      2.01          0.799   0.0220   
#> 
#> $analysis
#> # A tibble: 3 × 10
#>   analysis  time     n event   ahr theta  info info0 info_frac info_frac0
#>      <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1    12  439.  99.5 0.811 0.210  24.5  24.9     0.309      0.308
#> 2        2    25  527. 248.  0.711 0.341  60.5  61.9     0.763      0.766
#> 3        3    36  527. 323.  0.683 0.381  79.3  80.7     1          1    
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
#> # A tibble: 6 × 7
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper     0.00510     0.000104  3.71          0.490    0.000104
#> 2        1 lower     0.00459     0.0665   -1.50          1.33     0.934   
#> 3        2 upper     0.701       0.0122    2.25          0.756    0.0122  
#> 4        2 lower     0.0655      0.896     1.26          0.856    0.104   
#> 5        3 upper     0.900       0.0241    2.03          0.799    0.0214  
#> 6        3 lower     0.100       0.976     2.02          0.799    0.0216  
#> 
#> $analysis
#> # A tibble: 3 × 10
#>   analysis  time     n event   ahr theta  info info0 info_frac info_frac0
#>      <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1  12.5  465.  108. 0.806 0.216  26.7  27.1     0.334      0.333
#> 2        2  26.4  530.  260. 0.706 0.348  63.7  65.1     0.797      0.800
#> 3        3  36    530.  325. 0.683 0.381  79.9  81.3     1          1    
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
#> # A tibble: 6 × 7
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper 0.00226        0.0000538  3.87          0.449   0.0000538
#> 2        1 lower 0.000000613    0.0000538 -3.87          2.23    1.000    
#> 3        2 upper 0.550          0.00921    2.36          0.730   0.00919  
#> 4        2 lower 0.00000125     0.00921   -2.36          1.37    0.991    
#> 5        3 upper 0.900          0.0250     2.01          0.794   0.0222   
#> 6        3 lower 0.00000128     0.0250    -2.01          1.26    0.978    
#> 
#> $analysis
#> # A tibble: 3 × 10
#>   analysis  time     n event   ahr theta  info info0 info_frac info_frac0
#>      <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1    12  412.  93.4 0.811 0.210  23.0  23.3     0.309      0.308
#> 2        2    24  494. 224.  0.715 0.335  54.9  56.1     0.738      0.741
#> 3        3    36  494. 303.  0.683 0.381  74.4  75.8     1          1    
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
#> # A tibble: 6 × 7
#>   analysis bound probability probability0      z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl>  <dbl>          <dbl>       <dbl>
#> 1        1 upper     0.00305    0.0000538  3.87           0.481   0.0000538
#> 2        1 lower     0.0430     0.268     -0.619          1.12    0.732    
#> 3        2 upper     0.638      0.00921    2.36           0.750   0.00920  
#> 4        2 lower     0.0823     0.874      1.13           0.871   0.129    
#> 5        3 upper     0.900      0.0250     1.98           0.813   0.0240   
#> 6        3 lower     0.100      0.975      1.97           0.813   0.0243   
#> 
#> $analysis
#> # A tibble: 3 × 10
#>   analysis  time     n event   ahr theta  info info0 info_frac info_frac0
#>      <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1    12  494.  112. 0.811 0.210  27.6  28.0     0.309      0.308
#> 2        2    24  593.  269. 0.715 0.335  65.9  67.3     0.738      0.741
#> 3        3    36  593.  364. 0.683 0.381  89.3  90.9     1          1    
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
#> # A tibble: 3 × 7
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper    0.000619   0.00000679  4.35          0.435  0.00000679
#> 2        2 upper    0.505      0.00371     2.68          0.719  0.00371   
#> 3        3 upper    0.900      0.0125      2.28          0.785  0.0114    
#> 
#> $analysis
#> # A tibble: 3 × 10
#>   analysis  time     n event   ahr theta  info info0 info_frac info_frac0
#>      <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1    12  483.  109. 0.811 0.210  27.0  27.4     0.309      0.308
#> 2        2    24  579.  263. 0.715 0.335  64.3  65.8     0.738      0.741
#> 3        3    36  579.  355. 0.683 0.381  87.2  88.8     1          1    
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
#> # A tibble: 3 × 7
#>   analysis bound probability probability0     z `~hr at bound` `nominal p`
#>      <int> <chr>       <dbl>        <dbl> <dbl>          <dbl>       <dbl>
#> 1        1 upper   0.0000938  0.000000587  4.86          0.395 0.000000587
#> 2        2 upper   0.513      0.00393      2.66          0.721 0.00393    
#> 3        3 upper   0.900      0.0125       2.28          0.785 0.0113     
#> 
#> $analysis
#> # A tibble: 3 × 10
#>   analysis  time     n event   ahr theta  info info0 info_frac info_frac0
#>      <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1    12  483.  110. 0.811 0.210  27.0  27.4     0.309      0.308
#> 2        2    24  580.  263. 0.715 0.335  64.4  65.9     0.738      0.741
#> 3        3    36  580.  356. 0.683 0.381  87.3  88.9     1          1    
#> 
# }
```
