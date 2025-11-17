# Fixed design under non-proportional hazards

Computes fixed design sample size (given power) or power (given sample
size) by:

- `fixed_design_ahr()` - Average hazard ratio method.

- `fixed_design_fh()` - Weighted logrank test with Fleming-Harrington
  weights (Farrington and Manning, 1990).

- `fixed_design_mb()` - Weighted logrank test with Magirr-Burman
  weights.

- `fixed_design_lf()` - Lachin-Foulkes method (Lachin and Foulkes,
  1986).

- `fixed_design_maxcombo()` - MaxCombo method.

- `fixed_design_rmst()` - RMST method.

- `fixed_design_milestone()` - Milestone method.

Additionally, `fixed_design_rd()` provides fixed design for binary
endpoint with treatment effect measuring in risk difference.

## Usage

``` r
fixed_design_ahr(
  enroll_rate,
  fail_rate,
  alpha = 0.025,
  power = NULL,
  ratio = 1,
  study_duration = 36,
  event = NULL,
  info_scale = c("h0_h1_info", "h0_info", "h1_info")
)

fixed_design_fh(
  alpha = 0.025,
  power = NULL,
  ratio = 1,
  study_duration = 36,
  enroll_rate,
  fail_rate,
  rho = 0,
  gamma = 0,
  info_scale = c("h0_h1_info", "h0_info", "h1_info")
)

fixed_design_lf(
  alpha = 0.025,
  power = NULL,
  ratio = 1,
  study_duration = 36,
  enroll_rate,
  fail_rate
)

fixed_design_maxcombo(
  alpha = 0.025,
  power = NULL,
  ratio = 1,
  study_duration = 36,
  enroll_rate,
  fail_rate,
  rho = c(0, 0, 1),
  gamma = c(0, 1, 0),
  tau = rep(-1, 3)
)

fixed_design_mb(
  alpha = 0.025,
  power = NULL,
  ratio = 1,
  study_duration = 36,
  enroll_rate,
  fail_rate,
  tau = 6,
  w_max = Inf,
  info_scale = c("h0_h1_info", "h0_info", "h1_info")
)

fixed_design_milestone(
  alpha = 0.025,
  power = NULL,
  ratio = 1,
  enroll_rate,
  fail_rate,
  study_duration = 36,
  tau = NULL
)

fixed_design_rd(
  alpha = 0.025,
  power = NULL,
  ratio = 1,
  p_c,
  p_e,
  rd0 = 0,
  n = NULL,
  info_scale = c("h0_h1_info", "h0_info", "h1_info")
)

fixed_design_rmst(
  alpha = 0.025,
  power = NULL,
  ratio = 1,
  study_duration = 36,
  enroll_rate,
  fail_rate,
  tau = NULL
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

  One-sided Type I error (strictly between 0 and 1).

- power:

  Power (`NULL` to compute power or strictly between 0 and `1 - alpha`
  otherwise).

- ratio:

  Experimental:Control randomization ratio.

- study_duration:

  Study duration.

- event:

  A numerical vector specifying the targeted events at each analysis.
  See details.

- info_scale:

  Information scale for calculation. Options are:

  - `"h0_h1_info"` (default): variance under both null and alternative
    hypotheses is used.

  - `"h0_info"`: variance under null hypothesis is used.

  - `"h1_info"`: variance under alternative hypothesis is used.

- rho:

  A vector of numbers paring with gamma and tau for MaxCombo test.

- gamma:

  A vector of numbers paring with rho and tau for MaxCombo test.

- tau:

  Test parameter in RMST.

- w_max:

  Test parameter of Magirr-Burman method.

- p_c:

  A numerical value of the control arm rate.

- p_e:

  A numerical value of the experimental arm rate.

- rd0:

  Risk difference under null hypothesis, default is 0.

- n:

  Sample size. If NULL with power input, the sample size will be
  computed to achieve the targeted power

## Value

A list of design characteristic summary.

## Examples

``` r
# AHR method ----

# Example 1: given power and compute sample size
x <- fixed_design_ahr(
  alpha = .025, power = .9,
  enroll_rate = define_enroll_rate(duration = 18, rate = 1),
  fail_rate = define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12,
    hr = c(1, .6),
    dropout_rate = .001
  ),
  study_duration = 36
)
x |> summary()
#> # A tibble: 1 × 8
#>   Design                   N Events  Time   AHR Bound alpha Power
#>   <chr>                <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Average hazard ratio  463.   325.    36 0.697  1.96 0.025   0.9

# Example 2: given sample size and compute power
x <- fixed_design_ahr(
  alpha = .025,
  enroll_rate = define_enroll_rate(duration = 18, rate = 20),
  fail_rate = define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12,
    hr = c(1, .6),
    dropout_rate = .001
  ),
  study_duration = 36
)
x |> summary()
#> # A tibble: 1 × 8
#>   Design                   N Events  Time   AHR Bound alpha Power
#>   <chr>                <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Average hazard ratio   360   252.    36 0.697  1.96 0.025 0.816

# WLR test with FH weights ----

# Example 1: given power and compute sample size
x <- fixed_design_fh(
  alpha = .025, power = .9,
  enroll_rate = define_enroll_rate(duration = 18, rate = 1),
  fail_rate = define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12,
    hr = c(1, .6),
    dropout_rate = .001
  ),
  study_duration = 36,
  rho = 1, gamma = 1
)
x |> summary()
#> # A tibble: 1 × 8
#>   Design                          N Events  Time   AHR Bound alpha Power
#>   <chr>                       <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Fleming-Harrington FH(1, 1)  352.   247.    36 0.644  1.96 0.025   0.9

# Example 2: given sample size and compute power
x <- fixed_design_fh(
  alpha = .025,
  enroll_rate = define_enroll_rate(duration = 18, rate = 20),
  fail_rate = define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12,
    hr = c(1, .6),
    dropout_rate = .001
  ),
  study_duration = 36,
  rho = 1, gamma = 1
)
x |> summary()
#> # A tibble: 1 × 8
#>   Design                          N Events  Time   AHR Bound alpha Power
#>   <chr>                       <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Fleming-Harrington FH(1, 1)   360   252.    36 0.644  1.96 0.025 0.906

# LF method ----

# Example 1: given power and compute sample size
x <- fixed_design_lf(
  alpha = .025, power = .9,
  enroll_rate = define_enroll_rate(duration = 18, rate = 1),
  fail_rate = define_fail_rate(
    duration = 100,
    fail_rate = log(2) / 12,
    hr = .7,
    dropout_rate = .001
  ),
  study_duration = 36
)
x |> summary()
#> # A tibble: 1 × 7
#>   Design                 N Events  Time Bound alpha Power
#>   <chr>              <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Lachin and Foulkes  463.   329.    36  1.96 0.025   0.9

# Example 2: given sample size and compute power
x <- fixed_design_lf(
  alpha = .025,
  enroll_rate = define_enroll_rate(duration = 18, rate = 20),
  fail_rate = define_fail_rate(
    duration = 100,
    fail_rate = log(2) / 12,
    hr = .7,
    dropout_rate = .001
  ),
  study_duration = 36
)
x |> summary()
#> # A tibble: 1 × 7
#>   Design                 N Events  Time Bound alpha Power
#>   <chr>              <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Lachin and Foulkes   360   256.    36  1.96 0.025 0.816

# MaxCombo test ----

# Example 1: given power and compute sample size
x <- fixed_design_maxcombo(
  alpha = .025, power = .9,
  enroll_rate = define_enroll_rate(duration = 18, rate = 1),
  fail_rate = define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12,
    hr = c(1, .6),
    dropout_rate = .001
  ),
  study_duration = 36,
  rho = c(0, 0.5), gamma = c(0, 0), tau = c(-1, -1)
)
x |> summary()
#> # A tibble: 1 × 7
#>   Design                               N Events  Time Bound alpha Power
#>   <chr>                            <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 MaxCombo: FHC(0, 0), FHC(0.5, 0)  483.   339.    36  2.02 0.025 0.900

# Example 2: given sample size and compute power
x <- fixed_design_maxcombo(
  alpha = .025,
  enroll_rate = define_enroll_rate(duration = 18, rate = 20),
  fail_rate = define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12,
    hr = c(1, .6),
    dropout_rate = .001
  ),
  study_duration = 36,
  rho = c(0, 0.5), gamma = c(0, 0), tau = c(-1, -1)
)
x |> summary()
#> # A tibble: 1 × 7
#>   Design                               N Events  Time Bound alpha Power
#>   <chr>                            <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 MaxCombo: FHC(0, 0), FHC(0.5, 0)  360.   252.    36  2.02 0.025 0.797

# WLR test with MB weights ----

# Example 1: given power and compute sample size
x <- fixed_design_mb(
  alpha = .025, power = .9,
  enroll_rate = define_enroll_rate(duration = 18, rate = 1),
  fail_rate = define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12,
    hr = c(1, .6),
    dropout_rate = .001
  ),
  study_duration = 36,
  tau = 4,
  w_max = 2
)
x |> summary()
#> # A tibble: 1 × 8
#>   Design                            N Events  Time   AHR Bound alpha Power
#>   <chr>                         <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Modestly weighted LR: tau = 4  430.   301.    36 0.688  1.96 0.025   0.9

# Example 2: given sample size and compute power
x <- fixed_design_mb(
  alpha = .025,
  enroll_rate = define_enroll_rate(duration = 18, rate = 20),
  fail_rate = define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12,
    hr = c(1, .6),
    dropout_rate = .001
  ),
  study_duration = 36,
  tau = 4,
  w_max = 2
)
x |> summary()
#> # A tibble: 1 × 8
#>   Design                            N Events  Time   AHR Bound alpha Power
#>   <chr>                         <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Modestly weighted LR: tau = 4   360   252.    36 0.688  1.96 0.025 0.844

# Milestone method ----

# Example 1: given power and compute sample size
x <- fixed_design_milestone(
  alpha = .025, power = .9,
  enroll_rate = define_enroll_rate(duration = 18, rate = 1),
  fail_rate = define_fail_rate(
    duration = 100,
    fail_rate = log(2) / 12,
    hr = .7,
    dropout_rate = .001
  ),
  study_duration = 36,
  tau = 18
)
x |> summary()
#> # A tibble: 1 × 7
#>   Design                  N Events  Time Bound alpha Power
#>   <chr>               <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Milestone: tau = 18  606.   431.    36  1.96 0.025   0.9

# Example 2: given sample size and compute power
x <- fixed_design_milestone(
  alpha = .025,
  enroll_rate = define_enroll_rate(duration = 18, rate = 20),
  fail_rate = define_fail_rate(
    duration = 100,
    fail_rate = log(2) / 12,
    hr = .7,
    dropout_rate = .001
  ),
  study_duration = 36,
  tau = 18
)
x |> summary()
#> # A tibble: 1 × 7
#>   Design                  N Events  Time Bound alpha Power
#>   <chr>               <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Milestone: tau = 18   360   256.    36  1.96 0.025 0.705

# Binary endpoint with risk differences ----

# Example 1: given power and compute sample size
x <- fixed_design_rd(
  alpha = 0.025, power = 0.9, p_c = .15, p_e = .1,
  rd0 = 0, ratio = 1
)
x |> summary()
#> # A tibble: 1 × 5
#>   Design              N Bound alpha Power
#>   <chr>           <dbl> <dbl> <dbl> <dbl>
#> 1 Risk difference 1835.  1.96 0.025   0.9

# Example 2: given sample size and compute power
x <- fixed_design_rd(
  alpha = 0.025, power = NULL, p_c = .15, p_e = .1,
  rd0 = 0, n = 2000, ratio = 1
)
x |> summary()
#> # A tibble: 1 × 5
#>   Design              N Bound alpha Power
#>   <chr>           <dbl> <dbl> <dbl> <dbl>
#> 1 Risk difference  2000  1.96 0.025 0.923

# RMST method ----

# Example 1: given power and compute sample size
x <- fixed_design_rmst(
  alpha = .025, power = .9,
  enroll_rate = define_enroll_rate(duration = 18, rate = 1),
  fail_rate = define_fail_rate(
    duration = 100,
    fail_rate = log(2) / 12,
    hr = .7,
    dropout_rate = .001
  ),
  study_duration = 36,
  tau = 18
)
x |> summary()
#> # A tibble: 1 × 7
#>   Design             N Events  Time Bound alpha Power
#>   <chr>          <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 RMST: tau = 18  671.   477.    36  1.96 0.025   0.9

# Example 2: given sample size and compute power
x <- fixed_design_rmst(
  alpha = .025,
  enroll_rate = define_enroll_rate(duration = 18, rate = 20),
  fail_rate = define_fail_rate(
    duration = 100,
    fail_rate = log(2) / 12,
    hr = .7,
    dropout_rate = .001
  ),
  study_duration = 36,
  tau = 18
)
x |> summary()
#> # A tibble: 1 × 7
#>   Design             N Events  Time Bound alpha Power
#>   <chr>          <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 RMST: tau = 18   360   256.    36  1.96 0.025 0.661
```
