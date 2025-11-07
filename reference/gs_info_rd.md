# Information and effect size under risk difference

Information and effect size under risk difference

## Usage

``` r
gs_info_rd(
  p_c = tibble::tibble(stratum = "All", rate = 0.2),
  p_e = tibble::tibble(stratum = "All", rate = 0.15),
  n = tibble::tibble(stratum = "All", n = c(100, 200, 300), analysis = 1:3),
  rd0 = 0,
  ratio = 1,
  weight = c("unstratified", "ss", "invar")
)
```

## Arguments

- p_c:

  Rate at the control group.

- p_e:

  Rate at the experimental group.

- n:

  Sample size.

- rd0:

  The risk difference under H0.

- ratio:

  Experimental:Control randomization ratio.

- weight:

  Weighting method, can be `"unstratified"`, `"ss"`, or `"invar"`.

## Value

A tibble with columns as analysis index, sample size, risk difference,
risk difference under null hypothesis, theta1 (standardized treatment
effect under alternative hypothesis), theta0 (standardized treatment
effect under null hypothesis), and statistical information.

## Examples

``` r
# Example 1 ----
# unstratified case with H0: rd0 = 0
gs_info_rd(
  p_c = tibble::tibble(stratum = "All", rate = .15),
  p_e = tibble::tibble(stratum = "All", rate = .1),
  n = tibble::tibble(stratum = "All", n = c(100, 200, 300), analysis = 1:3),
  rd0 = 0,
  ratio = 1
)
#> # A tibble: 3 × 8
#>   analysis     n    rd   rd0 theta1 theta0 info1 info0
#>      <int> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>
#> 1        1   100  0.05     0   0.05      0  230.  229.
#> 2        2   200  0.05     0   0.05      0  460.  457.
#> 3        3   300  0.05     0   0.05      0  690.  686.

# Example 2 ----
# unstratified case with H0: rd0 != 0
gs_info_rd(
  p_c = tibble::tibble(stratum = "All", rate = .2),
  p_e = tibble::tibble(stratum = "All", rate = .15),
  n = tibble::tibble(stratum = "All", n = c(100, 200, 300), analysis = 1:3),
  rd0 = 0.005,
  ratio = 1
)
#> # A tibble: 3 × 8
#>   analysis     n    rd   rd0 theta1 theta0 info1 info0
#>      <int> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>
#> 1        1   100  0.05 0.005   0.05  0.005  174.  173.
#> 2        2   200  0.05 0.005   0.05  0.005  348.  346.
#> 3        3   300  0.05 0.005   0.05  0.005  522.  519.

# Example 3 ----
# stratified case under sample size weighting and H0: rd0 = 0
gs_info_rd(
  p_c = tibble::tibble(stratum = c("S1", "S2", "S3"), rate = c(.15, .2, .25)),
  p_e = tibble::tibble(stratum = c("S1", "S2", "S3"), rate = c(.1, .16, .19)),
  n = tibble::tibble(
    stratum = rep(c("S1", "S2", "S3"), each = 3),
    analysis = rep(1:3, 3),
    n = c(50, 100, 200, 40, 80, 160, 60, 120, 240)
  ),
  rd0 = 0,
  ratio = 1,
  weight = "ss"
)
#> # A tibble: 3 × 8
#>   analysis     n     rd   rd0 theta1 theta0 info1 info0
#>      <int> <dbl>  <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>
#> 1        1   150 0.0513     0 0.0513      0  261.  260.
#> 2        2   300 0.0513     0 0.0513      0  522.  519.
#> 3        3   600 0.0513     0 0.0513      0 1043. 1038.

# Example 4 ----
# stratified case under inverse variance weighting and H0: rd0 = 0
gs_info_rd(
  p_c = tibble::tibble(
    stratum = c("S1", "S2", "S3"),
    rate = c(.15, .2, .25)
  ),
  p_e = tibble::tibble(
    stratum = c("S1", "S2", "S3"),
    rate = c(.1, .16, .19)
  ),
  n = tibble::tibble(
    stratum = rep(c("S1", "S2", "S3"), each = 3),
    analysis = rep(1:3, 3),
    n = c(50, 100, 200, 40, 80, 160, 60, 120, 240)
  ),
  rd0 = 0,
  ratio = 1,
  weight = "invar"
)
#> # A tibble: 3 × 8
#>   analysis     n     rd   rd0 theta1 theta0 info1 info0
#>      <int> <dbl>  <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>
#> 1        1   150 0.0507     0 0.0507      0  271.  269.
#> 2        2   300 0.0507     0 0.0507      0  542.  539.
#> 3        3   600 0.0507     0 0.0507      0 1083. 1078.

# Example 5 ----
# stratified case under sample size weighting and H0: rd0 != 0
gs_info_rd(
  p_c = tibble::tibble(
    stratum = c("S1", "S2", "S3"),
    rate = c(.15, .2, .25)
  ),
  p_e = tibble::tibble(
    stratum = c("S1", "S2", "S3"),
    rate = c(.1, .16, .19)
  ),
  n = tibble::tibble(
    stratum = rep(c("S1", "S2", "S3"), each = 3),
    analysis = rep(1:3, 3),
    n = c(50, 100, 200, 40, 80, 160, 60, 120, 240)
  ),
  rd0 = 0.02,
  ratio = 1,
  weight = "ss"
)
#> # A tibble: 3 × 8
#>   analysis     n     rd   rd0 theta1 theta0 info1 info0
#>      <int> <dbl>  <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>
#> 1        1   150 0.0513  0.02 0.0513   0.02  261.  260.
#> 2        2   300 0.0513  0.02 0.0513   0.02  522.  519.
#> 3        3   600 0.0513  0.02 0.0513   0.02 1043. 1038.

# Example 6 ----
# stratified case under inverse variance weighting and H0: rd0 != 0
gs_info_rd(
  p_c = tibble::tibble(
    stratum = c("S1", "S2", "S3"),
    rate = c(.15, .2, .25)
  ),
  p_e = tibble::tibble(
    stratum = c("S1", "S2", "S3"),
    rate = c(.1, .16, .19)
  ),
  n = tibble::tibble(
    stratum = rep(c("S1", "S2", "S3"), each = 3),
    analysis = rep(1:3, 3),
    n = c(50, 100, 200, 40, 80, 160, 60, 120, 240)
  ),
  rd0 = 0.02,
  ratio = 1,
  weight = "invar"
)
#> # A tibble: 3 × 8
#>   analysis     n     rd   rd0 theta1 theta0 info1 info0
#>      <int> <dbl>  <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>
#> 1        1   150 0.0507  0.02 0.0507   0.02  271.  269.
#> 2        2   300 0.0507  0.02 0.0507   0.02  542.  539.
#> 3        3   600 0.0507  0.02 0.0507   0.02 1083. 1078.

# Example 7 ----
# stratified case under inverse variance weighting and H0: rd0 != 0 and
# rd0 difference for different statum
gs_info_rd(
  p_c = tibble::tibble(
    stratum = c("S1", "S2", "S3"),
    rate = c(.15, .2, .25)
  ),
  p_e = tibble::tibble(
    stratum = c("S1", "S2", "S3"),
    rate = c(.1, .16, .19)
  ),
  n = tibble::tibble(
    stratum = rep(c("S1", "S2", "S3"), each = 3),
    analysis = rep(1:3, 3),
    n = c(50, 100, 200, 40, 80, 160, 60, 120, 240)
  ),
  rd0 = tibble::tibble(
    stratum = c("S1", "S2", "S3"),
    rd0 = c(0.01, 0.02, 0.03)
  ),
  ratio = 1,
  weight = "invar"
)
#> # A tibble: 3 × 8
#>   analysis     n     rd    rd0 theta1 theta0 info1 info0
#>      <int> <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>
#> 1        1   150 0.0507 0.0190 0.0507 0.0190  271.  269.
#> 2        2   300 0.0507 0.0190 0.0507 0.0190  542.  539.
#> 3        3   600 0.0507 0.0190 0.0507 0.0190 1083. 1078.
```
