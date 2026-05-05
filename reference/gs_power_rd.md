# Group sequential design power of binary outcome measuring in risk difference

Group sequential design power of binary outcome measuring in risk
difference

## Usage

``` r
gs_power_rd(
  p_c = tibble::tibble(stratum = "All", rate = 0.2),
  p_e = tibble::tibble(stratum = "All", rate = 0.15),
  n = tibble::tibble(stratum = "All", n = c(40, 50, 60), analysis = 1:3),
  rd0 = 0,
  ratio = 1,
  weight = c("unstratified", "ss", "invar", "mr"),
  upper = gs_b,
  lower = gs_b,
  upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
  lpar = c(qnorm(0.1), rep(-Inf, 2)),
  info_scale = c("h0_h1_info", "h0_info", "h1_info"),
  binding = FALSE,
  test_upper = TRUE,
  test_lower = TRUE,
  r = 18,
  tol = 1e-06
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

  Treatment effect under super-superiority designs, the default is 0.

- ratio:

  Experimental:control randomization ratio.

- weight:

  Weighting method, can be `"unstratified"`, `"ss"`, or `"invar"`.

- upper:

  Function to compute upper bound.

- lower:

  Function to compare lower bound.

- upar:

  Parameters passed to `upper`.

- lpar:

  Parameters passed to `lower`.

- info_scale:

  Information scale for calculation. Options are:

  - `"h0_h1_info"` (default): variance under both null and alternative
    hypotheses is used.

  - `"h0_info"`: variance under null hypothesis is used.

  - `"h1_info"`: variance under alternative hypothesis is used.

- binding:

  Indicator of whether futility bound is binding; default of `FALSE` is
  recommended.

- test_upper:

  Indicator of which analyses should include an upper (efficacy) bound;
  single value of `TRUE` (default) indicates all analyses; otherwise, a
  logical vector of the same length as `info` should indicate which
  analyses will have an efficacy bound.

- test_lower:

  Indicator of which analyses should include a lower bound; single value
  of `TRUE` (default) indicates all analyses; single value `FALSE`
  indicated no lower bound; otherwise, a logical vector of the same
  length as `info` should indicate which analyses will have a lower
  bound.

- r:

  Integer value controlling grid for numerical integration as in
  Jennison and Turnbull (2000); default is 18, range is 1 to 80. Larger
  values provide larger number of grid points and greater accuracy.
  Normally, `r` will not be changed by the user.

- tol:

  Tolerance parameter for boundary convergence (on Z-scale).

## Value

A list with input parameter, analysis, and bound.

## Examples

``` r
# Example 1 ----
library(gsDesign)

# unstratified case with H0: rd0 = 0
gs_power_rd(
  p_c = tibble::tibble(
    stratum = "All",
    rate = .2
  ),
  p_e = tibble::tibble(
    stratum = "All",
    rate = .15
  ),
  n = tibble::tibble(
    stratum = "All",
    n = c(20, 40, 60),
    analysis = 1:3
  ),
  rd0 = 0,
  ratio = 1,
  upper = gs_b,
  lower = gs_b,
  upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
  lpar = c(qnorm(.1), rep(-Inf, 2))
)
#> $design
#> [1] "rd"
#> 
#> $bound
#>   analysis bound  probability probability0         z ~risk difference at bound
#> 1        1 upper 0.0003091285 0.0001035057  3.710303                 0.6291125
#> 2        2 upper 0.0181812128 0.0060484866  2.511427                 0.3011095
#> 3        3 upper 0.0728412074 0.0249830182  1.993048                 0.1951084
#> 4        1 lower 0.0571429811 0.1000000000 -1.281552                -0.2172976
#>      nominal p
#> 1 0.0001035057
#> 2 0.0060122074
#> 3 0.0231281218
#> 4 0.9000000000
#> 
#> $analysis
#> # A tibble: 3 × 10
#>   analysis     n    rd   rd0 theta1 theta0  info info0 info_frac info_frac0
#>      <int> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1    20  0.05     0   0.05      0  34.8  34.6     0.333      0.333
#> 2        2    40  0.05     0   0.05      0  69.6  69.3     0.667      0.667
#> 3        3    60  0.05     0   0.05      0 104.  104.      1          1    
#> 

# Example 2 ----
# unstratified case with H0: rd0 != 0
gs_power_rd(
  p_c = tibble::tibble(
    stratum = "All",
    rate = .2
  ),
  p_e = tibble::tibble(
    stratum = "All",
    rate = .15
  ),
  n = tibble::tibble(
    stratum = "All",
    n = c(20, 40, 60),
    analysis = 1:3
  ),
  rd0 = 0.005,
  ratio = 1,
  upper = gs_b,
  lower = gs_b,
  upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
  lpar = c(qnorm(.1), rep(-Inf, 2))
)
#> $design
#> [1] "rd"
#> 
#> $bound
#>   analysis bound  probability probability0         z ~risk difference at bound
#> 1        1 upper 0.0003091285 0.0001162159  3.710303                 0.5712012
#> 2        2 upper 0.0181812128 0.0067985887  2.511427                 0.2759986
#> 3        3 upper 0.0728412074 0.0280730655  1.993048                 0.1805976
#> 4        1 lower 0.0571429811 0.0949329260 -1.281552                -0.1905679
#>      nominal p
#> 1 0.0001035057
#> 2 0.0060122074
#> 3 0.0231281218
#> 4 0.9000000000
#> 
#> $analysis
#> # A tibble: 3 × 10
#>   analysis     n    rd   rd0 theta1 theta0  info info0 info_frac info_frac0
#>      <int> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1    20  0.05 0.005   0.05  0.005  34.8  34.6     0.333      0.333
#> 2        2    40  0.05 0.005   0.05  0.005  69.6  69.3     0.667      0.667
#> 3        3    60  0.05 0.005   0.05  0.005 104.  104.      1          1    
#> 

# use spending function
gs_power_rd(
  p_c = tibble::tibble(
    stratum = "All",
    rate = .2
  ),
  p_e = tibble::tibble(
    stratum = "All",
    rate = .15
  ),
  n = tibble::tibble(
    stratum = "All",
    n = c(20, 40, 60),
    analysis = 1:3
  ),
  rd0 = 0.005,
  ratio = 1,
  upper = gs_spending_bound,
  lower = gs_b,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
  lpar = c(qnorm(.1), rep(-Inf, 2))
)
#> $design
#> [1] "rd"
#> 
#> $bound
#>   analysis bound  probability probability0         z ~risk difference at bound
#> 1        1 upper 0.0003091285 0.0001162159  3.710303                 0.5712012
#> 2        2 upper 0.0181809114 0.0067984607  2.511434                 0.2759993
#> 3        3 upper 0.0728406608 0.0280728068  1.993051                 0.1805979
#> 4        1 lower 0.0571429811 0.0949329260 -1.281552                -0.1905679
#>      nominal p
#> 1 0.0001035057
#> 2 0.0060120917
#> 3 0.0231279272
#> 4 0.9000000000
#> 
#> $analysis
#> # A tibble: 3 × 10
#>   analysis     n    rd   rd0 theta1 theta0  info info0 info_frac info_frac0
#>      <int> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1    20  0.05 0.005   0.05  0.005  34.8  34.6     0.333      0.333
#> 2        2    40  0.05 0.005   0.05  0.005  69.6  69.3     0.667      0.667
#> 3        3    60  0.05 0.005   0.05  0.005 104.  104.      1          1    
#> 

# Example 3 ----
# stratified case under sample size weighting and H0: rd0 = 0
gs_power_rd(
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
    n = c(10, 20, 24, 18, 26, 30, 10, 20, 24)
  ),
  rd0 = 0,
  ratio = 1,
  weight = "ss",
  upper = gs_b,
  lower = gs_b,
  upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
  lpar = c(qnorm(.1), rep(-Inf, 2))
)
#> $design
#> [1] "rd"
#> 
#> $bound
#>   analysis bound  probability probability0         z ~risk difference at bound
#> 1        1 upper 0.0004374648 0.0001035057  3.710303                 0.4556368
#> 2        2 upper 0.0237074382 0.0060370551  2.511427                 0.2278406
#> 3        3 upper 0.0795090157 0.0236920419  1.993048                 0.1658102
#> 4        1 lower 0.0470452990 0.1000000000 -1.281552                -0.1573785
#>      nominal p
#> 1 0.0001035057
#> 2 0.0060122074
#> 3 0.0231281218
#> 4 0.9000000000
#> 
#> $analysis
#> # A tibble: 3 × 10
#>   analysis     n     rd   rd0 theta1 theta0  info info0 info_frac info_frac0
#>      <int> <dbl>  <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1    38 0.0479     0 0.0479      0  66.3  66.0     0.485      0.485
#> 2        2    66 0.0491     0 0.0491      0 116.  115.      0.846      0.846
#> 3        3    78 0.0492     0 0.0492      0 137.  136.      1          1    
#> 

# Example 4 ----
# stratified case under inverse variance weighting and H0: rd0 = 0
gs_power_rd(
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
    n = c(10, 20, 24, 18, 26, 30, 10, 20, 24)
  ),
  rd0 = 0,
  ratio = 1,
  weight = "invar",
  upper = gs_b,
  lower = gs_b,
  upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
  lpar = c(qnorm(.1), rep(-Inf, 2))
)
#> $design
#> [1] "rd"
#> 
#> $bound
#>   analysis bound  probability probability0         z ~risk difference at bound
#> 1        1 upper 0.0004427274 0.0001035057  3.710303                 0.4492645
#> 2        2 upper 0.0239817291 0.0060373908  2.511427                 0.2246733
#> 3        3 upper 0.0803017559 0.0236948429  1.993048                 0.1635101
#> 4        1 lower 0.0466790577 0.1000000000 -1.281552                -0.1551775
#>      nominal p
#> 1 0.0001035057
#> 2 0.0060122074
#> 3 0.0231281218
#> 4 0.9000000000
#> 
#> $analysis
#> # A tibble: 3 × 10
#>   analysis     n     rd   rd0 theta1 theta0  info info0 info_frac info_frac0
#>      <int> <dbl>  <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1    38 0.0477     0 0.0477      0  68.2  67.9     0.483      0.483
#> 2        2    66 0.0488     0 0.0488      0 119.  119.      0.845      0.845
#> 3        3    78 0.0489     0 0.0489      0 141.  141.      1          1    
#> 

# Example 5 ----
# stratified case under sample size weighting and H0: rd0 != 0
gs_power_rd(
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
    n = c(10, 20, 24, 18, 26, 30, 10, 20, 24)
  ),
  rd0 = 0.02,
  ratio = 1,
  weight = "ss",
  upper = gs_b,
  lower = gs_b,
  upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
  lpar = c(qnorm(.1), rep(-Inf, 2))
)
#> $design
#> [1] "rd"
#> 
#> $bound
#>   analysis bound  probability probability0         z ~risk difference at bound
#> 1        1 upper 0.0004374648 0.0001942497  3.710303                0.28537089
#> 2        2 upper 0.0237074382 0.0108531061  2.511427                0.15269839
#> 3        3 upper 0.0795090157 0.0400648305  1.993048                0.11657078
#> 4        1 lower 0.0470452990 0.0743583639 -1.281552               -0.07166003
#>      nominal p
#> 1 0.0001035057
#> 2 0.0060122074
#> 3 0.0231281218
#> 4 0.9000000000
#> 
#> $analysis
#> # A tibble: 3 × 10
#>   analysis     n     rd   rd0 theta1 theta0  info info0 info_frac info_frac0
#>      <int> <dbl>  <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1    38 0.0479  0.02 0.0479   0.02  66.3  66.0     0.485      0.485
#> 2        2    66 0.0491  0.02 0.0491   0.02 116.  115.      0.846      0.846
#> 3        3    78 0.0492  0.02 0.0492   0.02 137.  136.      1          1    
#> 

# Example 6 ----
# stratified case under inverse variance weighting and H0: rd0 != 0
gs_power_rd(
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
    n = c(10, 20, 24, 18, 26, 30, 10, 20, 24)
  ),
  rd0 = 0.03,
  ratio = 1,
  weight = "invar",
  upper = gs_b,
  lower = gs_b,
  upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
  lpar = c(qnorm(.1), rep(-Inf, 2))
)
#> $design
#> [1] "rd"
#> 
#> $bound
#>   analysis bound  probability probability0         z ~risk difference at bound
#> 1        1 upper 0.0004427274  0.000267021  3.710303                0.19650137
#> 2        2 upper 0.0239817291  0.014517313  2.511427                0.11326589
#> 3        3 upper 0.0803017559  0.051793142  1.993048                0.09059827
#> 4        1 lower 0.0466790577  0.063159416 -1.281552               -0.02751015
#>      nominal p
#> 1 0.0001035057
#> 2 0.0060122074
#> 3 0.0231281218
#> 4 0.9000000000
#> 
#> $analysis
#> # A tibble: 3 × 10
#>   analysis     n     rd   rd0 theta1 theta0  info info0 info_frac info_frac0
#>      <int> <dbl>  <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1    38 0.0477  0.03 0.0477   0.03  68.2  67.9     0.483      0.483
#> 2        2    66 0.0488  0.03 0.0488   0.03 119.  119.      0.845      0.845
#> 3        3    78 0.0489  0.03 0.0489   0.03 141.  141.      1          1    
#> 
```
