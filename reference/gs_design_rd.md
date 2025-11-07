# Group sequential design of binary outcome measuring in risk difference

Group sequential design of binary outcome measuring in risk difference

## Usage

``` r
gs_design_rd(
  p_c = tibble::tibble(stratum = "All", rate = 0.2),
  p_e = tibble::tibble(stratum = "All", rate = 0.15),
  info_frac = 1:3/3,
  rd0 = 0,
  alpha = 0.025,
  beta = 0.1,
  ratio = 1,
  stratum_prev = NULL,
  weight = c("unstratified", "ss", "invar"),
  upper = gs_b,
  lower = gs_b,
  upar = gsDesign(k = 3, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
  lpar = c(qnorm(0.1), rep(-Inf, 2)),
  test_upper = TRUE,
  test_lower = TRUE,
  info_scale = c("h0_h1_info", "h0_info", "h1_info"),
  binding = FALSE,
  r = 18,
  tol = 1e-06,
  h1_spending = TRUE
)
```

## Arguments

- p_c:

  Rate at the control group.

- p_e:

  Rate at the experimental group.

- info_frac:

  Statistical information fraction.

- rd0:

  Treatment effect under super-superiority designs, the default is 0.

- alpha:

  One-sided Type I error.

- beta:

  Type II error.

- ratio:

  Experimental:Control randomization ratio (not yet implemented).

- stratum_prev:

  Randomization ratio of different stratum. If it is unstratified design
  then `NULL`. Otherwise it is a tibble containing two columns (stratum
  and prevalence).

- weight:

  The weighting scheme for stratified population.

- upper:

  Function to compute upper bound.

- lower:

  Function to compute lower bound.

- upar:

  Parameters passed to `upper`.

- lpar:

  Parameters passed to `lower`.

- test_upper:

  Indicator of which analyses should include an upper (efficacy) bound;
  single value of `TRUE` (default) indicates all analyses; otherwise, a
  logical vector of the same length as `info` should indicate which
  analyses will have an efficacy bound.

- test_lower:

  Indicator of which analyses should include an lower bound; single
  value of `TRUE` (default) indicates all analyses; single value of
  `FALSE` indicates no lower bound; otherwise, a logical vector of the
  same length as `info` should indicate which analyses will have a lower
  bound.

- info_scale:

  Information scale for calculation. Options are:

  - `"h0_h1_info"` (default): variance under both null and alternative
    hypotheses is used.

  - `"h0_info"`: variance under null hypothesis is used.

  - `"h1_info"`: variance under alternative hypothesis is used.

- binding:

  Indicator of whether futility bound is binding; default of `FALSE` is
  recommended.

- r:

  Integer value controlling grid for numerical integration as in
  Jennison and Turnbull (2000); default is 18, range is 1 to 80. Larger
  values provide larger number of grid points and greater accuracy.
  Normally, `r` will not be changed by the user.

- tol:

  Tolerance parameter for boundary convergence (on Z-scale).

- h1_spending:

  Indicator that lower bound to be set by spending under alternate
  hypothesis (input `fail_rate`) if spending is used for lower bound.

## Value

A list with input parameters, analysis, and bound.

## Details

To be added.

## Examples

``` r
library(gsDesign)

# Example 1 ----
# unstratified group sequential design
x <- gs_design_rd(
  p_c = tibble::tibble(stratum = "All", rate = .2),
  p_e = tibble::tibble(stratum = "All", rate = .15),
  info_frac = c(0.7, 1),
  rd0 = 0,
  alpha = .025,
  beta = .1,
  ratio = 1,
  stratum_prev = NULL,
  weight = "unstratified",
  upper = gs_b,
  lower = gs_b,
  upar = gsDesign(k = 2, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
  lpar = c(qnorm(.1), rep(-Inf, 2))
)

y <- gs_power_rd(
  p_c = tibble::tibble(stratum = "All", rate = .2),
  p_e = tibble::tibble(stratum = "All", rate = .15),
  n = tibble::tibble(stratum = "All", n = x$analysis$n, analysis = 1:2),
  rd0 = 0,
  ratio = 1,
  weight = "unstratified",
  upper = gs_b,
  lower = gs_b,
  upar = gsDesign(k = 2, test.type = 1, sfu = sfLDOF, sfupar = NULL)$upper$bound,
  lpar = c(qnorm(.1), rep(-Inf, 2))
)

# The above 2 design share the same power with the same sample size and treatment effect
x$bound$probability[x$bound$bound == "upper" & x$bound$analysis == 2]
#> [1] 0.9
y$bound$probability[y$bound$bound == "upper" & y$bound$analysis == 2]
#> [1] 0.9

# Example 2 ----
# stratified group sequential design
gs_design_rd(
  p_c = tibble::tibble(
    stratum = c("biomarker positive", "biomarker negative"),
    rate = c(.2, .25)
  ),
  p_e = tibble::tibble(
    stratum = c("biomarker positive", "biomarker negative"),
    rate = c(.15, .22)
  ),
  info_frac = c(0.7, 1),
  rd0 = 0,
  alpha = .025,
  beta = .1,
  ratio = 1,
  stratum_prev = tibble::tibble(
    stratum = c("biomarker positive", "biomarker negative"),
    prevalence = c(.4, .6)
  ),
  weight = "ss",
  upper = gs_spending_bound, lower = gs_b,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
  lpar = rep(-Inf, 2)
)
#> $design
#> [1] "rd"
#> 
#> $bound
#> # A tibble: 2 × 7
#>   analysis bound probability probability0     z `~risk difference at bound`
#>      <int> <chr>       <dbl>        <dbl> <dbl>                       <dbl>
#> 1        1 upper       0.616      0.00738  2.44                      0.0339
#> 2        2 upper       0.900      0.0250   2.00                      0.0232
#> # ℹ 1 more variable: `nominal p` <dbl>
#> 
#> $analysis
#> # A tibble: 2 × 8
#>   analysis     n    rd   rd0  info info0 info_frac info_frac0
#>      <int> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>
#> 1        1 3426. 0.038     0 5184. 5172.       0.7        0.7
#> 2        2 4894. 0.038     0 7406. 7388.       1          1  
#> 
```
