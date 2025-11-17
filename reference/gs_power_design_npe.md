# Group sequential design computation with non-constant effect and information.

The following two functions allow a non-constant treatment effect over
time, but also can be applied for the usual homogeneous effect size
designs. They require treatment effect and statistical information at
each analysis as well as a method of deriving bounds, such as spending.
Initial bound types supported are spending bounds and fixed bounds.
These routines enables two things not available in the gsDesign
package: 1) non-constant effect, 2) more flexibility in boundary
selection.

`gs_power_npe()` derives group sequential bounds and boundary crossing
probabilities for a design, given treatment effect and information at
each analysis and the method of deriving bounds, such as spending.

`gs_design_npe()` derives group sequential design size, bounds and
boundary crossing probabilities based on proportionate information and
effect size at analyses, as well as the method of deriving bounds, such
as spending.

The only differences in arguments between the two functions are the
`alpha` and `beta` parameters used in the `gs_design_npe()`.

## Usage

``` r
gs_design_npe(
  theta = 0.1,
  theta0 = 0,
  theta1 = theta,
  info = 1,
  info0 = NULL,
  info1 = NULL,
  info_scale = c("h0_h1_info", "h0_info", "h1_info"),
  alpha = 0.025,
  beta = 0.1,
  upper = gs_b,
  upar = qnorm(0.975),
  lower = gs_b,
  lpar = -Inf,
  test_upper = TRUE,
  test_lower = TRUE,
  binding = FALSE,
  r = 18,
  tol = 1e-06
)

gs_power_npe(
  theta = 0.1,
  theta0 = 0,
  theta1 = theta,
  info = 1,
  info0 = NULL,
  info1 = NULL,
  info_scale = c("h0_h1_info", "h0_info", "h1_info"),
  upper = gs_b,
  upar = qnorm(0.975),
  lower = gs_b,
  lpar = -Inf,
  test_upper = TRUE,
  test_lower = TRUE,
  binding = FALSE,
  r = 18,
  tol = 1e-06
)
```

## Arguments

- theta:

  Natural parameter for group sequential design representing expected
  cumulative drift at all analyses; used for power calculation. It can
  be a scalar (constant treatment effect) or a vector (non-constant
  treatment effect). The user must provide a value for `theta`.

- theta0:

  Natural parameter for null hypothesis. It can be a scalar (constant
  treatment effect) or a vector (non-constant treatment effect). The
  default is 0. If a value other than 0 is provided, it affects upper
  bound computation.

- theta1:

  Natural parameter for alternate hypothesis, if needed for lower bound
  computation. It can be a scalar (constant treatment effect) or a
  vector (non-constant treatment effect). The default is the same as
  `theta`, which yields the usual beta-spending. If set to 0, spending
  is 2-sided under the null hypothesis.

- info:

  Statistical information at all analyses for input `theta`. It is a
  vector of positive numbers in increasing order. The user must provide
  values corresponding to `theta`.

- info0:

  Statistical information under null hypothesis. It is a vector of all
  positive numbers with increasing order. Default is set to be the same
  as `info`. If `info0` is different than `info`, it impacts null
  hypothesis bound calculation.

- info1:

  Statistical information under hypothesis used for futility bound
  calculation. It is a vector of all positive numbers with increasing
  order. Default is set to be the same as `info`. If `info1` is
  different from `info`, it impacts futility bound calculation.

- info_scale:

  Information scale for calculation. Options are:

  - `"h0_h1_info"` (default): variance under both null and alternative
    hypotheses is used.

  - `"h0_info"`: variance under null hypothesis is used. This is often
    used for testing methods that use local alternatives, such as the
    Schoenfeld method.

  - `"h1_info"`: variance under alternative hypothesis is used.

- alpha:

  One-sided Type I error.

- beta:

  Type II error.

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

  Indicator of which analyses should include a lower bound; single value
  of `TRUE` (default) indicates all analyses; single value of `FALSE`
  indicated no lower bound; otherwise, a logical vector of the same
  length as `info` should indicate which analyses will have a lower
  bound.

- binding:

  Indicator of whether futility bound is binding; default of `FALSE` is
  recommended.

- r:

  Integer value controlling grid for numerical integration as in
  Jennison and Turnbull (2000); default is 18, range is 1 to 80. Larger
  values provide larger number of grid points and greater accuracy.
  Normally, `r` will not be changed by the user.

- tol:

  Tolerance parameter for boundary convergence (on Z-scale); normally
  not changed by the user.

## Value

A tibble with columns of

- `analysis`: analysis index.

- `bound`: either of value `"upper"` or `"lower"`, indicating the upper
  and lower bound.

- `z`: the Z-score bounds.

- `probability`: cumulative probability of crossing the bound at or
  before the analysis.

- `theta`: same as the input.

- `theta1`: same as the input.

- `info`: statistical information at each analysis.

  - If it is returned by `gs_power_npe`, the `info`, `info0`, `info1`
    are same as the input.

  - If it is returned by `gs_design_npe`, the `info`, `info0`, `info1`
    are changed by a constant scale factor. factor to ensure the design
    has power `1 - beta`.

- `info0`: statistical information under the null at each analysis.

- `info1`: statistical information under the alternative at each
  analysis.

- `info_frac`: information fraction at each analysis, i.e.,
  `info / max(info)`.

## Details

The bound specifications (`upper`, `lower`, `upar`, `lpar`) of
`gs_design_npe()` will be used to ensure Type I error and other boundary
properties are as specified. See the help file of
[`gs_spending_bound()`](https://merck.github.io/gsDesign2/reference/gs_spending_bound.md)
for details on spending function.

## Specification

The contents of this section are shown in PDF user manual only.

The contents of this section are shown in PDF user manual only.

## Author

Keaven Anderson <keaven_anderson@merck.com>

## Examples

``` r
library(gsDesign)

# Example 1 ----
# gs_design_npe with single analysis
# Lachin book p 71 difference of proportions example
pc <- .28 # Control response rate
pe <- .40 # Experimental response rate
p0 <- (pc + pe) / 2 # Ave response rate under H0

# Information per increment of 1 in sample size
info0 <- 1 / (p0 * (1 - p0) * 4)
info <- 1 / (pc * (1 - pc) * 2 + pe * (1 - pe) * 2)

# Result should round up to next even number = 652
# Divide information needed under H1 by information per patient added
gs_design_npe(theta = pe - pc, info = info, info0 = info0)
#> # A tibble: 1 × 10
#>   analysis bound     z probability probability0 theta  info info0 info1
#>      <dbl> <chr> <dbl>       <dbl>        <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1        1 upper  1.96         0.9        0.025  0.12  737.  725.  737.
#> # ℹ 1 more variable: info_frac <dbl>


# Example 2 ----
# gs_design_npe with with fixed bound
x <- gs_design_npe(
  alpha = 0.0125,
  theta = c(.1, .2, .3),
  info = (1:3) * 80,
  info0 = (1:3) * 80,
  upper = gs_b,
  upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF, alpha = 0.0125)$upper$bound,
  lower = gs_b,
  lpar = c(-1, 0, 0)
)
x
#> # A tibble: 6 × 10
#>   analysis bound     z probability probability0 theta info_frac  info info0
#>      <int> <chr> <dbl>       <dbl>        <dbl> <dbl>     <dbl> <dbl> <dbl>
#> 1        1 upper  4.17    0.000278    0.0000152   0.1     0.333  51.6  51.6
#> 2        1 lower -1       0.0429      0.159       0.1     0.333  51.6  51.6
#> 3        2 upper  2.85    0.208       0.00222     0.2     0.667 103.  103. 
#> 4        2 lower  0       0.0537      0.513       0.2     0.667 103.  103. 
#> 5        3 upper  2.26    0.900       0.0125      0.3     1     155.  155. 
#> 6        3 lower  0       0.0537      0.606       0.3     1     155.  155. 
#> # ℹ 1 more variable: info1 <dbl>

# Same upper bound; this represents non-binding Type I error and will total 0.025
gs_power_npe(
  theta = rep(0, 3),
  info = (x |> dplyr::filter(bound == "upper"))$info,
  upper = gs_b,
  upar = (x |> dplyr::filter(bound == "upper"))$z,
  lower = gs_b,
  lpar = rep(-Inf, 3)
)
#> # A tibble: 6 × 10
#>   analysis bound       z probability theta theta1 info_frac  info info0 info1
#>      <int> <chr>   <dbl>       <dbl> <dbl>  <dbl>     <dbl> <dbl> <dbl> <dbl>
#> 1        1 upper    4.17   0.0000152     0      0     0.333  51.6  51.6  51.6
#> 2        2 upper    2.85   0.00222       0      0     0.667 103.  103.  103. 
#> 3        3 upper    2.26   0.0125        0      0     1     155.  155.  155. 
#> 4        1 lower -Inf      0             0      0     0.333  51.6  51.6  51.6
#> 5        2 lower -Inf      0             0      0     0.667 103.  103.  103. 
#> 6        3 lower -Inf      0             0      0     1     155.  155.  155. 

# Example 3 ----
# gs_design_npe with spending bound
# Design with futility only at analysis 1; efficacy only at analyses 2, 3
# Spending bound for efficacy; fixed bound for futility
# NOTE: test_upper and test_lower DO NOT WORK with gs_b; must explicitly make bounds infinite
# test_upper and test_lower DO WORK with gs_spending_bound
gs_design_npe(
  theta = c(.1, .2, .3),
  info = (1:3) * 40,
  info0 = (1:3) * 40,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
  lower = gs_b,
  lpar = c(-1, -Inf, -Inf),
  test_upper = c(FALSE, TRUE, TRUE)
)
#> # A tibble: 6 × 10
#>   analysis bound       z probability probability0 theta info_frac  info info0
#>      <int> <chr>   <dbl>       <dbl>        <dbl> <dbl>     <dbl> <dbl> <dbl>
#> 1        1 upper  Inf         0           0         0.1     0.333  44.6  44.6
#> 2        1 lower   -1         0.0477      0.159     0.1     0.333  44.6  44.6
#> 3        2 upper    2.51      0.267       0.00605   0.2     0.667  89.1  89.1
#> 4        2 lower -Inf         0.0477      0.159     0.2     0.667  89.1  89.1
#> 5        3 upper    1.99      0.900       0.0249    0.3     1     134.  134. 
#> 6        3 lower -Inf         0.0477      0.159     0.3     1     134.  134. 
#> # ℹ 1 more variable: info1 <dbl>

# one can try `info_scale = "h1_info"` or `info_scale = "h0_info"` here
gs_design_npe(
  theta = c(.1, .2, .3),
  info = (1:3) * 40,
  info0 = (1:3) * 30,
  info_scale = "h1_info",
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
  lower = gs_b,
  lpar = c(-1, -Inf, -Inf),
  test_upper = c(FALSE, TRUE, TRUE)
)
#> # A tibble: 6 × 10
#>   analysis bound       z probability probability0 theta info_frac  info info0
#>      <int> <chr>   <dbl>       <dbl>        <dbl> <dbl>     <dbl> <dbl> <dbl>
#> 1        1 upper  Inf         0           0         0.1     0.333  44.6  44.6
#> 2        1 lower   -1         0.0477      0.159     0.1     0.333  44.6  44.6
#> 3        2 upper    2.51      0.267       0.00605   0.2     0.667  89.1  89.1
#> 4        2 lower -Inf         0.0477      0.159     0.2     0.667  89.1  89.1
#> 5        3 upper    1.99      0.900       0.0249    0.3     1     134.  134. 
#> 6        3 lower -Inf         0.0477      0.159     0.3     1     134.  134. 
#> # ℹ 1 more variable: info1 <dbl>

# Example 4 ----
# gs_design_npe with spending function bounds
# 2-sided asymmetric bounds
# Lower spending based on non-zero effect
gs_design_npe(
  theta = c(.1, .2, .3),
  info = (1:3) * 40,
  info0 = (1:3) * 30,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
)
#> # A tibble: 6 × 10
#>   analysis bound      z probability probability0 theta info_frac  info info0
#>      <int> <chr>  <dbl>       <dbl>        <dbl> <dbl>     <dbl> <dbl> <dbl>
#> 1        1 upper  3.71     0.000145     0.000104   0.1     0.333  43.5  32.7
#> 2        1 lower -1.34     0.0139       0.0909     0.1     0.333  43.5  32.7
#> 3        2 upper  2.51     0.258        0.00605    0.2     0.667  87.1  65.3
#> 4        2 lower  0.150    0.0460       0.562      0.2     0.667  87.1  65.3
#> 5        3 upper  1.99     0.900        0.0249     0.3     1     131.   98.0
#> 6        3 lower  2.00     0.0908       0.976      0.3     1     131.   98.0
#> # ℹ 1 more variable: info1 <dbl>

# Example 5 ----
# gs_design_npe with two-sided symmetric spend, O'Brien-Fleming spending
# Typically, 2-sided bounds are binding
xx <- gs_design_npe(
  theta = c(.1, .2, .3),
  info = (1:3) * 40,
  binding = TRUE,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
)
xx
#> # A tibble: 6 × 10
#>   analysis bound      z probability probability0 theta info_frac  info info0
#>      <int> <chr>  <dbl>       <dbl>        <dbl> <dbl>     <dbl> <dbl> <dbl>
#> 1        1 upper  3.71     0.00104      0.000104   0.1     0.333  39.8  39.8
#> 2        1 lower -3.08     0.000104     0.00104    0.1     0.333  39.8  39.8
#> 3        2 upper  2.51     0.233        0.00605    0.2     0.667  79.5  79.5
#> 4        2 lower -0.728    0.00605      0.233      0.2     0.667  79.5  79.5
#> 5        3 upper  1.99     0.900        0.0250     0.3     1     119.  119. 
#> 6        3 lower  1.28     0.0250       0.900      0.3     1     119.  119. 
#> # ℹ 1 more variable: info1 <dbl>

# Re-use these bounds under alternate hypothesis
# Always use binding = TRUE for power calculations
gs_power_npe(
  theta = c(.1, .2, .3),
  info = (1:3) * 40,
  binding = TRUE,
  upper = gs_b,
  lower = gs_b,
  upar = (xx |> dplyr::filter(bound == "upper"))$z,
  lpar = -(xx |> dplyr::filter(bound == "upper"))$z
)
#> # A tibble: 6 × 10
#>   analysis bound     z probability theta theta1 info_frac  info info0 info1
#>      <int> <chr> <dbl>       <dbl> <dbl>  <dbl>     <dbl> <dbl> <dbl> <dbl>
#> 1        1 upper  3.71  0.00104      0.1    0.1     0.333    40    40    40
#> 2        2 upper  2.51  0.235        0.2    0.2     0.667    80    80    80
#> 3        3 upper  1.99  0.902        0.3    0.3     1       120   120   120
#> 4        1 lower -3.71  0.00000704   0.1    0.1     0.333    40    40    40
#> 5        2 lower -2.51  0.0000151    0.2    0.2     0.667    80    80    80
#> 6        3 lower -1.99  0.0000151    0.3    0.3     1       120   120   120

# Example 6 ----
# Default of gs_power_npe (single analysis; Type I error controlled)
gs_power_npe(theta = 0) |> dplyr::filter(bound == "upper")
#> # A tibble: 1 × 10
#>   analysis bound     z probability theta theta1 info_frac  info info0 info1
#>      <int> <chr> <dbl>       <dbl> <dbl>  <dbl>     <dbl> <dbl> <dbl> <dbl>
#> 1        1 upper  1.96      0.0250     0      0         1     1     1     1

# Example 7 ----
# gs_power_npe with fixed bound
gs_power_npe(
  theta = c(.1, .2, .3),
  info = (1:3) * 40,
  upper = gs_b,
  upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
  lower = gs_b,
  lpar = c(-1, 0, 0)
)
#> # A tibble: 6 × 10
#>   analysis bound     z probability theta theta1 info_frac  info info0 info1
#>      <int> <chr> <dbl>       <dbl> <dbl>  <dbl>     <dbl> <dbl> <dbl> <dbl>
#> 1        1 upper  3.71     0.00104   0.1    0.1     0.333    40    40    40
#> 2        2 upper  2.51     0.235     0.2    0.2     0.667    80    80    80
#> 3        3 upper  1.99     0.869     0.3    0.3     1       120   120   120
#> 4        1 lower -1        0.0513    0.1    0.1     0.333    40    40    40
#> 5        2 lower  0        0.0715    0.2    0.2     0.667    80    80    80
#> 6        3 lower  0        0.0715    0.3    0.3     1       120   120   120

# Same fixed efficacy bounds, no futility bound (i.e., non-binding bound), null hypothesis
gs_power_npe(
  theta = rep(0, 3),
  info = (1:3) * 40,
  upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
  lpar = rep(-Inf, 3)
) |>
  dplyr::filter(bound == "upper")
#> # A tibble: 3 × 10
#>   analysis bound     z probability theta theta1 info_frac  info info0 info1
#>      <int> <chr> <dbl>       <dbl> <dbl>  <dbl>     <dbl> <dbl> <dbl> <dbl>
#> 1        1 upper  3.71    0.000104     0      0     0.333    40    40    40
#> 2        2 upper  2.51    0.00605      0      0     0.667    80    80    80
#> 3        3 upper  1.99    0.0250       0      0     1       120   120   120

# Example 8 ----
# gs_power_npe with fixed bound testing futility only at analysis 1; efficacy only at analyses 2, 3
gs_power_npe(
  theta = c(.1, .2, .3),
  info = (1:3) * 40,
  upper = gs_b,
  upar = c(Inf, 3, 2),
  lower = gs_b,
  lpar = c(qnorm(.1), -Inf, -Inf)
)
#> # A tibble: 6 × 10
#>   analysis bound       z probability theta theta1 info_frac  info info0 info1
#>      <int> <chr>   <dbl>       <dbl> <dbl>  <dbl>     <dbl> <dbl> <dbl> <dbl>
#> 1        1 upper  Inf         0        0.1    0.1     0.333    40    40    40
#> 2        2 upper    3         0.113    0.2    0.2     0.667    80    80    80
#> 3        3 upper    2         0.887    0.3    0.3     1       120   120   120
#> 4        1 lower   -1.28      0.0278   0.1    0.1     0.333    40    40    40
#> 5        2 lower -Inf         0.0278   0.2    0.2     0.667    80    80    80
#> 6        3 lower -Inf         0.0278   0.3    0.3     1       120   120   120

# Example 9 ----
# gs_power_npe with spending function bounds
# Lower spending based on non-zero effect
gs_power_npe(
  theta = c(.1, .2, .3),
  info = (1:3) * 40,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
)
#> # A tibble: 6 × 10
#>   analysis bound       z probability theta theta1 info_frac  info info0 info1
#>      <int> <chr>   <dbl>       <dbl> <dbl>  <dbl>     <dbl> <dbl> <dbl> <dbl>
#> 1        1 upper  3.71       0.00104   0.1    0.1     0.333    40    40    40
#> 2        2 upper  2.51       0.235     0.2    0.2     0.667    80    80    80
#> 3        3 upper  1.99       0.883     0.3    0.3     1       120   120   120
#> 4        1 lower -1.36       0.0230    0.1    0.1     0.333    40    40    40
#> 5        2 lower  0.0726     0.0552    0.2    0.2     0.667    80    80    80
#> 6        3 lower  1.86       0.100     0.3    0.3     1       120   120   120

# Same bounds, but power under different theta
gs_power_npe(
  theta = c(.15, .25, .35),
  info = (1:3) * 40,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
)
#> # A tibble: 6 × 10
#>   analysis bound      z probability theta theta1 info_frac  info info0 info1
#>      <int> <chr>  <dbl>       <dbl> <dbl>  <dbl>     <dbl> <dbl> <dbl> <dbl>
#> 1        1 upper  3.71      0.00288  0.15   0.15     0.333    40    40    40
#> 2        2 upper  2.51      0.391    0.25   0.25     0.667    80    80    80
#> 3        3 upper  1.99      0.931    0.35   0.35     1       120   120   120
#> 4        1 lower -1.05      0.0230   0.15   0.15     0.333    40    40    40
#> 5        2 lower  0.520     0.0552   0.25   0.25     0.667    80    80    80
#> 6        3 lower  2.41      0.100    0.35   0.35     1       120   120   120

# Example 10 ----
# gs_power_npe with two-sided symmetric spend, O'Brien-Fleming spending
# Typically, 2-sided bounds are binding
x <- gs_power_npe(
  theta = rep(0, 3),
  info = (1:3) * 40,
  binding = TRUE,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
)

# Re-use these bounds under alternate hypothesis
# Always use binding = TRUE for power calculations
gs_power_npe(
  theta = c(.1, .2, .3),
  info = (1:3) * 40,
  binding = TRUE,
  upar = (x |> dplyr::filter(bound == "upper"))$z,
  lpar = -(x |> dplyr::filter(bound == "upper"))$z
)
#> # A tibble: 6 × 10
#>   analysis bound     z probability theta theta1 info_frac  info info0 info1
#>      <int> <chr> <dbl>       <dbl> <dbl>  <dbl>     <dbl> <dbl> <dbl> <dbl>
#> 1        1 upper  3.71  0.00104      0.1    0.1     0.333    40    40    40
#> 2        2 upper  2.51  0.235        0.2    0.2     0.667    80    80    80
#> 3        3 upper  1.99  0.902        0.3    0.3     1       120   120   120
#> 4        1 lower -3.71  0.00000704   0.1    0.1     0.333    40    40    40
#> 5        2 lower -2.51  0.0000151    0.2    0.2     0.667    80    80    80
#> 6        3 lower -1.99  0.0000151    0.3    0.3     1       120   120   120

# Example 11 ----
# Different values of `r` and `tol` lead to different numerical accuracy
# Larger `r` and smaller `tol` give better accuracy, but leads to slow computation
n_analysis <- 5
gs_power_npe(
  theta = 0.1,
  info = 1:n_analysis,
  info0 = 1:n_analysis,
  info1 = NULL,
  info_scale = "h0_info",
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
  lower = gs_b,
  lpar = -rep(Inf, n_analysis),
  test_upper = TRUE,
  test_lower = FALSE,
  binding = FALSE,
  # Try different combinations of (r, tol) with
  # r in 6, 18, 24, 30, 35, 40, 50, 60, 70, 80, 90, 100
  # tol in 1e-6, 1e-12
  r = 6,
  tol = 1e-6
)
#> # A tibble: 10 × 10
#>    analysis bound       z probability theta theta1 info_frac  info info0 info1
#>       <int> <chr>   <dbl>       <dbl> <dbl>  <dbl>     <dbl> <int> <int> <int>
#>  1        1 upper    4.88 0.000000890   0.1    0.1       0.2     1     1     1
#>  2        2 upper    3.36 0.000650      0.1    0.1       0.4     2     2     2
#>  3        3 upper    2.68 0.00627       0.1    0.1       0.6     3     3     3
#>  4        4 upper    2.29 0.0200        0.1    0.1       0.8     4     4     4
#>  5        5 upper    2.03 0.0408        0.1    0.1       1       5     5     5
#>  6        1 lower -Inf    0             0.1    0.1       0.2     1     1     1
#>  7        2 lower -Inf    0             0.1    0.1       0.4     2     2     2
#>  8        3 lower -Inf    0             0.1    0.1       0.6     3     3     3
#>  9        4 lower -Inf    0             0.1    0.1       0.8     4     4     4
#> 10        5 lower -Inf    0             0.1    0.1       1       5     5     5
```
