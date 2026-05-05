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
#>   analysis bound         z  probability probability0 theta info_frac      info
#> 1        1 upper  4.170845 0.0002775712 1.517362e-05   0.1 0.3333333  51.58274
#> 2        1 lower -1.000000 0.0428790458 1.586553e-01   0.1 0.3333333  51.58274
#> 3        2 upper  2.845813 0.2076724793 2.220491e-03   0.2 0.6666667 103.16548
#> 4        2 lower  0.000000 0.0537297034 5.125858e-01   0.2 0.6666667 103.16548
#> 5        3 upper  2.263724 0.9000056035 1.248248e-02   0.3 1.0000000 154.74822
#> 6        3 lower  0.000000 0.0537312146 6.063484e-01   0.3 1.0000000 154.74822
#>       info0     info1
#> 1  51.58274  51.58274
#> 2  51.58274  51.58274
#> 3 103.16548 103.16548
#> 4 103.16548 103.16548
#> 5 154.74822 154.74822
#> 6 154.74822 154.74822

# Same upper bound; this represents non-binding Type I error and will total 0.025
gs_power_npe(
  theta = rep(0, 3),
  info = (x |> dplyr::filter(bound == "upper"))$info,
  upper = gs_b,
  upar = (x |> dplyr::filter(bound == "upper"))$z,
  lower = gs_b,
  lpar = rep(-Inf, 3)
)
#>   analysis bound        z  probability theta theta1 info_frac      info
#> 1        1 upper 4.170845 1.517362e-05     0      0 0.3333333  51.58274
#> 2        2 upper 2.845813 2.220501e-03     0      0 0.6666667 103.16548
#> 3        3 upper 2.263724 1.250024e-02     0      0 1.0000000 154.74822
#> 4        1 lower     -Inf 0.000000e+00     0      0 0.3333333  51.58274
#> 5        2 lower     -Inf 0.000000e+00     0      0 0.6666667 103.16548
#> 6        3 lower     -Inf 0.000000e+00     0      0 1.0000000 154.74822
#>       info0     info1
#> 1  51.58274  51.58274
#> 2 103.16548 103.16548
#> 3 154.74822 154.74822
#> 4  51.58274  51.58274
#> 5 103.16548 103.16548
#> 6 154.74822 154.74822

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
#>   analysis bound         z probability probability0 theta info_frac      info
#> 1        1 upper       Inf   0.0000000  0.000000000   0.1 0.3333333  44.55550
#> 2        1 lower -1.000000   0.0477076  0.158655254   0.1 0.3333333  44.55550
#> 3        2 upper  2.509315   0.2670769  0.006048281   0.2 0.6666667  89.11101
#> 4        2 lower      -Inf   0.0477076  0.158655254   0.2 0.6666667  89.11101
#> 5        3 upper  1.992888   0.8999998  0.024947293   0.3 1.0000000 133.66651
#> 6        3 lower      -Inf   0.0477076  0.158655254   0.3 1.0000000 133.66651
#>       info0     info1
#> 1  44.55550  44.55550
#> 2  44.55550  44.55550
#> 3  89.11101  89.11101
#> 4  89.11101  89.11101
#> 5 133.66651 133.66651
#> 6 133.66651 133.66651

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
#>   analysis bound         z probability probability0 theta info_frac      info
#> 1        1 upper       Inf   0.0000000  0.000000000   0.1 0.3333333  44.55550
#> 2        1 lower -1.000000   0.0477076  0.158655254   0.1 0.3333333  44.55550
#> 3        2 upper  2.509315   0.2670769  0.006048281   0.2 0.6666667  89.11101
#> 4        2 lower      -Inf   0.0477076  0.158655254   0.2 0.6666667  89.11101
#> 5        3 upper  1.992888   0.8999998  0.024947293   0.3 1.0000000 133.66651
#> 6        3 lower      -Inf   0.0477076  0.158655254   0.3 1.0000000 133.66651
#>       info0     info1
#> 1  44.55550  44.55550
#> 2  44.55550  44.55550
#> 3  89.11101  89.11101
#> 4  89.11101  89.11101
#> 5 133.66651 133.66651
#> 6 133.66651 133.66651

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
#>   analysis bound          z  probability probability0 theta info_frac      info
#> 1        1 upper  3.7103029 0.0001448358 0.0001035057   0.1 0.3333333  43.54986
#> 2        1 lower -1.3350350 0.0138507196 0.0909324478   0.1 0.3333333  43.54986
#> 3        2 upper  2.5114338 0.2584942092 0.0060483766   0.2 0.6666667  87.09972
#> 4        2 lower  0.1503316 0.0459829116 0.5623935172   0.2 0.6666667  87.09972
#> 5        3 upper  1.9930511 0.8999994407 0.0249432388   0.3 1.0000000 130.64958
#> 6        3 lower  2.0018760 0.0908269980 0.9755024218   0.3 1.0000000 130.64958
#>      info0     info1
#> 1 32.66239  43.54986
#> 2 32.66239  43.54986
#> 3 65.32479  87.09972
#> 4 65.32479  87.09972
#> 5 97.98718 130.64958
#> 6 97.98718 130.64958

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
#>   analysis bound          z  probability probability0 theta info_frac      info
#> 1        1 upper  3.7103029 0.0010356714 0.0001035057   0.1 0.3333333  39.75242
#> 2        1 lower -3.0798077 0.0001035057 0.0010356714   0.1 0.3333333  39.75242
#> 3        2 upper  2.5114338 0.2332809784 0.0060483891   0.2 0.6666667  79.50483
#> 4        2 lower -0.7281241 0.0060483891 0.2332809784   0.2 0.6666667  79.50483
#> 5        3 upper  1.9930482 0.9000060668 0.0250000000   0.3 1.0000000 119.25725
#> 6        3 lower  1.2831008 0.0250000000 0.9000060668   0.3 1.0000000 119.25725
#>       info0     info1
#> 1  39.75242  39.75242
#> 2  39.75242  39.75242
#> 3  79.50483  79.50483
#> 4  79.50483  79.50483
#> 5 119.25725 119.25725
#> 6 119.25725 119.25725

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
#>   analysis bound         z  probability theta theta1 info_frac info info0 info1
#> 1        1 upper  3.710303 1.042508e-03   0.1    0.1 0.3333333   40    40    40
#> 2        2 upper  2.511434 2.349812e-01   0.2    0.2 0.6666667   80    80    80
#> 3        3 upper  1.993048 9.020716e-01   0.3    0.3 1.0000000  120   120   120
#> 4        1 lower -3.710303 7.035242e-06   0.1    0.1 0.3333333   40    40    40
#> 5        2 lower -2.511434 1.510031e-05   0.2    0.2 0.6666667   80    80    80
#> 6        3 lower -1.993048 1.512598e-05   0.3    0.3 1.0000000  120   120   120

# Example 6 ----
# Default of gs_power_npe (single analysis; Type I error controlled)
gs_power_npe(theta = 0) |> dplyr::filter(bound == "upper")
#>   analysis bound        z probability theta theta1 info_frac info info0 info1
#> 1        1 upper 1.959964       0.025     0      0         1    1     1     1

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
#>   analysis bound         z probability theta theta1 info_frac info info0 info1
#> 1        1 upper  3.710303 0.001042508   0.1    0.1 0.3333333   40    40    40
#> 2        2 upper  2.511427 0.234899008   0.2    0.2 0.6666667   80    80    80
#> 3        3 upper  1.993048 0.868689026   0.3    0.3 1.0000000  120   120   120
#> 4        1 lower -1.000000 0.051291779   0.1    0.1 0.3333333   40    40    40
#> 5        2 lower  0.000000 0.071509147   0.2    0.2 0.6666667   80    80    80
#> 6        3 lower  0.000000 0.071522125   0.3    0.3 1.0000000  120   120   120

# Same fixed efficacy bounds, no futility bound (i.e., non-binding bound), null hypothesis
gs_power_npe(
  theta = rep(0, 3),
  info = (1:3) * 40,
  upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
  lpar = rep(-Inf, 3)
) |>
  dplyr::filter(bound == "upper")
#>   analysis bound        z  probability theta theta1 info_frac info info0 info1
#> 1        1 upper 3.710303 0.0001035057     0      0 0.3333333   40    40    40
#> 2        2 upper 2.511427 0.0060485045     0      0 0.6666667   80    80    80
#> 3        3 upper 1.993048 0.0250002354     0      0 1.0000000  120   120   120

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
#>   analysis bound         z probability theta theta1 info_frac info info0 info1
#> 1        1 upper       Inf  0.00000000   0.1    0.1 0.3333333   40    40    40
#> 2        2 upper  3.000000  0.11291848   0.2    0.2 0.6666667   80    80    80
#> 3        3 upper  2.000000  0.88742529   0.3    0.3 1.0000000  120   120   120
#> 4        1 lower -1.281552  0.02780962   0.1    0.1 0.3333333   40    40    40
#> 5        2 lower      -Inf  0.02780962   0.2    0.2 0.6666667   80    80    80
#> 6        3 lower      -Inf  0.02780962   0.3    0.3 1.0000000  120   120   120

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
#>   analysis bound           z probability theta theta1 info_frac info info0
#> 1        1 upper  3.71030287 0.001042508   0.1    0.1 0.3333333   40    40
#> 2        2 upper  2.51143381 0.234968841   0.2    0.2 0.6666667   80    80
#> 3        3 upper  1.99305108 0.882718886   0.3    0.3 1.0000000  120   120
#> 4        1 lower -1.36250263 0.023023722   0.1    0.1 0.3333333   40    40
#> 5        2 lower  0.07264138 0.055155914   0.2    0.2 0.6666667   80    80
#> 6        3 lower  1.85908660 0.100000000   0.3    0.3 1.0000000  120   120
#>   info1
#> 1    40
#> 2    80
#> 3   120
#> 4    40
#> 5    80
#> 6   120

# Same bounds, but power under different theta
gs_power_npe(
  theta = c(.15, .25, .35),
  info = (1:3) * 40,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
)
#>   analysis bound         z probability theta theta1 info_frac info info0 info1
#> 1        1 upper  3.710303 0.002875773  0.15   0.15 0.3333333   40    40    40
#> 2        2 upper  2.511434 0.391439113  0.25   0.25 0.6666667   80    80    80
#> 3        3 upper  1.993051 0.931364287  0.35   0.35 1.0000000  120   120   120
#> 4        1 lower -1.046275 0.023023722  0.15   0.15 0.3333333   40    40    40
#> 5        2 lower  0.519855 0.055155914  0.25   0.25 0.6666667   80    80    80
#> 6        3 lower  2.408073 0.100000000  0.35   0.35 1.0000000  120   120   120

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
#>   analysis bound         z  probability theta theta1 info_frac info info0 info1
#> 1        1 upper  3.710303 1.042508e-03   0.1    0.1 0.3333333   40    40    40
#> 2        2 upper  2.511434 2.349812e-01   0.2    0.2 0.6666667   80    80    80
#> 3        3 upper  1.993051 9.020711e-01   0.3    0.3 1.0000000  120   120   120
#> 4        1 lower -3.710303 7.035242e-06   0.1    0.1 0.3333333   40    40    40
#> 5        2 lower -2.511434 1.510031e-05   0.2    0.2 0.6666667   80    80    80
#> 6        3 lower -1.993051 1.512598e-05   0.3    0.3 1.0000000  120   120   120

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
#>    analysis bound        z  probability theta theta1 info_frac info info0 info1
#> 1         1 upper 4.876885 8.901578e-07   0.1    0.1       0.2    1     1     1
#> 2         2 upper 3.359204 6.500358e-04   0.1    0.1       0.4    2     2     2
#> 3         3 upper 2.681324 6.271330e-03   0.1    0.1       0.6    3     3     3
#> 4         4 upper 2.290483 2.003375e-02   0.1    0.1       0.8    4     4     4
#> 5         5 upper 2.031517 4.083845e-02   0.1    0.1       1.0    5     5     5
#> 6         1 lower     -Inf 0.000000e+00   0.1    0.1       0.2    1     1     1
#> 7         2 lower     -Inf 0.000000e+00   0.1    0.1       0.4    2     2     2
#> 8         3 lower     -Inf 0.000000e+00   0.1    0.1       0.6    3     3     3
#> 9         4 lower     -Inf 0.000000e+00   0.1    0.1       0.8    4     4     4
#> 10        5 lower     -Inf 0.000000e+00   0.1    0.1       1.0    5     5     5
```
