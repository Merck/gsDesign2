# Simple conditional power computation with non-constant effect size

Simple conditional power computation with non-constant effect size

## Usage

``` r
gs_cp_simple(x = NULL, theta = NULL, i = 1, zi = NULL)
```

## Arguments

- x:

  An object of type gsDesign2.

- theta:

  Optional numeric vector of natural parameter for treatment effects
  from analysis \\i\\ through final analysis \\K\\. If `NULL`, values
  are taken from `x$analysis$theta`.

- i:

  Integer index for current analysis. Default is 1.

- zi:

  Numeric scalar z-value observed at analysis \\i\\.

## Value

A numeric vector with the conditional power \\P(Z_j \> b_j \mid Z_i =
z_i)\\ for \\j = i+1, ..., K\\.

## Details

Suppose there are \\K\\ analyses. Let \\Z_i\\ and \\Z_j\\ be the
Z-statistics at a current analysis \\i\\ and a future analysis \\j\\,
respectively. Let's denote the statistical information at the \\i\\-th
analysis as \\I_i\\. We further assume \\Z_i\\ and \\Z_j\\ are bivariate
normal under standard group sequential assumptions with independent
increments, then \$\$E(Z_i) = \theta_i\sqrt{I_i}\$\$ \$\$Var(Z_i) =
1/I_i\$\$ \$\$Cov(Z_i, Z_j) = t \equiv I_i/I_j\$\$ See
https://merck.github.io/gsDesign2/articles/story-npe-background.html for
assumption details. Returned value is \$\$P(Z_j \> b_j \mid Z_i = z_i) =
1 - \Phi\left(\frac{b_j - \sqrt{t}z_i - \sqrt{I_j}(\theta_j -
\theta_i\sqrt{t})}{\sqrt{1 - t}}\right)\$\$ where \\b_j\\ is the
efficacy bound at analysis \\j\\, \\z_i\\ is the observed Z-value at
analysis \\i\\, and \\\theta_i\\ is the natural parameter for treatment
effect at analysis \\i\\.

## Examples

``` r
library(gsDesign2)
library(gsDesign)
# Calculate conditional power with optional user-input theta (if NULL, will come from the input design)
alpha <- 0.025
beta <- 0.1
ratio <- 1
enroll_rate <- define_enroll_rate(
  duration = c(2, 2, 10),
  rate = (1:3) / 3)
fail_rate <- define_fail_rate(
  duration = Inf, fail_rate = log(2) / 9,
  hr = 0.6, dropout_rate = .0001)
analysis_time <- c(12, 24, 36)
ratio <- 1
upper <- gs_spending_bound
lower <- gs_b
upar <- list(sf = sfLDOF, total_spend = alpha)
lpar <- rep(-Inf, 3)
x <- gs_design_ahr(
  enroll_rate = enroll_rate, fail_rate = fail_rate,
  alpha = alpha, beta = beta, ratio = ratio,
  info_scale = "h0_h1_info",
  info_frac = NULL,
  analysis_time = c(12, 24, 36),
  upper = upper, upar = upar, test_upper = TRUE,
  lower = lower, lpar = lpar, test_lower = FALSE,
) |>
  to_integer()

# theta is NULL case
gs_cp_simple(x = x, theta = NULL, i = 1, zi = 1.5)
#> [1] 0.6914911 0.9129447

# User-input theta case
gs_cp_simple(x = x, theta = c(0.1, 0.2, 0.3), i = 1, zi = 1.5)
#> [1] 0.2562896 0.7194958
```
