# Conditional power computation with non-constant effect size for non-/crossing an upper boundary at analysis j given observed Z value at analysis i

Conditional power computation with non-constant effect size for
non-/crossing an upper boundary at analysis j given observed Z value at
analysis i

## Usage

``` r
gs_cp(x = NULL, theta = NULL, i = 1, zi = NULL)
```

## Arguments

- x:

  An object of type gsDesign2.

- theta:

  Optional numeric vector with length \\j-i+1\\, which specifies the
  natural parameter for treatment effect of interim analysis \\i\\
  through analysis \\j\\. The default is `NULL`.

- i:

  Index of current analysis, with default of 1.

- zi:

  Numeric scalar z-value observed at analysis \\i\\.

## Value

A list with the following elements:

- `prob_alpha`:

  A numeric vector of \\(\alpha\_{i,i+1}, \ldots, \alpha\_{i,j-1},
  \alpha\_{i,j})\\, where \\\alpha\_{i,j} = P(\\Z_j \geq b_j\\ \\
  \\\cap\_{m=i+1}^{j-1} a_m \leq Z_m \< b_m\\ \mid Z_i = z_i)\\.

- `prob_alpha_plus`:

  A numeric vector of \\(\alpha^+\_{i,i+1}, \ldots, \alpha^+\_{i,j-1},
  \alpha^+\_{i,j})\\, where \\\alpha^+\_{i,j} = P(\\Z_j \geq b_j\\ \\
  \\\cap\_{m=i+1}^{j-1} Z_m \< b_m\\ \mid Z_i = z_i)\\.

- `prob_beta`:

  A numeric vector of \\(\beta\_{i,i+1}, \ldots, \beta\_{i,j-1},
  \beta\_{i,j})\\, where \\\beta\_{i,j} = P(\\Z_j \leq b_j\\ \\
  \\\cap\_{m=i+1}^{j-1} a_m \leq Z_m \< b_m\\ \mid Z_i = z_i)\\.

## Details

We assume \\Z_i, i = 1, ..., K\\ are the z-statistics at an interim
analysis i, respectively. We assume further \\Z_i, i = 1, ..., K\\
follows multivariate normal distribution \$\$E(Z_i) =
\theta_i\sqrt{I_i}\$\$ \$\$Cov(Z_i, Z_j) = I_i/I_j\$\$. See
https://merck.github.io/gsDesign2/articles/story-npe-background.html for
assumption details.

The returned value is list of \$\$P(\\Z_j \geq b_j\\ \\
\\\cap\_{m=i+1}^{j-1} a_m \leq Z_m \< b_m\\ \mid Z_i = z_i).\$\$
\$\$P(\\Z_j \geq b_j\\ \\ \\\cap\_{m=i+1}^{j-1} Z_m \< b_m\\ \mid Z_i =
z_i).\$\$ \$\$P(\\Z_j \leq b_j\\ \\ \\\cap\_{m=i+1}^{j-1} a_m \leq Z_m
\< b_m\\ \mid Z_i = z_i).\$\$

## Examples

``` r
library(gsDesign2)
library(gsDesign)
#> 
#> Attaching package: ‘gsDesign’
#> The following objects are masked from ‘package:gsDesign2’:
#> 
#>     as_gt, as_rtf
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(mvtnorm)
# Example 1
# original design ----
enroll_rate <- define_enroll_rate(duration = c(2, 2, 2, 18),
                                  rate = c(1, 2, 3, 4))
fail_rate <- define_fail_rate(duration = c(3, Inf),
                              fail_rate = log(2) / 10,
                              dropout_rate = 0.001,
                              hr = c(1, 0.7))
x <- gs_design_ahr(enroll_rate = enroll_rate, fail_rate = fail_rate,
                   alpha = 0.025, beta = 0.1, ratio = 1,
                   info_frac = c(0.4, 0.6, 0.8, 1), analysis_time = 30,
                   binding = FALSE,
                   upper = gs_spending_bound,
                   upar = list(sf = sfLDOF, total_spend = 0.025, param = NULL),
                   lower = gs_spending_bound,
                   lpar = list(sf = sfLDOF, total_spend = 0.1),
                   h1_spending = TRUE,
                   test_lower = TRUE,
                   info_scale = "h0_h1_info") |> to_integer()

# calculate conditional power
# case 1: currently at IA1, compute conditional power at IA2, IA3 and FA, with default theta = NULL
gs_cp(x = x, i = 1, zi = -gsDesign::hrn2z(hr = 0.8, n = 150+180, ratio = 1))
#> $prob_alpha
#> [1] 0.4092021 0.4521861 0.1219315
#> 
#> $prob_alpha_plus
#> [1] 0.4092021 0.4521865 0.1232153
#> 
#> $prob_beta
#> [1] 0.5907979 0.0000000 0.0000000
#> 

# case 2: currently at IA1, compute conditional power at IA2, IA3 and FA, with user-input theta
gs_cp(x = x, theta = c(0.15, 0.2, 0.25, 0.3), i = 1, zi = -gsDesign::hrn2z(hr = 0.8, n = 150+180, ratio = 1))
#> $prob_alpha
#> [1] 0.53387369 0.43584552 0.02968923
#> 
#> $prob_alpha_plus
#> [1] 0.53387369 0.43584635 0.02989685
#> 
#> $prob_beta
#> [1] 0.4661263 0.0000000 0.0000000
#> 
```
