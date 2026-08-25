# Conditional power computation with non-constant effect size for (non-)crossing an upper/lower boundary at future analyses given Z value at analysis i

Conditional power computation with non-constant effect size for
(non-)crossing an upper/lower boundary at future analyses given Z value
at analysis i

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

A list of conditional powers:

- `prob_alpha` is a numeric vector of (\\\alpha\_{i,i+1}, ...,
  \alpha\_{i,j-1}, \alpha\_{i,j}\\), where

  - for \\j = i+1\\, \\\alpha\_{i,j} = P(\\Z_j \geq b_j\\ \mid Z_i =
    z_i)\\;

  - for \\j \> i+1\\, \\\alpha\_{i,j} = P(\\Z_j \geq b_j\\ \\
    \\\cap\_{m=i+1}^{j-1} a_m \leq Z_m \< b_m\\ \mid Z_i = z_i)\\.

- `prob_alpha_plus` is a numeric vector of (\\\alpha^+\_{i,i+1}, ...,
  \alpha^+\_{i,j-1}, \alpha^+\_{i,j}\\), where

  - for \\j = i+1\\, \\\alpha^+\_{i,j} = P(\\Z_j \geq b_j\\ \mid Z_i =
    z_i)\\;

  - for \\j \> i+1\\, \\\alpha^+\_{i,j} = P(\\Z_j \geq b_j\\ \\
    \\\cap\_{m=i+1}^{j-1} Z_m \< b_m\\ \mid Z_i = z_i)\\.

- `prob_beta` is a numeric vector of (\\\beta\_{i,i+1}, ...,
  \beta\_{i,j-1}, \beta\_{i,j}\\), where

  - for \\j = i+1\\, \\\beta\_{i,j} = P(\\Z_j \leq a_j\\ \mid Z_i =
    z_i)\\;

  - for \\j \> i+1\\, \\\beta\_{i,j} = P(\\Z_j \leq a_j\\ \\
    \\\cap\_{m=i+1}^{j-1} a_m \leq Z_m \< b_m\\ \mid Z_i = z_i)\\.

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

enroll_rate <- define_enroll_rate(duration = c(2, 2, 2, 18),
                                  rate = c(1, 2, 3, 4))
fail_rate <- define_fail_rate(duration = c(3, Inf),
                              fail_rate = log(2) / 10,
                              dropout_rate = 0.001,
                              hr = c(1, 0.7))

# Example 1: futility is tested at all analyses
# original design ----
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
# case 1: currently at IA1, compute conditional power at IA2, IA3 and FA, 
# with default theta = NULL
gs_cp(x = x, i = 1, 
zi = -gsDesign::hrn2z(hr = 0.8, n = 150+180, ratio = 1))
#> $prob_alpha
#> [1] 1.264453e-09 1.010150e-03 1.242573e-02
#> 
#> $prob_alpha_plus
#> [1] 1.264453e-09 1.432083e-03 1.186581e-01
#> 
#> $prob_beta
#> [1] 0.971766584 0.027223265 0.004116917
#> 

# case 2: currently at IA1, compute conditional power at IA2, IA3 and FA, 
# with user-input theta
gs_cp(x = x, 
  theta = c(0.15, 0.2, 0.25, 0.3), 
  i = 1, 
  zi = -gsDesign::hrn2z(hr = 0.8, n = 150+180, ratio = 1))
#> $prob_alpha
#> [1] 8.260564e-09 8.860056e-03 3.790670e-02
#> 
#> $prob_alpha_plus
#> [1] 8.260564e-09 1.477123e-02 5.011579e-01
#> 
#> $prob_beta
#> [1] 0.944398183 0.046741752 0.001632655
#> 

# Example 2: futility is only tested at IA1
# original design ----
x <- gs_design_ahr(enroll_rate = enroll_rate, fail_rate = fail_rate,
                   alpha = 0.025, beta = 0.1, ratio = 1,
                   info_frac = c(0.4, 0.6, 0.8, 1), analysis_time = 30,
                   binding = FALSE,
                   upper = gs_spending_bound,
                   upar = list(sf = sfLDOF, total_spend = 0.025, param = NULL),
                   test_upper = c(FALSE, TRUE, TRUE, TRUE),
                   lower = gs_spending_bound,
                   lpar = list(sf = sfLDOF, total_spend = 0.1),
                   test_lower = c(TRUE, FALSE, FALSE, FALSE),
                   h1_spending = TRUE,
                   info_scale = "h0_h1_info") |> to_integer()

# calculate conditional power
# case 1: currently at IA1, compute conditional power at IA2, IA3 and FA,
# with default theta = NULL
gs_cp(x = x, i = 1,
      zi = -gsDesign::hrn2z(hr = 0.8, n = 150+180, ratio = 1))
#> $prob_alpha
#> [1] 1.282562e-09 1.144666e-03 1.033265e-01
#> 
#> $prob_alpha_plus
#> [1] 1.282562e-09 1.144666e-03 1.033422e-01
#> 
#> $prob_beta
#> [1] 0.0000000 0.9988553 0.8955453
#> 

# case 2: currently at IA1, compute conditional power at IA2, IA3 and FA,
# with user-input theta
gs_cp(x = x,
  theta = c(0.15, 0.2, 0.25, 0.3),
  i = 1,
  zi = -gsDesign::hrn2z(hr = 0.8, n = 150+180, ratio = 1))
#> $prob_alpha
#> [1] 7.790501e-09 1.168612e-02 4.566769e-01
#> 
#> $prob_alpha_plus
#> [1] 7.790501e-09 1.168612e-02 4.562043e-01
#> 
#> $prob_beta
#> [1] 0.0000000 0.9883139 0.5318764
#> 
```
