# Derive spending bound for MaxCombo group sequential boundary

Derive spending bound for MaxCombo group sequential boundary

## Usage

``` r
gs_spending_combo(par = NULL, info = NULL)
```

## Arguments

- par:

  A list with the following items:

  - `sf` (class spending function).

  - `total_spend` (total spend).

  - `param` (any parameters needed by the spending function `sf()`).

  - `timing` (a vector containing values at which spending function is
    to be evaluated or `NULL` if information-based spending is used).

  - `max_info` (when `timing` is `NULL`, this can be input as positive
    number to be used with `info` for information fraction at each
    analysis).

- info:

  Statistical information at all analyses, at least up to analysis k.

## Value

A vector of the alpha spending per analysis.

## Examples

``` r
# alpha-spending
par <- list(sf = gsDesign::sfLDOF, total_spend = 0.025)
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.0001035057 0.0060483891 0.0250000000

par <- list(sf = gsDesign::sfLDPocock, total_spend = 0.025)
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.01132081 0.01908456 0.02500000

par <- list(sf = gsDesign::sfHSD, total_spend = 0.025, param = -40)
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 6.557724e-14 4.048992e-08 2.500000e-02

# Kim-DeMets (power) Spending Function
par <- list(sf = gsDesign::sfPower, total_spend = 0.025, param = 1.5)
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.004811252 0.013608276 0.025000000

# Exponential Spending Function
par <- list(sf = gsDesign::sfExponential, total_spend = 0.025, param = 1)
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.000015625 0.003952847 0.025000000

# Two-parameter Spending Function Families
par <- list(sf = gsDesign::sfLogistic, total_spend = 0.025, param = c(.1, .4, .01, .1))
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.001757277 0.008146545 0.025000000

par <- list(sf = gsDesign::sfBetaDist, total_spend = 0.025, param = c(.1, .4, .01, .1))
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.001818609 0.006568999 0.025000000

par <- list(sf = gsDesign::sfCauchy, total_spend = 0.025, param = c(.1, .4, .01, .1))
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.001378849 0.023755732 0.025000000

par <- list(sf = gsDesign::sfExtremeValue, total_spend = 0.025, param = c(.1, .4, .01, .1))
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.001785159 0.007184159 0.025000000

par <- list(sf = gsDesign::sfExtremeValue2, total_spend = 0.025, param = c(.1, .4, .01, .1))
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.001799588 0.007015878 0.025000000

par <- list(sf = gsDesign::sfNormal, total_spend = 0.025, param = c(.1, .4, .01, .1))
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.001797471 0.006969761 0.025000000

# t-distribution Spending Function
par <- list(sf = gsDesign::sfTDist, total_spend = 0.025, param = c(-1, 1.5, 4))
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.002063494 0.009705759 0.025000000

# Piecewise Linear and Step Function Spending Functions
par <- list(sf = gsDesign::sfLinear, total_spend = 0.025, param = c(.2, .4, .05, .2))
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.00375000 0.01388889 0.02500000

par <- list(sf = gsDesign::sfStep, total_spend = 0.025, param = c(1 / 3, 2 / 3, .1, .1))
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.0025 0.0025 0.0250

# Pointwise Spending Function
par <- list(sf = gsDesign::sfPoints, total_spend = 0.025, param = c(.25, .25))
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.00625 0.00625 0.02500

# Truncated, trimmed and gapped spending functions
par <- list(sf = gsDesign::sfTruncated, total_spend = 0.025,
  param = list(trange = c(.2, .8), sf = gsDesign::sfHSD, param = 1))
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.00788072 0.02137939 0.02500000

par <- list(sf = gsDesign::sfTrimmed, total_spend = 0.025,
  param = list(trange = c(.2, .8), sf = gsDesign::sfHSD, param = 1))
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.01121102 0.01924407 0.02500000

par <- list(sf = gsDesign::sfGapped, total_spend = 0.025,
  param = list(trange = c(.2, .8), sf = gsDesign::sfHSD, param = 1))
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.007169093 0.007169093 0.025000000

# Xi and Gallo conditional error spending functions
par <- list(sf = gsDesign::sfXG1, total_spend = 0.025, param = 0.5)
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.0001035057 0.0060483891 0.0250000000

par <- list(sf = gsDesign::sfXG2, total_spend = 0.025, param = 0.14)
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.008419231 0.021216583 0.025000000

par <- list(sf = gsDesign::sfXG3, total_spend = 0.025, param = 0.013)
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.02428922 0.02477989 0.02500000

# beta-spending
par <- list(sf = gsDesign::sfLDOF, total_spend = 0.2)
gs_spending_combo(par, info = 1:3 / 3)
#> [1] 0.02643829 0.11651432 0.20000000
```
