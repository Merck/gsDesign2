# Bound summary table

Summarizes the efficacy and futility bounds for each analysis.

## Usage

``` r
gs_bound_summary(
  x,
  digits = 4,
  ddigits = 2,
  tdigits = 0,
  timename = "Month",
  alpha = NULL
)
```

## Arguments

- x:

  Design object.

- digits:

  Number of digits past the decimal to be printed in the body of the
  table.

- ddigits:

  Number of digits past the decimal to be printed for the natural
  parameter delta.

- tdigits:

  Number of digits past the decimal point to be shown for estimated
  timing of each analysis.

- timename:

  Text string indicating time unit.

- alpha:

  Vector of alpha values to compute additional efficacy columns.

## Value

A data frame

## See also

[`gsDesign::gsBoundSummary()`](https://keaven.github.io/gsDesign/reference/gsBoundSummary.html)

## Examples

``` r
library(gsDesign2)

x <- gs_design_ahr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
gs_bound_summary(x)
#>       Analysis                Value Efficacy Futility
#> 1    IA 1: 31%                    Z   3.8728  -1.6993
#> 2       N: 439          p (1-sided)   0.0001   0.9554
#> 3   Events: 99         ~HR at bound   0.4600   1.4060
#> 4    Month: 12     P(Cross) if HR=1   0.0001   0.0446
#> 5              P(Cross) if AHR=0.81   0.0025   0.0032
#> 6    IA 2: 77%                    Z   2.3100   1.0889
#> 7       N: 527          p (1-sided)   0.0104   0.1381
#> 8  Events: 248         ~HR at bound   0.7455   0.8707
#> 9    Month: 25     P(Cross) if HR=1   0.0105   0.8620
#> 10             P(Cross) if AHR=0.71   0.6348   0.0599
#> 11       Final                    Z   2.0161   2.0143
#> 12      N: 527          p (1-sided)   0.0219   0.0220
#> 13 Events: 323         ~HR at bound   0.7990   0.7992
#> 14   Month: 36     P(Cross) if HR=1   0.0243   0.9756
#> 15             P(Cross) if AHR=0.68   0.9000   0.1001

x <- gs_design_wlr(info_frac = c(.25, .75, 1), analysis_time = c(12, 25, 36))
gs_bound_summary(x)
#>       Analysis                 Value Efficacy Futility
#> 1    IA 1: 30%                     Z   3.8945  -1.8697
#> 2       N: 436           p (1-sided)   0.0000   0.9692
#> 3   Events: 99          ~HR at bound   0.4569   1.4566
#> 4    Month: 12      P(Cross) if HR=1   0.0000   0.0308
#> 5              P(Cross) if wAHR=0.81   0.0012   0.0032
#> 6    IA 2: 76%                     Z   2.3162   1.0994
#> 7       N: 523           p (1-sided)   0.0103   0.1358
#> 8  Events: 246          ~HR at bound   0.7443   0.8692
#> 9    Month: 25      P(Cross) if HR=1   0.0103   0.8642
#> 10             P(Cross) if wAHR=0.71   0.6325   0.0610
#> 11       Final                     Z   2.0152   2.0151
#> 12      N: 523           p (1-sided)   0.0219   0.0219
#> 13 Events: 321          ~HR at bound   0.7985   0.7986
#> 14   Month: 36      P(Cross) if HR=1   0.0242   0.9758
#> 15             P(Cross) if wAHR=0.68   0.9000   0.1000

# Report multiple efficacy bounds (only supported for AHR designs)
x <- gs_design_ahr(analysis_time = 1:3*12, alpha = 0.0125)
gs_bound_summary(x, alpha = c(0.025, 0.05))
#>       Analysis                Value α=0.0125 α=0.025 α=0.05 Futility
#> 1    IA 1: 31%                    Z   4.3506  3.8728 3.3437  -1.6192
#> 2       N: 509          p (1-sided)   0.0000  0.0001 0.0004   0.9473
#> 3  Events: 115         ~HR at bound   0.4449  0.4863 0.5366   1.3518
#> 4    Month: 12     P(Cross) if HR=1   0.0000  0.0001 0.0004   0.0527
#> 5              P(Cross) if AHR=0.81   0.0007  0.0032 0.0139   0.0032
#> 6    IA 2: 74%                    Z   2.6778  2.3579 2.0022   1.1590
#> 7       N: 611          p (1-sided)   0.0037  0.0092 0.0226   0.1232
#> 8  Events: 278         ~HR at bound   0.7251  0.7535 0.7863   0.8701
#> 9    Month: 24     P(Cross) if HR=1   0.0037  0.0092 0.0228   0.8768
#> 10             P(Cross) if AHR=0.72   0.5336  0.6571 0.7770   0.0556
#> 11       Final                    Z   2.2781  2.0096 1.7134   2.2775
#> 12      N: 611          p (1-sided)   0.0114  0.0222 0.0433   0.0114
#> 13 Events: 375         ~HR at bound   0.7903  0.8125 0.8378   0.7903
#> 14   Month: 36     P(Cross) if HR=1   0.0122  0.0237 0.0445   0.9878
#> 15             P(Cross) if AHR=0.68   0.9000  0.9252 0.9390   0.1001
```
