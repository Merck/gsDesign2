# Average hazard ratio under non-proportional hazards

Provides a geometric average hazard ratio under various non-proportional
hazards assumptions for either single or multiple strata studies. The
piecewise exponential distribution allows a simple method to specify a
distribution and enrollment pattern where the enrollment, failure and
dropout rates changes over time.

## Usage

``` r
pw_info(
  enroll_rate = define_enroll_rate(duration = c(2, 2, 10), rate = c(3, 6, 9)),
  fail_rate = define_fail_rate(duration = c(3, 100), fail_rate = log(2)/c(9, 18), hr =
    c(0.9, 0.6), dropout_rate = 0.001),
  total_duration = 30,
  ratio = 1
)
```

## Arguments

- enroll_rate:

  An `enroll_rate` data frame with or without stratum created by
  [`define_enroll_rate()`](https://merck.github.io/gsDesign2/reference/define_enroll_rate.md).

- fail_rate:

  A `fail_rate` data frame with or without stratum created by
  [`define_fail_rate()`](https://merck.github.io/gsDesign2/reference/define_fail_rate.md).

- total_duration:

  Total follow-up from start of enrollment to data cutoff; this can be a
  single value or a vector of positive numbers.

- ratio:

  Experimental:Control randomization ratio.

## Value

A data frame with `time` (from `total_duration`), `stratum`, `t`, `hr`
(hazard ratio), `event` (expected number of events), `info` (information
under given scenarios), `info0` (information under related null
hypothesis), and `n` (sample size) for each value of `total_duration`
input

## Examples

``` r
# Example: default
pw_info()
#>   time stratum t  hr  n    event     info    info0
#> 1   30     All 0 0.9 12 21.24782 5.300180 5.311956
#> 2   30     All 3 0.6 96 37.24314 9.027063 9.310786

# Example: default with multiple analysis times (varying total_duration)
pw_info(total_duration = c(15, 30))
#>   time stratum t  hr  n    event     info    info0
#> 1   15     All 0 0.9 12 20.13991 5.023729 5.034979
#> 2   15     All 3 0.6 96 10.13850 2.417457 2.534625
#> 3   30     All 0 0.9 12 21.24782 5.300180 5.311956
#> 4   30     All 3 0.6 96 37.24314 9.027063 9.310786

# Stratified population
enroll_rate <- define_enroll_rate(
  stratum = c(rep("Low", 2), rep("High", 3)),
  duration = c(2, 10, 4, 4, 8),
  rate = c(5, 10, 0, 3, 6)
)
fail_rate <- define_fail_rate(
  stratum = c(rep("Low", 2), rep("High", 2)),
  duration = c(1, Inf, 1, Inf),
  fail_rate = c(.1, .2, .3, .4),
  dropout_rate = .001,
  hr = c(.9, .75, .8, .6)
)
# Give results by change-points in the piecewise model
ahr(enroll_rate = enroll_rate, fail_rate = fail_rate, total_duration = c(15, 30))
#>   time       ahr   n    event     info    info0
#> 1   15 0.7332218 164 113.2782 28.18130 28.31954
#> 2   30 0.7175169 170 166.1836 41.49942 41.54590

# Same example, give results by strata and time period
pw_info(enroll_rate = enroll_rate, fail_rate = fail_rate, total_duration = c(15, 30))
#>   time stratum t   hr   n     event      info     info0
#> 1   15    High 0 0.80   0 12.076677  2.990626  3.019169
#> 2   15    High 1 0.60  54 23.118608  5.741884  5.779652
#> 3   15     Low 0 0.90   5  9.962824  2.484435  2.490706
#> 4   15     Low 1 0.75 105 68.120046 16.964361 17.030011
#> 5   30    High 0 0.80   0 14.169853  3.509171  3.542463
#> 6   30    High 1 0.60  60 45.213092 11.297986 11.303273
#> 7   30     Low 0 0.90   5  9.962824  2.484435  2.490706
#> 8   30     Low 1 0.75 105 96.837847 24.207826 24.209462
```
