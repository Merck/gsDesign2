# Average hazard ratio under non-proportional hazards

Provides a geometric average hazard ratio under various non-proportional
hazards assumptions for either single or multiple strata studies. The
piecewise exponential distribution allows a simple method to specify a
distribution and enrollment pattern where the enrollment, failure and
dropout rates changes over time.

## Usage

``` r
ahr(
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

  Ratio of experimental to control randomization.

## Value

A data frame with `time` (from `total_duration`), `ahr` (average hazard
ratio), `n` (sample size), `event` (expected number of events), `info`
(information under given scenarios), `and` info0 (information under
related null hypothesis) for each value of `total_duration` input.

## Specification

The contents of this section are shown in PDF user manual only.

## Examples

``` r
# Example 1: default
ahr()
#>   time       ahr   n    event     info    info0
#> 1   30 0.6952153 108 58.49097 14.32724 14.62274

# Example 2: default with multiple analysis times (varying total_duration)
ahr(total_duration = c(15, 30))
#>   time       ahr   n    event      info     info0
#> 1   15 0.7857415 108 30.27841  7.441186  7.569603
#> 2   30 0.6952153 108 58.49097 14.327243 14.622742

# Example 3: stratified population
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
ahr(enroll_rate = enroll_rate, fail_rate = fail_rate, total_duration = c(15, 30))
#>   time       ahr   n    event     info    info0
#> 1   15 0.7332218 164 113.2782 28.18130 28.31954
#> 2   30 0.7175169 170 166.1836 41.49942 41.54590
```
