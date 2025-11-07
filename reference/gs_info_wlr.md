# Information and effect size for weighted log-rank test

Based on piecewise enrollment rate, failure rate, and dropout rates
computes approximate information and effect size using an average hazard
ratio model.

## Usage

``` r
gs_info_wlr(
  enroll_rate = define_enroll_rate(duration = c(2, 2, 10), rate = c(3, 6, 9)),
  fail_rate = define_fail_rate(duration = c(3, 100), fail_rate = log(2)/c(9, 18), hr =
    c(0.9, 0.6), dropout_rate = 0.001),
  ratio = 1,
  event = NULL,
  analysis_time = NULL,
  weight = "logrank",
  approx = "asymptotic",
  interval = c(0.01, 1000)
)
```

## Arguments

- enroll_rate:

  An `enroll_rate` data frame with or without stratum created by
  [`define_enroll_rate()`](https://merck.github.io/gsDesign2/reference/define_enroll_rate.md).

- fail_rate:

  Failure and dropout rates.

- ratio:

  Experimental:Control randomization ratio.

- event:

  Targeted minimum events at each analysis.

- analysis_time:

  Targeted minimum study duration at each analysis.

- weight:

  Weight of weighted log rank test:

  - `"logrank"` = regular logrank test.

  - `list(method = "fh", param = list(rho = ..., gamma = ...))` =
    Fleming-Harrington weighting functions.

  - `list(method = "mb", param = list(tau = ..., w_max = ...))` = Magirr
    and Burman weighting functions.

- approx:

  Approximate estimation method for Z statistics.

  - `"event_driven"` = only work under proportional hazard model with
    log rank test.

  - `"asymptotic"`.

- interval:

  An interval that is presumed to include the time at which expected
  event count is equal to targeted event.

## Value

A tibble with columns Analysis, Time, N, Events, AHR, delta, sigma2,
theta, info, info0. `info` and `info0` contain statistical information
under H1, H0, respectively. For analysis `k`, `Time[k]` is the maximum
of `analysis_time[k]` and the expected time required to accrue the
targeted `event[k]`. `AHR` is the expected average hazard ratio at each
analysis.

## Details

The [`ahr()`](https://merck.github.io/gsDesign2/reference/ahr.md)
function computes statistical information at targeted event times. The
[`expected_time()`](https://merck.github.io/gsDesign2/reference/expected_time.md)
function is used to get events and average HR at targeted
`analysis_time`.

## Examples

``` r
library(gsDesign2)

# Set enrollment rates
enroll_rate <- define_enroll_rate(duration = 12, rate = 500 / 12)

# Set failure rates
fail_rate <- define_fail_rate(
  duration = c(4, 100),
  fail_rate = log(2) / 15, # median survival 15 month
  hr = c(1, .6),
  dropout_rate = 0.001
)

# Set the targeted number of events and analysis time
event <- c(30, 40, 50)
analysis_time <- c(10, 24, 30)

gs_info_wlr(
  enroll_rate = enroll_rate, fail_rate = fail_rate,
  event = event, analysis_time = analysis_time
)
#>   analysis time        n     event       ahr        delta     sigma2     theta
#> 1        1   10 416.6667  77.80361 0.8720599 -0.005325328 0.04667807 0.1140863
#> 2        2   24 500.0001 246.28341 0.7164215 -0.040920239 0.12270432 0.3334865
#> 3        3   30 500.0001 293.69568 0.6955693 -0.052942680 0.14583769 0.3630247
#>       info    info0
#> 1 19.44920 19.47416
#> 2 61.35217 62.08666
#> 3 72.91885 74.25144
```
