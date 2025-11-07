# Information and effect size based on AHR approximation

Based on piecewise enrollment rate, failure rate, and dropout rates
computes approximate information and effect size using an average hazard
ratio model.

## Usage

``` r
gs_info_ahr(
  enroll_rate = define_enroll_rate(duration = c(2, 2, 10), rate = c(3, 6, 9)),
  fail_rate = define_fail_rate(duration = c(3, 100), fail_rate = log(2)/c(9, 18), hr =
    c(0.9, 0.6), dropout_rate = 0.001),
  ratio = 1,
  event = NULL,
  analysis_time = NULL,
  interval = c(0.01, 1000)
)
```

## Arguments

- enroll_rate:

  Enrollment rates from
  [`define_enroll_rate()`](https://merck.github.io/gsDesign2/reference/define_enroll_rate.md).

- fail_rate:

  Failure and dropout rates from
  [`define_fail_rate()`](https://merck.github.io/gsDesign2/reference/define_fail_rate.md).

- ratio:

  Experimental:Control randomization ratio.

- event:

  Targeted minimum events at each analysis.

- analysis_time:

  Targeted minimum study duration at each analysis.

- interval:

  An interval that is presumed to include the time at which expected
  event count is equal to targeted event.

## Value

A data frame with columns `analysis`, `time`, `ahr`, `event`, `theta`,
`info`, `info0`. The columns `info` and `info0` contain statistical
information under H1, H0, respectively. For analysis `k`, `time[k]` is
the maximum of `analysis_time[k]` and the expected time required to
accrue the targeted `event[k]`. `ahr` is the expected average hazard
ratio at each analysis.

## Details

The [`ahr()`](https://merck.github.io/gsDesign2/reference/ahr.md)
function computes statistical information at targeted event times. The
[`expected_time()`](https://merck.github.io/gsDesign2/reference/expected_time.md)
function is used to get events and average HR at targeted
`analysis_time`.

## Specification

The contents of this section are shown in PDF user manual only.

## Examples

``` r
library(gsDesign)
library(gsDesign2)

# Example 1 ----
# \donttest{
# Only put in targeted events
gs_info_ahr(event = c(30, 40, 50))
#>   analysis     time    event       ahr     theta      info     info0
#> 1        1 14.90817 30.00008 0.7865726 0.2400702  7.373433  7.500019
#> 2        2 19.16437 40.00000 0.7442008 0.2954444  9.789940 10.000001
#> 3        3 24.54264 50.00000 0.7128241 0.3385206 12.227632 12.500000
# }

# Example 2 ----
# Only put in targeted analysis times
gs_info_ahr(analysis_time = c(18, 27, 36))
#>   analysis time    event       ahr     theta      info     info0
#> 1        1   18 37.59032 0.7545471 0.2816376  9.208013  9.397579
#> 2        2   27 54.01154 0.7037599 0.3513180 13.216112 13.502885
#> 3        3   36 66.23948 0.6833395 0.3807634 16.267921 16.559870

# Example 3 ----
# \donttest{
# Some analysis times after time at which targeted event accrue
# Check that both Time >= input analysis_time and event >= input event
gs_info_ahr(event = c(30, 40, 50), analysis_time = c(16, 19, 26))
#>   analysis     time    event       ahr     theta      info     info0
#> 1        1 16.00000 33.06876 0.7759931 0.2536117  8.118487  8.267189
#> 2        2 19.16437 40.00000 0.7442008 0.2954444  9.789940 10.000001
#> 3        3 26.00000 52.41802 0.7071808 0.3464689 12.822714 13.104505
gs_info_ahr(event = c(30, 40, 50), analysis_time = c(14, 20, 24))
#>   analysis     time    event       ahr     theta      info     info0
#> 1        1 14.90817 30.00008 0.7865726 0.2400702  7.373433  7.500019
#> 2        2 20.00000 41.67282 0.7377944 0.3040901 10.195150 10.418206
#> 3        3 24.54264 50.00000 0.7128241 0.3385206 12.227632 12.500000
# }
```
