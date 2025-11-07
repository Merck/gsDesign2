# Predict time at which a targeted event count is achieved

`expected_time()` is made to match input format with
[`ahr()`](https://merck.github.io/gsDesign2/reference/ahr.md) and to
solve for the time at which the expected accumulated events is equal to
an input target. Enrollment and failure rate distributions are specified
as follows. The piecewise exponential distribution allows a simple
method to specify a distribution and enrollment pattern where the
enrollment, failure and dropout rates changes over time.

## Usage

``` r
expected_time(
  enroll_rate = define_enroll_rate(duration = c(2, 2, 10), rate = c(3, 6, 9) * 5),
  fail_rate = define_fail_rate(stratum = "All", duration = c(3, 100), fail_rate =
    log(2)/c(9, 18), hr = c(0.9, 0.6), dropout_rate = rep(0.001, 2)),
  target_event = 150,
  ratio = 1,
  interval = c(0.01, 100)
)
```

## Arguments

- enroll_rate:

  An `enroll_rate` data frame with or without stratum created by
  [`define_enroll_rate()`](https://merck.github.io/gsDesign2/reference/define_enroll_rate.md).

- fail_rate:

  A `fail_rate` data frame with or without stratum created by
  [`define_fail_rate()`](https://merck.github.io/gsDesign2/reference/define_fail_rate.md).

- target_event:

  The targeted number of events to be achieved.

- ratio:

  Experimental:Control randomization ratio.

- interval:

  An interval that is presumed to include the time at which expected
  event count is equal to `target_event`.

## Value

A data frame with `Time` (computed to match events in `target_event`),
`AHR` (average hazard ratio), `Events` (`target_event` input), `info`
(information under given scenarios), and `info0` (information under
related null hypothesis) for each value of `total_duration` input.

## Specification

## Examples

``` r
# Example 1 ----
# default
# \donttest{
expected_time()
#>       time       ahr event     info info0
#> 1 14.90814 0.7865729   150 36.86707  37.5
# }

# Example 2 ----
# check that result matches a finding using AHR()
# Start by deriving an expected event count
enroll_rate <- define_enroll_rate(duration = c(2, 2, 10), rate = c(3, 6, 9) * 5)
fail_rate <- define_fail_rate(
  duration = c(3, 100),
  fail_rate = log(2) / c(9, 18),
  hr = c(.9, .6),
  dropout_rate = .001
)
total_duration <- 20
xx <- ahr(enroll_rate, fail_rate, total_duration)
xx
#>   time       ahr   n    event     info    info0
#> 1   20 0.7377944 540 208.3641 50.97575 52.09103

# Next we check that the function confirms the timing of the final analysis.
# \donttest{
expected_time(enroll_rate, fail_rate,
  target_event = xx$event, interval = c(.5, 1.5) * xx$time
)
#>   time       ahr    event     info    info0
#> 1   20 0.7377944 208.3641 50.97575 52.09103
# }

# Example 3 ----
# In this example, we verify `expected_time()` by `ahr()`.
# \donttest{
x <- ahr(
  enroll_rate = enroll_rate, fail_rate = fail_rate,
  ratio = 1, total_duration = 20
)

cat("The number of events by 20 months is ", x$event, ".\n")
#> The number of events by 20 months is  208.3641 .

y <- expected_time(
  enroll_rate = enroll_rate, fail_rate = fail_rate,
  ratio = 1, target_event = x$event
)

cat("The time to get ", x$event, " is ", y$time, "months.\n")
#> The time to get  208.3641  is  20 months.
# }
```
