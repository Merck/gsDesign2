# Expected events observed under piecewise exponential model

Computes expected events over time and by strata under the assumption of
piecewise constant enrollment rates and piecewise exponential failure
and censoring rates. The piecewise exponential distribution allows a
simple method to specify a distribution and enrollment pattern where the
enrollment, failure and dropout rates changes over time. While the main
purpose may be to generate a trial that can be analyzed at a single
point in time or using group sequential methods, the routine can also be
used to simulate an adaptive trial design. The intent is to enable
sample size calculations under non-proportional hazards assumptions for
stratified populations.

## Usage

``` r
expected_event(
  enroll_rate = define_enroll_rate(duration = c(2, 2, 10), rate = c(3, 6, 9)),
  fail_rate = define_fail_rate(duration = c(3, 100), fail_rate = log(2)/c(9, 18),
    dropout_rate = 0.001),
  total_duration = 25,
  simple = TRUE
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

  Total follow-up from start of enrollment to data cutoff.

- simple:

  If default (`TRUE`), return numeric expected number of events,
  otherwise a data frame as described below.

## Value

The default when `simple = TRUE` is to return the total expected number
of events as a real number. Otherwise, when `simple = FALSE`, a data
frame is returned with the following variables for each period specified
in `fail_rate`:

- `t`: start of period.

- `fail_rate`: failure rate during the period.

- `event`: expected events during the period.

The records in the returned data frame correspond to the input data
frame `fail_rate`.

## Details

More periods will generally be supplied in output than those that are
input. The intent is to enable expected event calculations in a tidy
format to maximize flexibility for a variety of purposes.

## Specification

## Examples

``` r
library(gsDesign2)

# Default arguments, simple output (total event count only)
expected_event()
#> [1] 57.3537

# Event count by time period
expected_event(simple = FALSE)
#>   t  fail_rate    event
#> 1 0 0.07701635 22.24824
#> 2 3 0.03850818 35.10546

# Early cutoff
expected_event(total_duration = .5)
#> [1] 0.02850923

# Single time period example
expected_event(
  enroll_rate = define_enroll_rate(duration = 10, rate = 10),
  fail_rate = define_fail_rate(duration = 100, fail_rate = log(2) / 6, dropout_rate = .01),
  total_duration = 22,
  simple = FALSE
)
#>   t fail_rate    event
#> 1 0 0.1155245 80.40974

# Single time period example, multiple enrollment periods
expected_event(
  enroll_rate = define_enroll_rate(duration = c(5, 5), rate = c(10, 20)),
  fail_rate = define_fail_rate(duration = 100, fail_rate = log(2) / 6, dropout_rate = .01),
  total_duration = 22, simple = FALSE
)
#>   t fail_rate    event
#> 1 0 0.1155245 118.8484
```
