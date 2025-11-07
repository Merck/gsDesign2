# Piecewise constant expected accrual

Computes the expected cumulative enrollment (accrual) given a set of
piecewise constant enrollment rates and times.

## Usage

``` r
expected_accrual(
  time = 0:24,
  enroll_rate = define_enroll_rate(duration = c(3, 3, 18), rate = c(5, 10, 20))
)
```

## Arguments

- time:

  Times at which enrollment is to be computed.

- enroll_rate:

  An `enroll_rate` data frame with or without stratum created by
  [`define_enroll_rate()`](https://merck.github.io/gsDesign2/reference/define_enroll_rate.md).

## Value

A vector with expected cumulative enrollment for the specified `times`.

## Specification

The contents of this section are shown in PDF user manual only.

## Examples

``` r
library(tibble)

# Example 1: default
expected_accrual()
#>  [1]   0   5  10  15  25  35  45  65  85 105 125 145 165 185 205 225 245 265 285
#> [20] 305 325 345 365 385 405

# Example 2: unstratified design
expected_accrual(
  time = c(5, 10, 20),
  enroll_rate = define_enroll_rate(
    duration = c(3, 3, 18),
    rate = c(5, 10, 20)
  )
)
#> [1]  35 125 325

# Example 3: stratified design
expected_accrual(
  time = c(24, 30, 40),
  enroll_rate = define_enroll_rate(
    stratum = c("subgroup", "complement"),
    duration = c(33, 33),
    rate = c(30, 30)
  )
)
#> [1] 1440 1800 1980

# Example 4: expected accrual over time
# Scenario 4.1: for the enrollment in the first 3 months,
# it is exactly 3 * 5 = 15.
expected_accrual(
  time = 3,
  enroll_rate = define_enroll_rate(duration = c(3, 3, 18), rate = c(5, 10, 20))
)
#> [1] 15

# Scenario 4.2: for the enrollment in the first 6 months,
# it is exactly 3 * 5 + 3 * 10 = 45.
expected_accrual(
  time = 6,
  enroll_rate = define_enroll_rate(duration = c(3, 3, 18), rate = c(5, 10, 20))
)
#> [1] 45

# Scenario 4.3: for the enrollment in the first 24 months,
# it is exactly 3 * 5 + 3 * 10 + 18 * 20 = 405.
expected_accrual(
  time = 24,
  enroll_rate = define_enroll_rate(duration = c(3, 3, 18), rate = c(5, 10, 20))
)
#> [1] 405

# Scenario 4.4: for the enrollment after 24 months,
# it is the same as that from the 24 months, since the enrollment is stopped.
expected_accrual(
  time = 25,
  enroll_rate = define_enroll_rate(duration = c(3, 3, 18), rate = c(5, 10, 20))
)
#> [1] 405

# Instead of compute the enrolled subjects one time point by one time point,
# we can also compute it once.
expected_accrual(
  time = c(3, 6, 24, 25),
  enroll_rate = define_enroll_rate(duration = c(3, 3, 18), rate = c(5, 10, 20))
)
#> [1]  15  45 405 405
```
