# Approximate survival distribution with piecewise exponential distribution

Converts a discrete set of points from an arbitrary survival
distribution to a piecewise exponential approximation.

## Usage

``` r
s2pwe(times, survival)
```

## Arguments

- times:

  Positive increasing times at which survival distribution is provided.

- survival:

  Survival (1 - cumulative distribution function) at specified `times`.

## Value

A tibble containing the duration and rate.

## Specification

The contents of this section are shown in PDF user manual only.

## Examples

``` r
# Example: arbitrary numbers
s2pwe(1:9, (9:1) / 10)
#> # A tibble: 9 × 2
#>   duration  rate
#>      <dbl> <dbl>
#> 1        1 0.105
#> 2        1 0.118
#> 3        1 0.134
#> 4        1 0.154
#> 5        1 0.182
#> 6        1 0.223
#> 7        1 0.288
#> 8        1 0.405
#> 9        1 0.693
# Example: lognormal
s2pwe(c(1:6, 9), plnorm(c(1:6, 9), meanlog = 0, sdlog = 2, lower.tail = FALSE))
#> # A tibble: 7 × 2
#>   duration  rate
#>      <dbl> <dbl>
#> 1        1 0.693
#> 2        1 0.316
#> 3        1 0.224
#> 4        1 0.177
#> 5        1 0.148
#> 6        1 0.128
#> 7        3 0.103
```
