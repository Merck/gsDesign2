# Define failure rate

Define subject failure rate for a study with two treatment groups. Also
supports stratified designs that have different failure rates in each
stratum.

## Usage

``` r
define_fail_rate(duration, fail_rate, dropout_rate, hr = 1, stratum = "All")
```

## Arguments

- duration:

  A numeric vector of ordered piecewise study duration interval.

- fail_rate:

  A numeric vector of failure rate in each `duration` in the control
  group.

- dropout_rate:

  A numeric vector of dropout rate in each `duration`.

- hr:

  A numeric vector of hazard ratio between treatment and control group.

- stratum:

  A character vector of stratum name.

## Value

A `fail_rate` data frame.

## Details

Define the failure and dropout rate of subjects for a study as following
a piecewise exponential distribution. The `duration` are ordered
piecewise for a duration equal to \\t_i - t\_{i-1}\\, where \\0 = t_0 \<
t_i \< \cdots \< t_M = \infty\\. The failure rate, dropout rate, and
hazard ratio in a study duration can be specified.

For a study with multiple strata, different duration, failure rates,
dropout rates, and hazard ratios can be specified in each stratum.

## Examples

``` r
# Define enroll rate
define_fail_rate(
  duration = c(3, 100),
  fail_rate = log(2) / c(9, 18),
  hr = c(.9, .6),
  dropout_rate = .001
)
#> # A tibble: 2 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 All            3    0.0770        0.001   0.9
#> 2 All          100    0.0385        0.001   0.6

# Define enroll rate with stratum
define_fail_rate(
  stratum = c(rep("Low", 2), rep("High", 2)),
  duration = 1,
  fail_rate = c(.1, .2, .3, .4),
  dropout_rate = .001,
  hr = c(.9, .75, .8, .6)
)
#> # A tibble: 4 × 5
#>   stratum duration fail_rate dropout_rate    hr
#>   <chr>      <dbl>     <dbl>        <dbl> <dbl>
#> 1 Low            1       0.1        0.001  0.9 
#> 2 Low            1       0.2        0.001  0.75
#> 3 High           1       0.3        0.001  0.8 
#> 4 High           1       0.4        0.001  0.6 
```
