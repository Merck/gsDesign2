# Define enrollment rate

Define the enrollment rate of subjects for a study as following a
piecewise exponential distribution.

## Usage

``` r
define_enroll_rate(duration, rate, stratum = "All")
```

## Arguments

- duration:

  A numeric vector of ordered piecewise study duration interval.

- rate:

  A numeric vector of enrollment rate in each `duration`.

- stratum:

  A character vector of stratum name.

## Value

An `enroll_rate` data frame.

## Details

The `duration` are ordered piecewise for a duration equal to \\t_i -
t\_{i-1}\\, where \\0 = t_0 \< t_i \< \cdots \< t_M = \infty\\. The
enrollment rates are defined in each duration with the same length.

For a study with multiple strata, different duration and rates can be
specified in each stratum.

## Examples

``` r
# Define enroll rate without stratum
define_enroll_rate(
  duration = c(2, 2, 10),
  rate = c(3, 6, 9)
)
#> # A tibble: 3 × 3
#>   stratum duration  rate
#>   <chr>      <dbl> <dbl>
#> 1 All            2     3
#> 2 All            2     6
#> 3 All           10     9

# Define enroll rate with stratum
define_enroll_rate(
  duration = rep(c(2, 2, 2, 18), 3),
  rate = c((1:4) / 3, (1:4) / 2, (1:4) / 6),
  stratum = c(array("High", 4), array("Moderate", 4), array("Low", 4))
)
#> # A tibble: 12 × 3
#>    stratum  duration  rate
#>    <chr>       <dbl> <dbl>
#>  1 High            2 0.333
#>  2 High            2 0.667
#>  3 High            2 1    
#>  4 High           18 1.33 
#>  5 Moderate        2 0.5  
#>  6 Moderate        2 1    
#>  7 Moderate        2 1.5  
#>  8 Moderate       18 2    
#>  9 Low             2 0.167
#> 10 Low             2 0.333
#> 11 Low             2 0.5  
#> 12 Low            18 0.667
```
