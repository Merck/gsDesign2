# Blinded estimation of average hazard ratio

Based on blinded data and assumed hazard ratios in different intervals,
compute a blinded estimate of average hazard ratio (AHR) and
corresponding estimate of statistical information. This function is
intended for use in computing futility bounds based on spending assuming
the input hazard ratio (hr) values for intervals specified here.

## Usage

``` r
ahr_blinded(
  surv = survival::Surv(time = simtrial::ex1_delayed_effect$month, event =
    simtrial::ex1_delayed_effect$evntd),
  intervals = c(3, Inf),
  hr = c(1, 0.6),
  ratio = 1
)
```

## Arguments

- surv:

  Input survival object (see
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html));
  note that only 0 = censored, 1 = event for
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html).

- intervals:

  Vector containing positive values indicating interval lengths where
  the exponential rates are assumed. Note that a final infinite interval
  is added if any events occur after the final interval specified.

- hr:

  Vector of hazard ratios assumed for each interval.

- ratio:

  Ratio of experimental to control randomization.

## Value

A `tibble` with one row containing

- `ahr` - Blinded average hazard ratio based on assumed period-specific
  hazard ratios input in `fail_rate` and observed events in the
  corresponding intervals.

- `event` - Total observed number of events.

- `info0` - Information under related null hypothesis.

- `theta` - Natural parameter for group sequential design representing
  expected incremental drift at all analyses.

## Specification

The contents of this section are shown in PDF user manual only.

## Examples

``` r
ahr_blinded(
  surv = survival::Surv(
    time = simtrial::ex2_delayed_effect$month,
    event = simtrial::ex2_delayed_effect$evntd
  ),
  intervals = c(4, 100),
  hr = c(1, .55),
  ratio = 1
)
#> # A tibble: 1 × 4
#>   event   ahr theta info0
#>   <dbl> <dbl> <dbl> <dbl>
#> 1   228 0.826 0.191    57
```
