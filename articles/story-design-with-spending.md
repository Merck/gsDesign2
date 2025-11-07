# Trial design with spending under NPH

``` r
library(gsDesign)
library(gsDesign2)
```

## Overview

This vignette covers how to implement designs for trials with spending
assuming non-proportional hazards. We are primarily concerned with
practical issues of implementation rather than design strategies, but we
will not ignore design strategy.

## Scenario for consideration

Here we set up enrollment, failure and dropout rates along with
assumptions for enrollment duration and times of analyses. We assume
there are 4 analysis (3 interim analyses + 1 final analysis) conducted
18, 24, 30, and 36 months after trial enrollment is opened.

``` r
n_analysis <- 4
analysis_time <- c(18, 24, 30, 36)
```

We assume there is a single stratum and enrollment targeted to last for
12 months. For the first 2 months, second 2 months, third 2 months and
the remaining months, the relative enrollment rates are \\8:12:16:24\\.
These rates will be updated by a constant multiple at the time of design
as we will note below.

``` r
enroll_rate <- define_enroll_rate(
  duration = c(2, 2, 2, 6),
  rate = c(8, 12, 16, 24)
)

enroll_rate |>
  gt::gt() |>
  gt::tab_header(title = "Planned Relative Enrollment Rates")
```

| Planned Relative Enrollment Rates |          |      |
|-----------------------------------|----------|------|
| stratum                           | duration | rate |
| All                               | 2        | 8    |
| All                               | 2        | 12   |
| All                               | 2        | 16   |
| All                               | 6        | 24   |

We assume a hazard ratio (HR) of 0.9 for the first 3 months 0.6
thereafter. We also assume the the control time-to-event follows a
piecewise exponential distribution with a median of 8 month for the
first 3 months and 14 months thereafter.

``` r
fail_rate <- define_fail_rate(
  duration = c(3, 100),
  fail_rate = log(2) / c(8, 14),
  hr = c(.9, .6),
  dropout_rate = .001
)

fail_rate |>
  gt::gt() |>
  gt::tab_header(title = "Table of Failure Rate Assumptions")
```

| Table of Failure Rate Assumptions |          |            |              |     |
|-----------------------------------|----------|------------|--------------|-----|
| stratum                           | duration | fail_rate  | dropout_rate | hr  |
| All                               | 3        | 0.08664340 | 0.001        | 0.9 |
| All                               | 100      | 0.04951051 | 0.001        | 0.6 |

## Fixed design with no interim analysis

We can derive power for the above enrollment rates and failure rates as
follows:

``` r
fixed_design_ahr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  power = NULL,
  ratio = 1,
  study_duration = 36,
  event = NULL
) |> summary()
#> # A tibble: 1 × 8
#>   Design                   N Events  Time   AHR Bound alpha Power
#>   <chr>                <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Average hazard ratio   216   151.    36 0.681  1.96 0.025 0.656
```

We now compute sample size and then translate from a continuous sample
size to an integer sample size.

``` r
fixed_design <- fixed_design_ahr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  power = .9,
  ratio = 1,
  study_duration = 36,
  event = NULL
) |> to_integer()

fixed_design$analysis
#> # A tibble: 1 × 8
#>   design     n event  time   ahr bound alpha power
#>   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 ahr      410   287  36.0 0.681  1.96 0.025 0.901
```

### Group sequential design

We now consider a group sequential design with bounds derived using
spending functions. We target the interim analysis for 24 months and the
final analysis for 36 months. Spending for both efficacy and futility is
based on the proportion of events expected at each analysis divided by
the total expected events at the final analysis.

``` r
gs <- gs_design_ahr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  info_frac = NULL,
  analysis_time = c(24, 36),
  upper = gs_spending_bound,
  lower = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1, param = NULL, timing = NULL),
  h1_spending = TRUE
) |> to_integer()

gs |>
  summary() |>
  gt::gt()
```

| Bound                                                                          | Z    | ~HR at bound | Nominal p | Alternate hypothesis | Null hypothesis |
|--------------------------------------------------------------------------------|------|--------------|-----------|----------------------|-----------------|
| Analysis: 1 Time: 23.9 N: 434 Events: 232 AHR: 0.71 Information fraction: 0.77 |      |              |           |                      |                 |
| Futility                                                                       | 1.04 | 0.8720       | 0.1486    | 0.0582               | 0.8514          |
| Efficacy                                                                       | 2.31 | 0.7383       | 0.0104    | 0.6235               | 0.0104          |
| Analysis: 2 Time: 35.8 N: 434 Events: 303 AHR: 0.68 Information fraction: 1    |      |              |           |                      |                 |
| Futility                                                                       | 1.94 | 0.7998       | 0.0260    | 0.0988               | 0.9723          |
| Efficacy                                                                       | 2.02 | 0.7933       | 0.0219    | 0.8998               | 0.0244          |
