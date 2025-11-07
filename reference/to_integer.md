# Round sample size and events

Round sample size and events

## Usage

``` r
to_integer(x, ...)

# S3 method for class 'fixed_design'
to_integer(x, round_up_final = TRUE, ratio = x$input$ratio, ...)

# S3 method for class 'gs_design'
to_integer(x, round_up_final = TRUE, ratio = x$input$ratio, ...)
```

## Arguments

- x:

  An object returned by fixed_design_xxx() and gs_design_xxx().

- ...:

  Additional parameters (not used).

- round_up_final:

  Events at final analysis is rounded up if `TRUE`; otherwise, just
  rounded, unless it is very close to an integer.

- ratio:

  Positive integer for randomization ratio (experimental:control). A
  positive integer will result in rounded sample size, which is a
  multiple of (ratio + 1). A positive non-integer will result in round
  sample size, which may not be a multiple of (ratio + 1). A negative
  number will result in an error.

## Value

A list similar to the output of fixed_design_xxx() and gs_design_xxx(),
except the sample size is an integer.

## Details

For the sample size of the fixed design:

- When `ratio` is a positive integer, the sample size is rounded up to a
  multiple of `ratio + 1` if `round_up_final = TRUE`, and just rounded
  to a multiple of `ratio + 1` if `round_up_final = FALSE`.

- When `ratio` is a positive non-integer, the sample size is rounded up
  if `round_up_final = TRUE`, (may not be a multiple of `ratio + 1`),
  and just rounded if `round_up_final = FALSE` (may not be a multiple of
  `ratio + 1`). Note the default `ratio` is taken from `x$input$ratio`.

For the number of events of the fixed design:

- If the continuous event is very close to an integer within 0.01
  differences, say 100.001 or 99.999, then the integer events is 100.

- Otherwise, round up if `round_up_final = TRUE` and round if
  `round_up_final = FALSE`.

For the sample size of group sequential designs:

- When `ratio` is a positive integer, the final sample size is rounded
  to a multiple of `ratio + 1`.

  - For 1:1 randomization (experimental:control), set `ratio = 1` to
    round to an even sample size.

  - For 2:1 randomization, set `ratio = 2` to round to a multiple of 3.

  - For 3:2 randomization, set `ratio = 4` to round to a multiple of 5.

  - Note that for the final analysis, the sample size is rounded up to
    the nearest multiple of `ratio + 1` if `round_up_final = TRUE`. If
    `round_up_final = FALSE`, the final sample size is rounded to the
    nearest multiple of `ratio + 1`.

- When `ratio` is positive non-integer, the final sample size MAY NOT be
  rounded to a multiple of `ratio + 1`.

  - The final sample size is rounded up if `round_up_final = TRUE`.

  - Otherwise, it is just rounded.

For the events of group sequential designs:

- For events at interim analysis, it is rounded.

- For events at final analysis:

  - If the continuous event is very close to an integer within 0.01
    differences, say 100.001 or 99.999, then the integer events is 100.

  - Otherwise, final events is rounded up if `round_up_final = TRUE` and
    rounded if `round_up_final = FALSE`.

## Examples

``` r
library(gsDesign2)

# Average hazard ratio
# \donttest{
x <- fixed_design_ahr(
  alpha = .025, power = .9,
  enroll_rate = define_enroll_rate(duration = 18, rate = 1),
  fail_rate = define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12, hr = c(1, .6),
    dropout_rate = .001
  ),
  study_duration = 36
)
x |>
  to_integer() |>
  summary()
#> # A tibble: 1 × 8
#>   Design                   N Events  Time   AHR Bound alpha Power
#>   <chr>                <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Average hazard ratio   464    325  35.9 0.697  1.96 0.025 0.900

# FH
x <- fixed_design_fh(
  alpha = 0.025, power = 0.9,
  enroll_rate = define_enroll_rate(duration = 18, rate = 20),
  fail_rate = define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12,
    hr = c(1, .6),
    dropout_rate = .001
  ),
  rho = 0.5, gamma = 0.5,
  study_duration = 36, ratio = 1
)
x |>
  to_integer() |>
  summary()
#> # A tibble: 1 × 8
#>   Design                              N Events  Time   AHR Bound alpha Power
#>   <chr>                           <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Fleming-Harrington FH(0.5, 0.5)   378    264  35.8 0.664  1.96 0.025 0.900

# MB
x <- fixed_design_mb(
  alpha = 0.025, power = 0.9,
  enroll_rate = define_enroll_rate(duration = 18, rate = 20),
  fail_rate = define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12, hr = c(1, .6),
    dropout_rate = .001
  ),
  tau = Inf, w_max = 2,
  study_duration = 36, ratio = 1
)
x |>
  to_integer() |>
  summary()
#> # A tibble: 1 × 8
#>   Design                              N Events  Time   AHR Bound alpha Power
#>   <chr>                           <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Modestly weighted LR: tau = Inf   382    267  35.8 0.667  1.96 0.025 0.899
# }
# \donttest{
# Example 1: Information fraction based spending
gs_design_ahr(
  analysis_time = c(18, 30),
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL),
  lower = gs_b,
  lpar = c(-Inf, -Inf)
) |>
  to_integer() |>
  summary()
#> # A tibble: 2 × 7
#> # Groups:   Analysis [2]
#>   Analysis         Bound     Z `~HR at bound` `Nominal p` `Alternate hypothesis`
#>   <chr>            <chr> <dbl>          <dbl>       <dbl>                  <dbl>
#> 1 Analysis: 1 Tim… Effi…  2.57          0.699      0.0051                  0.295
#> 2 Analysis: 2 Tim… Effi…  1.99          0.801      0.0234                  0.901
#> # ℹ 1 more variable: `Null hypothesis` <dbl>

gs_design_wlr(
  analysis_time = c(18, 30),
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL),
  lower = gs_b,
  lpar = c(-Inf, -Inf)
) |>
  to_integer() |>
  summary()
#> # A tibble: 2 × 7
#> # Groups:   Analysis [2]
#>   Analysis        Bound     Z `~wHR at bound` `Nominal p` `Alternate hypothesis`
#>   <chr>           <chr> <dbl>           <dbl>       <dbl>                  <dbl>
#> 1 Analysis: 1 Ti… Effi…  2.57           0.701      0.0051                  0.292
#> 2 Analysis: 2 Ti… Effi…  1.99           0.802      0.0234                  0.900
#> # ℹ 1 more variable: `Null hypothesis` <dbl>

gs_design_rd(
  p_c = tibble::tibble(stratum = c("A", "B"), rate = c(.2, .3)),
  p_e = tibble::tibble(stratum = c("A", "B"), rate = c(.15, .27)),
  weight = "ss",
  stratum_prev = tibble::tibble(stratum = c("A", "B"), prevalence = c(.4, .6)),
  info_frac = c(0.7, 1),
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL),
  lower = gs_b,
  lpar = c(-Inf, -Inf)
) |>
  to_integer() |>
  summary()
#> # A tibble: 2 × 7
#> # Groups:   Analysis [2]
#>   Analysis Bound     Z ~Risk difference at …¹ `Nominal p` `Alternate hypothesis`
#>   <chr>    <chr> <dbl>                  <dbl>       <dbl>                  <dbl>
#> 1 Analysi… Effi…  2.44                 0.0339      0.0074                  0.616
#> 2 Analysi… Effi…  2                    0.0232      0.0228                  0.9  
#> # ℹ abbreviated name: ¹​`~Risk difference at bound`
#> # ℹ 1 more variable: `Null hypothesis` <dbl>

# Example 2: Calendar based spending
x <- gs_design_ahr(
  upper = gs_spending_bound,
  analysis_time = c(18, 30),
  upar = list(
    sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL,
    timing = c(18, 30) / 30
  ),
  lower = gs_b,
  lpar = c(-Inf, -Inf)
) |> to_integer()

# The IA nominal p-value is the same as the IA alpha spending
x$bound$`nominal p`[1]
#> [1] 0.003808063
gsDesign::sfLDOF(alpha = 0.025, t = 18 / 30)$spend
#> [1] 0.003808063
# }
```
