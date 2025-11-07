# Integer designs

``` r
library(gsDesign)
library(gsDesign2)
library(tibble)
library(dplyr)
library(gt)
```

## Unstratified design

### Binary outcome

``` r
x <- gs_design_rd(
  p_c = tibble(stratum = "All", rate = 0.2),
  p_e = tibble(stratum = "All", rate = 0.15),
  info_frac = c(0.5, 0.8, 1),
  rd0 = 0,
  alpha = 0.025,
  beta = 0.1,
  ratio = 1,
  stratum_prev = NULL,
  weight = "unstratified",
  upper = gs_spending_bound,
  lower = gs_b,
  upar = list(sf = gsDesign::sfLDOF, timing = c(0.5, 0.8, 1), total_spend = 0.025, param = NULL),
  lpar = rep(-Inf, 3)
)

xi <- x |> to_integer()
```

Note that in the original design, the sample size is 1243.3070224,
1989.2912359, 2486.6140449, and in the integer design, the sample size
is updated to 1243, 1989, 2488. For the 2 interim analysis, we floor to
the closet multiplier of 2, since the randomization ratio is 1. At the
final analysis, we ceiling the sample size from 2486.6140449 to 2488 and
also make sure the integer sample size is a multiplier of 2.

Please also note that, since the sample size is rounded, the power of
the new design also changes a little bit, that is, from 0.9 to
0.9001336.

``` r
tibble(
  Design = rep(c("Original design", "Integer design"), each = 3),
  `Sample size` = c(x$analysis$n, xi$analysis$n),
  Z = c(
    (x$bound |> filter(bound == "upper"))$z,
    (xi$bound |> filter(bound == "upper"))$z
  ),
  `Information fraction` = c(x$analysis$info_frac, xi$analysis$info_frac),
  Power = c(
    (x$bound |> filter(bound == "upper"))$probability,
    (xi$bound |> filter(bound == "upper"))$probability
  )
) |>
  group_by(Design) |>
  gt() |>
  tab_header(
    title = "Comparison between the original/integer design",
    subtitle = "on binary endpoints (unstratified design)"
  ) |>
  fmt_number(columns = 2:5, decimals = 4)
```

| Comparison between the original/integer design |        |                      |        |
|------------------------------------------------|--------|----------------------|--------|
| on binary endpoints (unstratified design)      |        |                      |        |
| Sample size                                    | Z      | Information fraction | Power  |
| Original design                                |        |                      |        |
| 1,243.3070                                     | 2.9626 | 0.5000               | 0.2598 |
| 1,989.2912                                     | 2.2662 | 0.8000               | 0.7501 |
| 2,486.6140                                     | 2.0278 | 1.0000               | 0.9000 |
| Integer design                                 |        |                      |        |
| 1,243.0000                                     | 2.9626 | 0.4996               | 0.2597 |
| 1,989.0000                                     | 2.2662 | 0.7994               | 0.7500 |
| 2,488.0000                                     | 2.0280 | 1.0000               | 0.9001 |

### Survival outcome

``` r
x <- gs_design_ahr(
  analysis_time = c(12, 24, 36),
  upper = gs_spending_bound,
  lower = gs_b,
  upar = list(sf = gsDesign::sfLDOF, timing = 1:3 / 3, total_spend = 0.025, param = NULL),
  lpar = rep(-Inf, 3)
)
xi <- x |> to_integer()
```

Notice that with the integer design, the (i) number of events, (ii)
sample size, (iii) power, (iv) information fraction will be different.

``` r
tibble(
  Design = rep(c("Original design", "Integer design"), each = 3),
  Events = c(x$analysis$event, xi$analysis$event),
  `Sample size` = c(x$analysis$n, xi$analysis$n),
  Z = c(
    (x$bound |> filter(bound == "upper"))$z,
    (xi$bound |> filter(bound == "upper"))$z
  ),
  `Information fraction` = c(x$analysis$info_frac, xi$analysis$info_frac),
  Power = c(
    (x$bound |> filter(bound == "upper"))$probability,
    (xi$bound |> filter(bound == "upper"))$probability
  )
) |>
  group_by(Design) |>
  gt() |>
  tab_header(
    title = "Comparison between the original/integer design",
    subtitle = "on survival endpoints (unstratified design)"
  ) |>
  fmt_number(columns = 2:5, decimals = 4)
```

| Comparison between the original/integer design |             |        |                      |             |
|------------------------------------------------|-------------|--------|----------------------|-------------|
| on survival endpoints (unstratified design)    |             |        |                      |             |
| Events                                         | Sample size | Z      | Information fraction | Power       |
| Original design                                |             |        |                      |             |
| 91.9812                                        | 405.7098    | 3.7103 | 0.3091               | 0.003627376 |
| 221.2005                                       | 486.8518    | 2.5122 | 0.7376               | 0.481734802 |
| 298.6001                                       | 486.8518    | 1.9828 | 1.0000               | 0.900000000 |
| Integer design                                 |             |        |                      |             |
| 92.0000                                        | 406.0000    | 3.7103 | 0.3088               | 0.003620194 |
| 221.0000                                       | 488.0000    | 2.5122 | 0.7360               | 0.479605447 |
| 299.0000                                       | 488.0000    | 1.9830 | 1.0000               | 0.900134291 |

## Stratified design

``` r
x <- gs_design_rd(
  p_c = tibble(
    stratum = c("biomarker positive", "biomarker negative"),
    rate = c(0.2, 0.25)
  ),
  p_e = tibble(
    stratum = c("biomarker positive", "biomarker negative"),
    rate = c(0.15, 0.22)
  ),
  info_frac = c(0.7, 1),
  rd0 = 0,
  alpha = 0.025,
  beta = 0.1,
  ratio = 1,
  stratum_prev = tibble(
    stratum = c("biomarker positive", "biomarker negative"),
    prevalence = c(0.4, 0.6)
  ),
  weight = "ss",
  upper = gs_spending_bound,
  lower = gs_b,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = c(0.7, 1)),
  lpar = rep(-Inf, 2)
)

xi <- x |> to_integer()
```

Note that in the original design, the sample size is 3426.1318255,
4894.4740364, and in the integer design, the sample size is updated to
3426, 4896. For the 2 interim analysis, we floor to the closet
multiplier of 2, since the randomization ratio is 1. At the final
analysis, we ceiling the sample size from 4894.4740364 to 4896 and also
make sure the integer sample size is a multiplier of 2.

``` r
tibble(
  Design = rep(c("Original design", "Integer design"), each = 2),
  `Sample size` = c(x$analysis$n, xi$analysis$n),
  Z = c(
    (x$bound |> filter(bound == "upper"))$z,
    (xi$bound |> filter(bound == "upper"))$z
  ),
  `Information fraction` = c(x$analysis$info_frac, xi$analysis$info_frac),
  Power = c(
    (x$bound |> filter(bound == "upper"))$probability,
    (xi$bound |> filter(bound == "upper"))$probability
  )
) |>
  group_by(Design) |>
  gt() |>
  tab_header(
    title = "Comparison between the original/integer design",
    subtitle = "on binary endpoints (unstratified design)"
  ) |>
  fmt_number(columns = 2:5, decimals = 4)
```

| Comparison between the original/integer design |        |                      |        |
|------------------------------------------------|--------|----------------------|--------|
| on binary endpoints (unstratified design)      |        |                      |        |
| Sample size                                    | Z      | Information fraction | Power  |
| Original design                                |        |                      |        |
| 3,426.1318                                     | 2.4380 | 0.7000               | 0.6161 |
| 4,894.4740                                     | 1.9999 | 1.0000               | 0.9000 |
| Integer design                                 |        |                      |        |
| 3,426.0000                                     | 2.4380 | 0.6998               | 0.6160 |
| 4,896.0000                                     | 2.0000 | 1.0000               | 0.9001 |
