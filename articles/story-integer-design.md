# Integer designs

``` r

library(gsDesign)
library(gsDesign2)
library(tibble)
library(dplyr)
library(lt)
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
  lt() |>
  lt_group(~ Design, sep = TRUE) |>
  lt_header(
    title = "Comparison between the original/integer design",
    subtitle = "on binary endpoints (unstratified design)"
  ) |>
  lt_format(columns = 2:5, decimals = 4)
```

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
  lt() |>
  lt_group(~ Design, sep = TRUE) |>
  lt_header(
    title = "Comparison between the original/integer design",
    subtitle = "on survival endpoints (unstratified design)"
  ) |>
  lt_format(columns = 2:5, decimals = 4)
```

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
  lt() |>
  lt_group(~ Design, sep = TRUE) |>
  lt_header(
    title = "Comparison between the original/integer design",
    subtitle = "on binary endpoints (unstratified design)"
  ) |>
  lt_format(columns = 2:5, decimals = 4)
```
