# Reproducing gsSurv Schoenfeld bounds with harm bounds

``` r

library(gsDesign2)
library(dplyr)
library(gt)
```

## Overview

This vignette shows that
[`gs_design_ahr()`](https://merck.github.io/gsDesign2/reference/gs_design_ahr.md)
can reproduce the Z-boundaries from
`gsDesign::gsSurv(method = "Schoenfeld")` for a proportional hazards
design with a single hazard ratio. The Schoenfeld approximation uses the
null hypothesis variance, so the corresponding
[`gs_design_ahr()`](https://merck.github.io/gsDesign2/reference/gs_design_ahr.md)
calls use `info_scale = "h0_info"`.

The comparison covers
[`gsSurv()`](https://keaven.github.io/gsDesign//reference/nSurv.html)
`test.type` values 1 through 8. Types 7 and 8 include the additional
harm bound. We use the same spending functions, spending times, and
numerical integration settings in both packages.

## Design assumptions

We use a 3-analysis design with information fractions 35%, 70%, and
100%. Enrollment is uniform for 16 months, the total trial duration is
36 months, and the control arm has exponential failure with a 12-month
median. The experimental-to-control hazard ratio is constant at 0.7.

``` r

trial_duration <- 36
info_frac <- c(.35, .7, 1)

enroll_rate <- define_enroll_rate(duration = 16, rate = 1)
minfup <- trial_duration - sum(enroll_rate$duration)

fail_rate <- define_fail_rate(
  duration = Inf,
  fail_rate = log(2) / 12,
  hr = .7,
  dropout_rate = -log(.99) / 12
)

alpha <- 0.025
beta <- 0.15
astar_candidates <- c(.10, .15, .20)
astar <- .20
ratio <- 1
r <- 32
tol <- 1e-8
```

The `astar` argument is used for null-spending lower bounds in test
types 5 and 6 and for harm bounds in test types 7 and 8. For harm
bounds, `astar` controls the cumulative null probability of crossing a
lower-tail boundary, representing evidence that experimental treatment
is worse than control. We compare `astar` values of 0.10, 0.15, and 0.20
and select `astar = 0.20`. This choice is intentionally somewhat more
liberal than `astar = 0.15` because the harm bound is meant to warn of a
potentially important trend in the wrong direction, not to require
definitive evidence of harm before raising concern. It still keeps all
harm Z-bounds below 0.

``` r

candidate_harm_bounds <- lapply(astar_candidates, function(astar_candidate) {
  gs_harm <- gsDesign::gsSurv(
    k = length(info_frac),
    test.type = 7,
    alpha = alpha,
    beta = beta,
    astar = astar_candidate,
    timing = info_frac,
    T = trial_duration,
    minfup = minfup,
    lambdaC = fail_rate$fail_rate,
    eta = fail_rate$dropout_rate,
    hr = fail_rate$hr,
    ratio = ratio,
    sfu = gsDesign::sfHSD,
    sfupar = -4,
    sfl = gsDesign::sfHSD,
    sflpar = -2,
    sfharm = gsDesign::sfHSD,
    sfharmparam = -2,
    r = r,
    tol = tol,
    method = "Schoenfeld"
  )

  data.frame(
    astar = astar_candidate,
    analysis = seq_along(gs_harm$harm$bound),
    harm_z = as.numeric(gs_harm$harm$bound),
    harm_p_lower_tail = pnorm(as.numeric(gs_harm$harm$bound))
  )
}) |>
  do.call(what = rbind) |>
  mutate(
    analysis = factor(
      analysis,
      levels = seq_along(info_frac),
      labels = c(paste("IA", seq_len(length(info_frac) - 1)), "Final")
    )
  )

candidate_harm_bounds |>
  gt() |>
  fmt_number(
    columns = c("astar", "harm_z", "harm_p_lower_tail"),
    decimals = 3
  )
```

| astar | analysis | harm_z | harm_p_lower_tail |
|-------|----------|--------|-------------------|
| 0.100 | IA 1     | −2.148 | 0.016             |
| 0.100 | IA 2     | −1.751 | 0.040             |
| 0.100 | Final    | −1.376 | 0.084             |
| 0.150 | IA 1     | −1.981 | 0.024             |
| 0.150 | IA 2     | −1.544 | 0.061             |
| 0.150 | Final    | −1.123 | 0.131             |
| 0.200 | IA 1     | −1.856 | 0.032             |
| 0.200 | IA 2     | −1.385 | 0.083             |
| 0.200 | Final    | −0.921 | 0.179             |

## Boundary specifications

We use the same Hwang-Shih-DeCani spending functions in both packages.
The upper boundary is O’Brien-Fleming-like with `param = -4`. The lower
futility and harm boundaries use `param = -2`.

``` r

upper_par <- list(
  sf = gsDesign::sfHSD,
  total_spend = alpha,
  param = -4,
  timing = info_frac
)

lower_beta_par <- list(
  sf = gsDesign::sfHSD,
  total_spend = beta,
  param = -2,
  timing = info_frac
)

lower_null_par <- list(
  sf = gsDesign::sfHSD,
  total_spend = astar,
  param = -2,
  timing = info_frac
)

harm_par <- lower_null_par
```

## Design constructors

The
[`gsSurv()`](https://keaven.github.io/gsDesign//reference/nSurv.html)
constructor is direct. For
[`gs_design_ahr()`](https://merck.github.io/gsDesign2/reference/gs_design_ahr.md),
the mapping is:

- `test.type = 1`: one-sided efficacy only.
- `test.type = 2`: symmetric two-sided design; use the upper spending
  rule for the lower bound and set `h1_spending = FALSE`.
- `test.type = 3` and `4`: beta-spending futility, with binding and
  non-binding lower bounds, respectively.
- `test.type = 5` and `6`: null-spending futility, with binding and
  non-binding lower bounds, respectively.
- `test.type = 7` and `8`: beta-spending futility plus null-spending
  harm, with binding and non-binding lower/harm bounds, respectively.

``` r

make_gs_surv <- function(test_type) {
  gsDesign::gsSurv(
    k = length(info_frac),
    test.type = test_type,
    alpha = alpha,
    beta = beta,
    astar = astar,
    timing = info_frac,
    T = trial_duration,
    minfup = minfup,
    lambdaC = fail_rate$fail_rate,
    eta = fail_rate$dropout_rate,
    hr = fail_rate$hr,
    ratio = ratio,
    sfu = gsDesign::sfHSD,
    sfupar = -4,
    sfl = gsDesign::sfHSD,
    sflpar = -2,
    sfharm = gsDesign::sfHSD,
    sfharmparam = -2,
    r = r,
    tol = tol,
    method = "Schoenfeld"
  )
}

make_gs_design_ahr <- function(test_type) {
  args <- list(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    alpha = alpha,
    beta = beta,
    ratio = ratio,
    info_frac = info_frac,
    analysis_time = trial_duration,
    r = r,
    tol = tol,
    info_scale = "h0_info",
    upper = gs_spending_bound,
    upar = upper_par,
    lower = gs_b,
    lpar = rep(-Inf, length(info_frac)),
    test_lower = FALSE,
    harm = gs_b,
    hpar = rep(-Inf, length(info_frac)),
    test_harm = FALSE,
    binding = FALSE,
    h1_spending = TRUE
  )

  if (test_type == 2) {
    args$lower <- gs_spending_bound
    args$lpar <- upper_par
    args$test_lower <- TRUE
    args$binding <- TRUE
    args$h1_spending <- FALSE
  }

  if (test_type %in% 3:4) {
    args$lower <- gs_spending_bound
    args$lpar <- lower_beta_par
    args$test_lower <- TRUE
    args$binding <- test_type == 3
    args$h1_spending <- TRUE
  }

  if (test_type %in% 5:6) {
    args$lower <- gs_spending_bound
    args$lpar <- lower_null_par
    args$test_lower <- TRUE
    args$binding <- test_type == 5
    args$h1_spending <- FALSE
  }

  if (test_type %in% 7:8) {
    args$lower <- gs_spending_bound
    args$lpar <- lower_beta_par
    args$test_lower <- TRUE
    args$binding <- test_type == 7
    args$h1_spending <- TRUE
    args$harm <- gs_spending_bound
    args$hpar <- harm_par
    args$test_harm <- TRUE
  }

  do.call(gs_design_ahr, args)
}
```

## Bound comparison

We extract upper, lower, and harm Z-boundaries from both packages and
compare them analysis by analysis. Infinite bounds are not included.

``` r

extract_gs_surv_bounds <- function(x, test_type) {
  add_bound <- function(bound, z) {
    if (is.null(z)) {
      return(NULL)
    }

    z <- as.numeric(z)
    keep <- is.finite(z) & abs(z) < 20

    data.frame(
      test_type = test_type,
      analysis = seq_along(z)[keep],
      bound = bound,
      gsSurv = z[keep]
    )
  }

  do.call(
    rbind,
    Filter(
      Negate(is.null),
      list(
        add_bound("upper", x$upper$bound),
        if (test_type != 1) add_bound("lower", x$lower$bound),
        if (test_type %in% 7:8) add_bound("harm", x$harm$bound)
      )
    )
  )
}

extract_gs_design_ahr_bounds <- function(x, test_type) {
  x$bound |>
    transmute(
      test_type = test_type,
      analysis,
      bound,
      gs_design_ahr = z
    )
}

comparison <- lapply(1:8, function(test_type) {
  gs_surv <- make_gs_surv(test_type)
  gs_ahr <- make_gs_design_ahr(test_type)

  merge(
    extract_gs_surv_bounds(gs_surv, test_type),
    extract_gs_design_ahr_bounds(gs_ahr, test_type),
    by = c("test_type", "analysis", "bound"),
    all = TRUE
  )
}) |>
  do.call(what = rbind) |>
  mutate(
    difference = gs_design_ahr - gsSurv,
    abs_difference = abs(difference),
    bound = factor(bound, levels = c("upper", "lower", "harm"))
  ) |>
  arrange(test_type, analysis, bound)

stopifnot(max(comparison$abs_difference, na.rm = TRUE) < 1e-5)
stopifnot(all(comparison$gs_design_ahr[comparison$bound == "harm"] < 0))
```

The maximum absolute Z-boundary difference is below `1e-5` for every
test type. All harm bounds in the selected design are negative.

``` r

test_type_labels <- data.frame(
  test_type = 1:8,
  description = c(
    "One-sided efficacy",
    "Two-sided symmetric",
    "Beta-spending futility, binding",
    "Beta-spending futility, non-binding",
    "Null-spending futility, binding",
    "Null-spending futility, non-binding",
    "Binding futility and harm",
    "Non-binding futility and harm"
  )
)

comparison |>
  group_by(test_type) |>
  summarize(max_abs_z_difference = max(abs_difference), .groups = "drop") |>
  left_join(test_type_labels, by = "test_type") |>
  select(test_type, description, max_abs_z_difference) |>
  gt() |>
  fmt_scientific(columns = max_abs_z_difference, decimals = 2)
```

| test_type | description                         | max_abs_z_difference |
|-----------|-------------------------------------|----------------------|
| 1         | One-sided efficacy                  | 6.70 × 10⁻⁷          |
| 2         | Two-sided symmetric                 | 6.70 × 10⁻⁷          |
| 3         | Beta-spending futility, binding     | 6.68 × 10⁻⁷          |
| 4         | Beta-spending futility, non-binding | 6.70 × 10⁻⁷          |
| 5         | Null-spending futility, binding     | 6.70 × 10⁻⁷          |
| 6         | Null-spending futility, non-binding | 6.70 × 10⁻⁷          |
| 7         | Binding futility and harm           | 6.68 × 10⁻⁷          |
| 8         | Non-binding futility and harm       | 6.70 × 10⁻⁷          |

The detailed Z-boundary comparison is shown below.

``` r

comparison |>
  left_join(test_type_labels, by = "test_type") |>
  mutate(
    test_type = paste0("test.type ", test_type, ": ", description),
    bound = as.character(bound)
  ) |>
  select(-description) |>
  gt(groupname_col = "test_type") |>
  fmt_number(columns = c(gsSurv, gs_design_ahr), decimals = 6) |>
  fmt_scientific(columns = c(difference, abs_difference), decimals = 2)
```

| analysis | bound | gsSurv | gs_design_ahr | difference | abs_difference |
|----|----|----|----|----|----|
| test.type 1: One-sided efficacy |  |  |  |  |  |
| 1 | upper | 2.983459 | 2.983459 | 0.00 | 0.00 |
| 2 | upper | 2.492116 | 2.492117 | 6.70 × 10⁻⁷ | 6.70 × 10⁻⁷ |
| 3 | upper | 2.003326 | 2.003327 | 3.49 × 10⁻⁷ | 3.49 × 10⁻⁷ |
| test.type 2: Two-sided symmetric |  |  |  |  |  |
| 1 | upper | 2.983459 | 2.983459 | 0.00 | 0.00 |
| 1 | lower | −2.983459 | −2.983459 | 0.00 | 0.00 |
| 2 | upper | 2.492116 | 2.492117 | 6.70 × 10⁻⁷ | 6.70 × 10⁻⁷ |
| 2 | lower | −2.492116 | −2.492117 | −6.70 × 10⁻⁷ | 6.70 × 10⁻⁷ |
| 3 | upper | 2.003326 | 2.003327 | 3.78 × 10⁻⁷ | 3.78 × 10⁻⁷ |
| 3 | lower | −2.003326 | −2.003327 | −3.78 × 10⁻⁷ | 3.78 × 10⁻⁷ |
| test.type 3: Beta-spending futility, binding |  |  |  |  |  |
| 1 | upper | 2.983459 | 2.983459 | 0.00 | 0.00 |
| 1 | lower | −0.165324 | −0.165324 | 2.66 × 10⁻⁷ | 2.66 × 10⁻⁷ |
| 2 | upper | 2.491541 | 2.491542 | 6.68 × 10⁻⁷ | 6.68 × 10⁻⁷ |
| 2 | lower | 1.023887 | 1.023887 | 2.24 × 10⁻⁷ | 2.24 × 10⁻⁷ |
| 3 | upper | 1.964627 | 1.964627 | 3.95 × 10⁻⁷ | 3.95 × 10⁻⁷ |
| 3 | lower | 1.964627 | 1.964627 | 2.82 × 10⁻⁷ | 2.82 × 10⁻⁷ |
| test.type 4: Beta-spending futility, non-binding |  |  |  |  |  |
| 1 | upper | 2.983459 | 2.983459 | 0.00 | 0.00 |
| 1 | lower | −0.143809 | −0.143809 | 2.44 × 10⁻⁷ | 2.44 × 10⁻⁷ |
| 2 | upper | 2.492116 | 2.492117 | 6.70 × 10⁻⁷ | 6.70 × 10⁻⁷ |
| 2 | lower | 1.054330 | 1.054331 | 1.92 × 10⁻⁷ | 1.92 × 10⁻⁷ |
| 3 | upper | 2.003326 | 2.003327 | 3.49 × 10⁻⁷ | 3.49 × 10⁻⁷ |
| 3 | lower | 2.003326 | 2.003327 | 2.47 × 10⁻⁷ | 2.47 × 10⁻⁷ |
| test.type 5: Null-spending futility, binding |  |  |  |  |  |
| 1 | upper | 2.983459 | 2.983459 | 0.00 | 0.00 |
| 1 | lower | −1.855898 | −1.855898 | 0.00 | 0.00 |
| 2 | upper | 2.492116 | 2.492117 | 6.70 × 10⁻⁷ | 6.70 × 10⁻⁷ |
| 2 | lower | −1.384626 | −1.384626 | −8.58 × 10⁻⁸ | 8.58 × 10⁻⁸ |
| 3 | upper | 2.003310 | 2.003310 | 3.49 × 10⁻⁷ | 3.49 × 10⁻⁷ |
| 3 | lower | −0.920673 | −0.920673 | −5.03 × 10⁻⁹ | 5.03 × 10⁻⁹ |
| test.type 6: Null-spending futility, non-binding |  |  |  |  |  |
| 1 | upper | 2.983459 | 2.983459 | 0.00 | 0.00 |
| 1 | lower | −1.855898 | −1.855898 | 0.00 | 0.00 |
| 2 | upper | 2.492116 | 2.492117 | 6.70 × 10⁻⁷ | 6.70 × 10⁻⁷ |
| 2 | lower | −1.384626 | −1.384626 | −3.81 × 10⁻⁸ | 3.81 × 10⁻⁸ |
| 3 | upper | 2.003326 | 2.003327 | 3.49 × 10⁻⁷ | 3.49 × 10⁻⁷ |
| 3 | lower | −0.920673 | −0.920673 | 1.48 × 10⁻⁸ | 1.48 × 10⁻⁸ |
| test.type 7: Binding futility and harm |  |  |  |  |  |
| 1 | upper | 2.983459 | 2.983459 | 0.00 | 0.00 |
| 1 | lower | −0.165324 | −0.165324 | 2.66 × 10⁻⁷ | 2.66 × 10⁻⁷ |
| 1 | harm | −1.855898 | −1.855898 | 0.00 | 0.00 |
| 2 | upper | 2.491541 | 2.491542 | 6.68 × 10⁻⁷ | 6.68 × 10⁻⁷ |
| 2 | lower | 1.023887 | 1.023887 | 2.24 × 10⁻⁷ | 2.24 × 10⁻⁷ |
| 2 | harm | −1.384626 | −1.384626 | −3.81 × 10⁻⁸ | 3.81 × 10⁻⁸ |
| 3 | upper | 1.964627 | 1.964627 | 3.95 × 10⁻⁷ | 3.95 × 10⁻⁷ |
| 3 | lower | 1.964627 | 1.964627 | 2.82 × 10⁻⁷ | 2.82 × 10⁻⁷ |
| 3 | harm | −0.920673 | −0.920673 | 1.48 × 10⁻⁸ | 1.48 × 10⁻⁸ |
| test.type 8: Non-binding futility and harm |  |  |  |  |  |
| 1 | upper | 2.983459 | 2.983459 | 0.00 | 0.00 |
| 1 | lower | −0.143809 | −0.143809 | 2.44 × 10⁻⁷ | 2.44 × 10⁻⁷ |
| 1 | harm | −1.855898 | −1.855898 | 0.00 | 0.00 |
| 2 | upper | 2.492116 | 2.492117 | 6.70 × 10⁻⁷ | 6.70 × 10⁻⁷ |
| 2 | lower | 1.054330 | 1.054331 | 1.92 × 10⁻⁷ | 1.92 × 10⁻⁷ |
| 2 | harm | −1.384626 | −1.384626 | −3.81 × 10⁻⁸ | 3.81 × 10⁻⁸ |
| 3 | upper | 2.003326 | 2.003327 | 3.49 × 10⁻⁷ | 3.49 × 10⁻⁷ |
| 3 | lower | 2.003326 | 2.003327 | 2.47 × 10⁻⁷ | 2.47 × 10⁻⁷ |
| 3 | harm | −0.920673 | −0.920673 | 1.48 × 10⁻⁸ | 1.48 × 10⁻⁸ |

## Sample size and event counts

Test types 7 and 8 add a harm bound to the corresponding beta-spending
futility designs in test types 3 and 4. With the current specification,
the harm bound is below the futility bound at each analysis, and adding
the harm bound does not change the efficacy bound, futility bound,
sample size, or event count.

``` r

harm_effect_comparison <- lapply(
  list(
    c(futility_only = 3, with_harm = 7),
    c(futility_only = 4, with_harm = 8)
  ),
  function(test_types) {
    futility_only <- make_gs_surv(test_types[["futility_only"]])
    with_harm <- make_gs_surv(test_types[["with_harm"]])

    data.frame(
      comparison = paste0(
        "test.type ", test_types[["futility_only"]],
        " vs test.type ", test_types[["with_harm"]]
      ),
      max_upper_z_difference = max(abs(with_harm$upper$bound - futility_only$upper$bound)),
      max_futility_z_difference = max(abs(with_harm$lower$bound - futility_only$lower$bound)),
      final_n_difference = tail(as.numeric(with_harm$eNC + with_harm$eNE), 1) -
        tail(as.numeric(futility_only$eNC + futility_only$eNE), 1),
      final_events_difference = tail(with_harm$n.I, 1) - tail(futility_only$n.I, 1),
      largest_harm_z = max(with_harm$harm$bound)
    )
  }
) |>
  do.call(what = rbind)

harm_effect_comparison |>
  gt() |>
  fmt_number(
    columns = c(
      max_upper_z_difference,
      max_futility_z_difference,
      final_n_difference,
      final_events_difference,
      largest_harm_z
    ),
    decimals = 6
  )
```

| comparison | max_upper_z_difference | max_futility_z_difference | final_n_difference | final_events_difference | largest_harm_z |
|----|----|----|----|----|----|
| test.type 3 vs test.type 7 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | −0.920673 |
| test.type 4 vs test.type 8 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | −0.920673 |

Thus, the sample size and event count changes seen for test types 7 and
8 relative to some other test types are inherited from their underlying
beta-spending futility designs, not from the addition of the harm bound.

The Z-boundaries match closely, but the displayed sample sizes and event
counts from
[`gsSurv()`](https://keaven.github.io/gsDesign//reference/nSurv.html)
and
[`gs_design_ahr()`](https://merck.github.io/gsDesign2/reference/gs_design_ahr.md)
are not identical. The table below compares final analysis values from
the two packages.

``` r

sample_size_comparison <- lapply(1:8, function(test_type) {
  gs_surv <- make_gs_surv(test_type)
  gs_ahr <- make_gs_design_ahr(test_type)

  data.frame(
    test_type = test_type,
    analysis = seq_along(info_frac),
    n_gsSurv = as.numeric(gs_surv$eNC + gs_surv$eNE),
    n_gs_design_ahr = gs_ahr$analysis$n,
    events_gsSurv = as.numeric(gs_surv$n.I),
    events_gs_design_ahr = gs_ahr$analysis$event,
    time_gsSurv = as.numeric(gs_surv$T),
    time_gs_design_ahr = gs_ahr$analysis$time
  )
}) |>
  do.call(what = rbind) |>
  mutate(
    n_difference = n_gs_design_ahr - n_gsSurv,
    events_difference = events_gs_design_ahr - events_gsSurv,
    n_percent_difference = 100 * n_difference / n_gsSurv,
    events_percent_difference = 100 * events_difference / events_gsSurv
  )

sample_size_comparison |>
  filter(analysis == length(info_frac)) |>
  left_join(test_type_labels, by = "test_type") |>
  select(
    test_type,
    description,
    n_gsSurv,
    n_gs_design_ahr,
    n_difference,
    events_gsSurv,
    events_gs_design_ahr,
    events_difference,
    events_percent_difference
  ) |>
  gt() |>
  fmt_number(
    columns = c(
      n_gsSurv,
      n_gs_design_ahr,
      n_difference,
      events_gsSurv,
      events_gs_design_ahr,
      events_difference
    ),
    decimals = 2
  ) |>
  fmt_number(columns = events_percent_difference, decimals = 3)
```

| test_type | description | n_gsSurv | n_gs_design_ahr | n_difference | events_gsSurv | events_gs_design_ahr | events_difference | events_percent_difference |
|----|----|----|----|----|----|----|----|----|
| 1 | One-sided efficacy | 395.16 | 397.96 | 2.80 | 287.12 | 289.15 | 2.04 | 0.710 |
| 2 | Two-sided symmetric | 395.16 | 397.96 | 2.80 | 287.12 | 289.15 | 2.04 | 0.710 |
| 3 | Beta-spending futility, binding | 407.56 | 410.46 | 2.89 | 296.13 | 298.23 | 2.10 | 0.710 |
| 4 | Beta-spending futility, non-binding | 417.28 | 420.24 | 2.96 | 303.19 | 305.34 | 2.15 | 0.710 |
| 5 | Null-spending futility, binding | 395.16 | 397.97 | 2.80 | 287.12 | 289.16 | 2.04 | 0.710 |
| 6 | Null-spending futility, non-binding | 395.17 | 397.97 | 2.80 | 287.12 | 289.16 | 2.04 | 0.710 |
| 7 | Binding futility and harm | 407.56 | 410.46 | 2.89 | 296.13 | 298.23 | 2.10 | 0.710 |
| 8 | Non-binding futility and harm | 417.28 | 420.24 | 2.96 | 303.19 | 305.34 | 2.15 | 0.710 |

The differences are a reporting conversion issue rather than a boundary
issue. For this proportional hazards example with 1:1 randomization, the
Schoenfeld null-variance information rate is exactly events / 4. The AHR
calculation also computes information under the alternative hypothesis.
Because the experimental arm has fewer events when the hazard ratio is
0.7, the AHR information per event under the alternative is slightly
smaller.

``` r

information_rate <- gs_info_ahr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  ratio = ratio,
  analysis_time = trial_duration
)

information_rate_comparison <- data.frame(
  quantity = c(
    "H1 AHR information per event",
    "H0/Schoenfeld information per event",
    "H0 divided by H1 information per event"
  ),
  value = c(
    information_rate$info / information_rate$event,
    information_rate$info0 / information_rate$event,
    (information_rate$info0 / information_rate$event) /
      (information_rate$info / information_rate$event)
  )
)

information_rate_comparison |>
  gt() |>
  fmt_number(columns = value, decimals = 6)
```

| quantity                               | value    |
|----------------------------------------|----------|
| H1 AHR information per event           | 0.248238 |
| H0/Schoenfeld information per event    | 0.250000 |
| H0 divided by H1 information per event | 1.007097 |

Thus both packages agree on the required H0 information target for the
Schoenfeld design. The current
[`gs_design_ahr()`](https://merck.github.io/gsDesign2/reference/gs_design_ahr.md)
output converts that H0 information target back to reported events and
sample size using the AHR alternative-hypothesis information rate. This
makes the displayed
[`gs_design_ahr()`](https://merck.github.io/gsDesign2/reference/gs_design_ahr.md)
event counts and sample sizes about 0.71% larger than the corresponding
[`gsSurv()`](https://keaven.github.io/gsDesign//reference/nSurv.html)
values in this example. Making these reported counts align exactly is a
display/conversion issue that can be addressed separately from the
boundary comparison.

## Interpretation

For a single hazard ratio,
[`gs_design_ahr()`](https://merck.github.io/gsDesign2/reference/gs_design_ahr.md)
with `info_scale = "h0_info"` matches `gsSurv(method = "Schoenfeld")`
boundaries to numerical integration tolerance. The harm-bound test types
are obtained by using the usual beta-spending lower bound for futility
and an additional null-spending lower-tail bound through the `harm`,
`hpar`, and `test_harm` arguments. With `astar = 0.20`, the harm
boundary represents an interim lower-tail evidence threshold that
experimental treatment may be worse than control, while keeping the harm
Z-boundaries below 0.
