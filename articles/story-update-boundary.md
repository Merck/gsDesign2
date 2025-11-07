# Efficacy and futility boundary update

``` r
library(gsDesign2)
library(gt)
```

## Design assumptions

We assume two analyses: an interim analysis (IA) and a final analysis
(FA). The IA is planned 20 months after opening enrollment, followed by
the FA at month 36. The planned enrollment period spans 14 months, with
the first 2 months having an enrollment rate of 1/3 the final rate, the
next 2 months with a rate of 2/3 of the final rate, and the final rate
for the remaining 10 months. To obtain the targeted 90% power, these
rates will be multiplied by a constant. The control arm is assumed to
follow an exponential distribution with a median of 9 months and the
dropout rate is 0.0001 per month regardless of treatment group. Finally,
the experimental treatment group is piecewise exponential with a 3-month
delayed treatment effect; that is, in the first 3 months HR = 1 and the
HR is 0.6 thereafter.

``` r
alpha <- 0.0125
beta <- 0.1
ratio <- 1

# Enrollment
enroll_rate <- define_enroll_rate(
  duration = c(2, 2, 10),
  rate = (1:3) / 3
)

# Failure and dropout
fail_rate <- define_fail_rate(
  duration = c(3, Inf),
  fail_rate = log(2) / 9,
  hr = c(1, 0.6),
  dropout_rate = .0001
)
# IA and FA analysis time
analysis_time <- c(20, 36)

# Randomization ratio
ratio <- 1
```

We use the null hypothesis information for boundary crossing probability
calculations under both the null and alternate hypotheses. This will
also imply the null hypothesis information will be used for the
information fraction used in spending functions to derive the design.

``` r
info_scale <- "h0_info"
```

## One-sided design

For the design, we have efficacy bounds at both the IA and FA. We use
the Lan and DeMets (1983) spending function with a total alpha of
0.0125, which approximates an O’Brien-Fleming bound.

``` r
upper <- gs_spending_bound
upar <- list(sf = gsDesign::sfLDOF, total_spend = alpha, param = NULL)

x <- gs_design_ahr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  alpha = alpha,
  beta = beta,
  info_frac = NULL,
  info_scale = "h0_info",
  analysis_time = analysis_time,
  ratio = ratio,
  upper = gs_spending_bound,
  upar = upar,
  test_upper = TRUE,
  lower = gs_b,
  lpar = rep(-Inf, 2),
  test_lower = FALSE
) |> to_integer()
```

The planned design targets:

- Planned events: 227, 349
- Planned information fraction for interim and final analysis: 0.6504, 1
- Planned alpha spending: 0.0054, 0.025
- Planned efficacy bounds: 2.8853, 2.2611

We note that rounding up the final targeted events increases power
slightly over the targeted 90%.

``` r
x |>
  summary() |>
  as_gt() |>
  tab_header(title = "Planned design")
```

[TABLE]

### Bounds for alternate alpha

At the stage of study design, we may be required to report the designs
under multiple \\\alpha\\ if alpha is reallocated due to rejection of
another hypothesis. At the design stage, the planned \\\alpha\\ is
0.0125. Assume the updated \\\alpha\\ is 0.025 due to reallocation of
\\\alpha\\ from some other hypothesis. The corresponding bounds are

``` r
gs_update_ahr(
  x = x,
  alpha = 0.025
  ) |>
  summary(col_decimals = c(z = 4)) |>
  as_gt(title = "Updated design",
        subtitle = "For alternate alpha = 0.025")
```

[TABLE]

The above updated boundaries utilize the planned treatment effect and
the planned statistical information under null hypothesis, considering
the original design has `info_scale = "h0_info"`.

### Updating bounds with observed events at time of analyses

The planned events at interim analysis and final analysis are 227, 349.
However, at the time of each analysis, the observed events may differ
from the planned events due to using calendar-based cutoffs. For
example, we assume that 240 and 352 events were observed at IA and FA,
respectively. At the interim analysis, 30 events were observed during
the delayed effect period, At the final analysis, 32 events were
observed during the delayed effect period. In practice, even if
attempting to match event counts exactly the observed events at analyses
often differ from planned. We also assume the protocol specifies that
the full \\\alpha\\ will be spent at the final analysis even in a case
like this when there is a shortfall of events versus the design plan.

The updated design is

``` r
# Set spending fraction for interim according to observed events 
# divided by planned final events.
# Final spending fraction is 1 per plan even if there is a shortfall
# of events versus planned (as specified above)
# Update bound
gs_update_ahr(
  x = x,
  ustime = c(240 / max(x$analysis$event), 1),
  event_tbl = data.frame(analysis = c(1, 1, 2, 2), 
                         event = c(30, 210, 32, 320))
) |>
  summary(col_decimals = c(z = 4)) |>
  as_gt(title = "Updated design",
        subtitle = paste0("With observed 240 events at IA and 352 events at FA"))
```

[TABLE]

## Two-sided asymmetric design, beta-spending with non-binding lower bound

In this section, we investigate a 2 sided asymmetric design, with a
non-binding \\\beta\\-spending used to generate futility bounds.
\\\beta\\-spending refers to Type II error (1 - power) spending for the
lower bound crossing probabilities under the alternative hypothesis.
Non-binding bound computation assumes the trial continues if the lower
bound is crossed for Type I error, but not Type II error.

In the original designs, we employ the Lan-DeMets spending function used
to approximate O’Brien-Fleming bounds (Lan and DeMets 1983) for both
efficacy and futility bounds. The total spending for efficacy is 0.0125,
and for futility is 0.1. In addition, we assume there is no futility
test for the final analysis.

``` r
# Upper and lower bounds uses spending with Lan-DeMets spending approximating
# O'Brien-Fleming bound
upper <- gs_spending_bound
upar <- list(sf = gsDesign::sfLDOF, total_spend = alpha, param = NULL)
lower <- gs_spending_bound
lpar <- list(sf = gsDesign::sfLDOF, total_spend = beta, param = NULL)

x <- gs_design_ahr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  alpha = alpha,
  beta = beta,
  info_frac = NULL,
  info_scale = "h0_info",
  analysis_time = c(20, 36),
  ratio = ratio,
  upper = gs_spending_bound,
  upar = upar,
  test_upper = TRUE,
  lower = lower,
  lpar = lpar,
  test_lower = c(TRUE, FALSE),
  binding = FALSE
) |> to_integer()
```

In the planned design, we have

- Planned events: 237, 363
- Planned information fraction (timing): 0.6529, 1
- Planned alpha spending: 0.005538, 0.025
- Planned efficacy bounds: 2.8791, 2.2615
- Planned futility bounds: 0.6754

Since we added futility bounds, the sample size and number of events are
larger than we had above in the 1-sided example.

``` r
x |>
  summary() |>
  as_gt() |>
  tab_header(title = "Planned design",
             subtitle = "2-sided asymmetric design, non-binding futility")
```

[TABLE]

### Bounds for alternate alpha

We may want to report the design bounds under multiple \\\alpha\\ in the
case Type I error may be reallocated from another hypothesis. We assume
now that \\\alpha\\ is 0.025 but we still use the same sample size and
event timing as for the original alpha = 0.0125. The updated bounds are

``` r
gs_update_ahr(
  x = x,
  alpha = 0.025
  ) |>
  summary(col_decimals = c(z = 4)) |>
  as_gt(title = "Updated design",
        subtitle = "For alpha = 0.025")
```

[TABLE]

### Updating bounds with observed events at time of analyses

We assume the observed events same as for the 1-sided example above.

The updated design is

``` r
# Update spending fraction as above
ustime <- c(240 / max(x$analysis$event), 1)

gs_update_ahr(
  x = x,
  ustime = ustime,
  # Spending fraction for futility bound same as for efficacy
  lstime = ustime, 
  event_tbl = data.frame(analysis = c(1, 1, 2, 2), 
                         event = c(30, 210, 32, 320))
  ) |>
  summary(col_decimals = c(z = 4)) |>
  as_gt(title = "Updated design",
        subtitle = paste0("With observed 240 events at IA and 352 events at FA"))
```

[TABLE]

## References

Lan, K. K. Gordon, and David L DeMets. 1983. “Discrete Sequential
Boundaries for Clinical Trials.” *Biometrika* 70 (3): 659–63.
