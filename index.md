# gsDesign2

## Objective

The goal of gsDesign2 is to enable fixed or group sequential design
under non-proportional hazards. Piecewise constant enrollment, failure
rates and dropout rates for a stratified population are available to
enable highly flexible enrollment, time-to-event and time-to-dropout
assumptions. Substantial flexibility on top of what is in the gsDesign
package is intended for selecting boundaries. Comments on usability and
features are encouraged as this is still a young package.

## Installation

Install the released version of gsDesign2 from CRAN:

``` r
install.packages("gsDesign2")
```

Or install the development version from GitHub with:

``` r
remotes::install_github("Merck/gsDesign2")
```

## Use cases

### Step 1: specifying enrollment and failure rates

This is a basic example which shows you how to solve a common problem.
We assume there is a 4 month delay in treatment effect. Specifically, we
assume a hazard ratio of 1 for 4 months and 0.6 thereafter. For this
example we assume an exponential failure rate and low exponential
dropout rate. The `enroll_rate` specification indicates an expected
enrollment duration of 12 months with exponential inter-arrival times.

``` r
library(gsDesign2)

# Basic example

# Constant enrollment over 12 months
# Rate will be adjusted later by gsDesign2 NPH to get sample size
enroll_rate <- define_enroll_rate(duration = 12, rate = 1)

# 12 month median exponential failure rate in control
# 4 month delay in effect with HR=0.6 after
# Low exponential dropout rate
median_surv <- 12
fail_rate <- define_fail_rate(
  duration = c(4, Inf),
  fail_rate = log(2) / median_surv,
  hr = c(1, .6),
  dropout_rate = .001
)
```

The resulting failure rate specification is the following table. As many
rows and strata as needed can be specified to approximate whatever
patterns you wish.

``` r
fail_rate |> gt::gt()
```

| stratum | duration | fail_rate  | dropout_rate | hr  |
|---------|----------|------------|--------------|-----|
| All     | 4        | 0.05776227 | 0.001        | 1.0 |
| All     | Inf      | 0.05776227 | 0.001        | 0.6 |

### Step 2: derive a fixed design with no interim analyses

Computing a fixed sample size design with 2.5% one-sided Type I error
and 90% power. We specify a trial duration of 36 months with
`analysis_time`. Enrollment duration is the sum of
`enroll_rate$duration`. We used `fixed_design()` since there is a single
analysis:

``` r
fd <- fixed_design_ahr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  alpha = 0.025,
  power = 0.9,
  study_duration = 36,
  ratio = 1 # Experimental/control randomization ratio
)
```

The input enrollment rates have now been scaled to achieve power:

``` r
fd$enroll_rate |> gt::gt()
```

| stratum | duration | rate     |
|---------|----------|----------|
| All     | 12       | 35.05288 |

The failure and dropout rates remain unchanged from what was input. The
summary is obtained below. The columns are:

- `Design`: sample size derivation method.
- `N`: sample size; generally you will round up to an even number.
- `Event`: generally you will round up.
- `Bound`: Z value for efficacy; this is the inverse normal from 1 -
  alpha.
- `alpha`: 1-sided alpha level for testing.
- `Power`: power corresponding to enrollment, failure rate, and trial
  targeted events.

``` r
fd |>
  summary() |>
  as_gt()
```

| Fixed Design under AHR Method¹                     |          |          |      |          |       |       |
|----------------------------------------------------|----------|----------|------|----------|-------|-------|
| Design                                             | N        | Events   | Time | Bound    | alpha | Power |
| Average hazard ratio                               | 420.6346 | 311.0028 | 36   | 1.959964 | 0.025 | 0.9   |
| ¹ Power computed with average hazard ratio method. |          |          |      |          |       |       |

### Step 3: group sequential design

We provide a simple example for a group sequential design that
demonstrates a couple of features not available in the gsDesign package.
The first is specifying analysis times by calendar time rather than
information fraction. The second is not having an efficacy and futility
bound at each analysis. This is in addition to having methods for
non-proportional hazards as demonstrated in the fixed design above and
again here.

We use an O’Brien-Fleming spending function to derive our efficacy
bounds at 24 and 36 months. For futility, we simply require a nominally
significant trend in the wrong direction (\\p \< 0.1\\) after 8 months,
a trend in favor of experimental treatment after 14 months (\\Z \> 0\\)
and no bound later (\\Z = -\infty\\). Thus, we have two efficacy
analyses and two separate, earlier futility analysis. Power is set to
80% due to the somewhat aggressive futility bounds that are used for
safety (analysis 1 half way through enrollment) and proof of concept
(analysis 2). Such aggressive futility bounds may be desirable when a
previous proof of concept for experimental treatment has not been
established; essentially, this becomes a Phase II/III design with an
interim evaluation of appropriate efficacy trends before completing the
trial.

``` r
gsd <- gs_design_ahr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  ratio = 1,
  alpha = 0.025,
  beta = 0.2, # 80% power; enables aggressive futility bound specified
  analysis_time = c(8, 14, 24, 36),
  binding = FALSE, # Non-binding futility bound
  upper = gs_spending_bound, # Use spending bound for efficacy; total_spend is normally alpha
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  test_upper = c(FALSE, FALSE, TRUE, TRUE), # Only test efficacy after 1st analysis
  lower = gs_b, # Fixed Z-values will be provided for futility bound
  lpar = c(qnorm(0.1), 0, -Inf, -Inf)
)
```

Now we summarize the derived design. The summary table is further
described in the vignette [summarize group sequential designs in gt
tables](https://merck.github.io/gsDesign2/articles/story-summarize-designs.html).
Note that the design trend in favor of experimental treatment is very
minor at 8 months due to the delayed effect assumption used (see AHR at
analysis 1 in table). The design trend at 16 months is somewhat more
favorable when we are looking for HR \< 1 (favoring experimental
treatment) for a proof of concept. Actual bounds and timing selected for
a trial are situation dependent, but we hope the suggestions here are
provocative for what might be considered.

``` r
gsd |>
  summary() |>
  as_gt()
```

| Bound summary for AHR design                                                                                                                                                                                                                                                                                   |       |            |               |                                          |                 |
|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------|------------|---------------|------------------------------------------|-----------------|
| AHR approximations of ~HR at bound                                                                                                                                                                                                                                                                             |       |            |               |                                          |                 |
| Bound                                                                                                                                                                                                                                                                                                          | Z     | Nominal p¹ | ~HR at bound² | Cumulative boundary crossing probability |                 |
|                                                                                                                                                                                                                                                                                                                |       |            |               | Alternate hypothesis                     | Null hypothesis |
| Analysis: 1 Time: 8 N: 279.1 Event: 53.2 AHR: 0.91 Information fraction: 0.17                                                                                                                                                                                                                                  |       |            |               |                                          |                 |
| Futility                                                                                                                                                                                                                                                                                                       | -1.28 | 0.9000     | 1.4210        | 0.0539                                   | 0.1000          |
| Analysis: 2 Time: 14 N: 418.7 Event: 137.2 AHR: 0.82 Information fraction: 0.44                                                                                                                                                                                                                                |       |            |               |                                          |                 |
| Futility                                                                                                                                                                                                                                                                                                       | 0.00  | 0.5000     | 1.0000        | 0.1451                                   | 0.5091          |
| Analysis: 3 Time: 24 N: 418.7 Event: 238.4 AHR: 0.72 Information fraction: 0.76                                                                                                                                                                                                                                |       |            |               |                                          |                 |
| Efficacy                                                                                                                                                                                                                                                                                                       | 2.30  | 0.0106     | 0.7421        | 0.5582                                   | 0.0106          |
| Analysis: 4 Time: 36 N: 418.7 Event: 309.5 AHR: 0.69 Information fraction: 1                                                                                                                                                                                                                                   |       |            |               |                                          |                 |
| Efficacy                                                                                                                                                                                                                                                                                                       | 2.02  | 0.0219     | 0.7951        | 0.8000                                   | ³ 0.0244        |
| ¹ One-sided p-value for experimental vs control treatment. Value \< 0.5 favors experimental, \> 0.5 favors control.                                                                                                                                                                                            |       |            |               |                                          |                 |
| ² Approximate hazard ratio to cross bound.                                                                                                                                                                                                                                                                     |       |            |               |                                          |                 |
| ³ Cumulative alpha for final analysis (0.0244) is less than the full alpha (0.025) when the futility bound is non-binding. The smaller value subtracts the probability of crossing a futility bound before crossing an efficacy bound at a later analysis (0.025 - 0.0006 = 0.0244) under the null hypothesis. |       |            |               |                                          |                 |
