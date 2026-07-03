<!-- README.md is generated from README.Rmd. Please edit that file -->



# gsDesign2 <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/Merck/gsDesign2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Merck/gsDesign2/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/Merck/gsDesign2/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Merck/gsDesign2?branch=main)
[![CRAN status](https://www.r-pkg.org/badges/version/gsDesign2)](https://CRAN.R-project.org/package=gsDesign2)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/gsDesign2)](https://cran.r-project.org/package=gsDesign2)
<!-- badges: end -->

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
library(lt)

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
fail_rate |> lt()
```

<div class="lt-wrap"><table class="lt-table"><thead><tr><th scope="col">stratum</th><th scope="col" class="al-r">duration</th><th scope="col" class="al-r">fail rate</th><th scope="col" class="al-r">dropout rate</th><th scope="col" class="al-r">hr</th></tr></thead><tbody><tr><td>All</td><td class="al-r">4</td><td class="al-r" title="0.0577622650466621">0.0578</td><td class="al-r">0.001</td><td class="al-r" title="1">1.0</td></tr><tr><td>All</td><td class="al-r">∞</td><td class="al-r" title="0.0577622650466621">0.0578</td><td class="al-r">0.001</td><td class="al-r">0.6</td></tr></tbody></table></div>

### Step 2: derive a fixed design with no interim analyses

Computing a fixed sample size design with 2.5% one-sided Type I error
and 90% power. We specify a trial duration of 36 months with `analysis_time`.
Enrollment duration is the sum of `enroll_rate$duration`.
We used `fixed_design()` since there is a single analysis:


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
fd$enroll_rate |> lt()
```

<div class="lt-wrap"><table class="lt-table"><thead><tr><th scope="col">stratum</th><th scope="col" class="al-r">duration</th><th scope="col" class="al-r">rate</th></tr></thead><tbody><tr><td>All</td><td class="al-r">12</td><td class="al-r" title="35.0528819024983">35.05</td></tr></tbody></table></div>

The failure and dropout rates remain unchanged from what was input.
The summary is obtained below. The columns are:

- `Design`: sample size derivation method.
- `N`: sample size; generally you will round up to an even number.
- `Event`: generally you will round up.
- `Bound`: Z value for efficacy; this is the inverse normal from 1 - alpha.
- `alpha`: 1-sided alpha level for testing.
- `Power`: power corresponding to enrollment, failure rate, and
  trial targeted events.


``` r
fd |>
  summary() |>
  lt()
```

<div class="lt-wrap"><table class="lt-table"><thead><tr><th scope="col">Design</th><th scope="col" class="al-r">N</th><th scope="col" class="al-r">Events</th><th scope="col" class="al-r">Time</th><th scope="col" class="al-r">AHR</th><th scope="col" class="al-r">Bound</th><th scope="col" class="al-r">alpha</th><th scope="col" class="al-r">Power</th></tr></thead><tbody><tr><td>Average hazard ratio</td><td class="al-r" title="420.63458282998">420.6</td><td class="al-r" title="311.002757884978">311.0</td><td class="al-r">36</td><td class="al-r" title="0.691724419519974">0.6917</td><td class="al-r" title="1.95996398454005">1.960</td><td class="al-r">0.025</td><td class="al-r">0.9</td></tr></tbody></table></div>

### Step 3: group sequential design

We provide a simple example for a group sequential design that
demonstrates a couple of features not available in the gsDesign package.
The first is specifying analysis times by calendar time rather
than information fraction. The second is not having an efficacy and
futility bound at each analysis. This is in addition to having methods
for non-proportional hazards as demonstrated in the fixed design above
and again here.

We use an O'Brien-Fleming spending function to derive our efficacy
bounds at 24 and 36 months. For futility, we simply require a nominally
significant trend in the wrong direction ($p < 0.1$) after 8 months, a
trend in favor of experimental treatment after 14 months ($Z > 0$) and
no bound later ($Z = -\infty$). Thus, we have two efficacy analyses and
two separate, earlier futility analysis. Power is set to 80% due to the
somewhat aggressive futility bounds that are used for safety (analysis 1
half way through enrollment) and proof of concept (analysis 2). Such
aggressive futility bounds may be desirable when a previous proof of
concept for experimental treatment has not been established;
essentially, this becomes a Phase II/III design with an interim
evaluation of appropriate efficacy trends before completing the trial.


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
  test_upper = c(FALSE, TRUE, TRUE, TRUE), # Only test efficacy after 1st analysis
  lower = gs_b, # Fixed Z-values will be provided for futility bound
  lpar = c(qnorm(0.1), 0, -Inf, -Inf)
)
```

Now we summarize the derived design. The summary table is further described
in the vignette [summarize group sequential designs](https://merck.github.io/gsDesign2/articles/story-summarize-designs.html).
Note that the design trend in favor of experimental treatment
is very minor at 8 months due to the delayed effect assumption used
(see AHR at analysis 1 in table). The design trend at 16 months is somewhat
more favorable when we are looking for HR < 1 (favoring experimental
treatment) for a proof of concept. Actual bounds and timing selected for
a trial are situation dependent, but we hope the suggestions here are
provocative for what might be considered.


``` r
gsd |>
  summary() |>
  lt()
```

<div class="lt-wrap"><table class="lt-table"><thead><tr><th scope="col" class="lt-indent">Bound</th><th scope="col" class="al-r">Z</th><th scope="col" class="al-r">~HR at bound</th><th scope="col" class="al-r">Nominal p</th><th scope="col" class="al-r">Alternate hypothesis</th><th scope="col" class="al-r">Null hypothesis</th></tr></thead><tbody><tr class="lt-row-group"><th colspan="6" scope="colgroup">Analysis: 1 Time: 8 N: 279.3 Events: 53.3 AHR: 0.91 Information fraction: 0.17</th></tr><tr><td class="lt-indent">Futility</td><td class="al-r" title="-1.28">−1.28</td><td class="al-r" title="1.4208">1.421</td><td class="al-r" title="0.9">0.9000</td><td class="al-r">0.0539</td><td class="al-r" title="0.1">0.1000</td></tr><tr class="lt-row-group"><th colspan="6" scope="colgroup">Analysis: 2 Time: 14 N: 419 Events: 137.3 AHR: 0.82 Information fraction: 0.44</th></tr><tr><td class="lt-indent">Futility</td><td class="al-r" title="0">0.00</td><td class="al-r" title="1">1.000</td><td class="al-r" title="0.5">0.5000</td><td class="al-r" title="0.145">0.1450</td><td class="al-r">0.5091</td></tr><tr><td class="lt-indent">Efficacy</td><td class="al-r">3.17</td><td class="al-r" title="0.5821">0.582</td><td class="al-r">0.0008</td><td class="al-r" title="0.023">0.0230</td><td class="al-r">0.0008</td></tr><tr class="lt-row-group"><th colspan="6" scope="colgroup">Analysis: 3 Time: 24 N: 419 Events: 238.6 AHR: 0.72 Information fraction: 0.77</th></tr><tr><td class="lt-indent">Efficacy</td><td class="al-r">2.31</td><td class="al-r" title="0.7413">0.741</td><td class="al-r">0.0104</td><td class="al-r">0.5553</td><td class="al-r">0.0106</td></tr><tr class="lt-row-group"><th colspan="6" scope="colgroup">Analysis: 4 Time: 36 N: 419 Events: 309.8 AHR: 0.69 Information fraction: 1</th></tr><tr><td class="lt-indent">Efficacy</td><td class="al-r">2.02</td><td class="al-r" title="0.7951">0.795</td><td class="al-r">0.0218</td><td class="al-r" title="0.8">0.8000</td><td class="al-r">0.0244</td></tr></tbody></table></div>
