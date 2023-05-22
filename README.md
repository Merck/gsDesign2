
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gsDesign2 <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/Merck/gsDesign2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Merck/gsDesign2/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/Merck/gsDesign2/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Merck/gsDesign2?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/gsDesign2)](https://CRAN.R-project.org/package=gsDesign2)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/gsDesign2)](https://cran.r-project.org/package=gsDesign2)
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
library(gsDesign)
library(dplyr)
library(gt)

# Basic example

# Constant enrollment over 12 months
# Rate will be adjusted later by gsDesign2 NPH to get sample size
enroll_rate <- tibble::tibble(stratum = "All", duration = 12, rate = 1)

# 12 month median exponential failure rate in control
# 4 month delay in effect with HR=0.6 after
# Low exponential dropout rate
median_surv <- 12
fail_rate <- tibble::tibble(
  stratum = "All",
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
fail_rate %>%
  gt() %>%
  as_raw_html(inline_css = FALSE)
```

<div id="iohtwzwbzu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="stratum">stratum</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="duration">duration</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="fail_rate">fail_rate</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="hr">hr</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="dropout_rate">dropout_rate</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="stratum" class="gt_row gt_left">All</td>
<td headers="duration" class="gt_row gt_right">4</td>
<td headers="fail_rate" class="gt_row gt_right">0.05776227</td>
<td headers="hr" class="gt_row gt_right">1.0</td>
<td headers="dropout_rate" class="gt_row gt_right">0.001</td></tr>
    <tr><td headers="stratum" class="gt_row gt_left">All</td>
<td headers="duration" class="gt_row gt_right">Inf</td>
<td headers="fail_rate" class="gt_row gt_right">0.05776227</td>
<td headers="hr" class="gt_row gt_right">0.6</td>
<td headers="dropout_rate" class="gt_row gt_right">0.001</td></tr>
  </tbody>
  &#10;  
</table>
</div>

### Step 2: derive a fixed design with no interim analyses

Computing a fixed sample size design with 2.5% one-sided Type I error
and 90% power. We specify a trial duration of 36 months with
`analysis_time`. Enrollment duration is the sum of
`enroll_rate$duration`. We used `fixed_design()` since there is a single
analysis:

``` r
fd <- fixed_design(
  method = "ahr",
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
fd$enroll_rate %>%
  gt() %>%
  as_raw_html(inline_css = FALSE)
```

<div id="zlyydwofsx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="stratum">stratum</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="duration">duration</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="rate">rate</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="stratum" class="gt_row gt_left">All</td>
<td headers="duration" class="gt_row gt_right">12</td>
<td headers="rate" class="gt_row gt_right">35.05288</td></tr>
  </tbody>
  &#10;  
</table>
</div>

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
fd %>%
  summary() %>%
  as_gt() %>%
  as_raw_html(inline_css = FALSE)
```

<div id="sujaevgdsp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="8" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Fixed Design under AHR Method<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Design">Design</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="N">N</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Event">Event</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="time">time</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Bound">Bound</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="alpha">alpha</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Power">Power</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="design">design</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Design" class="gt_row gt_left">Average hazard ratio</td>
<td headers="N" class="gt_row gt_right">420.6346</td>
<td headers="Event" class="gt_row gt_right">311.0028</td>
<td headers="time" class="gt_row gt_right">36</td>
<td headers="Bound" class="gt_row gt_right">1.959964</td>
<td headers="alpha" class="gt_row gt_right">0.025</td>
<td headers="Power" class="gt_row gt_right">0.9</td>
<td headers="design" class="gt_row gt_left">ahr</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="8"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Power computed with average hazard ratio method.</td>
    </tr>
  </tfoot>
</table>
</div>

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
  test_upper = c(FALSE, FALSE, TRUE, TRUE), # Only test efficacy after 1st analysis
  lower = gs_b, # Fixed Z-values will be provided for futility bound
  lpar = c(qnorm(0.1), 0, -Inf, -Inf)
)
```

Now we summarize the derived design. The summary table is further
described in the vignette *Summarize group sequential designs in gt
tables*. Note that the design trend in favor of experimental treatment
is very minor at 8 months due to the delayed effect assumption used (see
AHR at analysis 1 in table). The design trend at 16 months is somewhat
more favorable when we are looking for HR \< 1 (favoring experimental
treatment) for a proof of concept. Actual bounds and timing selected for
a trial are situation dependent, but we hope the suggestions here are
provocative for what might be considered.

``` r
gsd %>%
  summary() %>%
  as_gt() %>%
  as_raw_html(inline_css = FALSE)
```

<div id="imumolcnny" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="6" class="gt_heading gt_title gt_font_normal" style>Bound summary for AHR design</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="6" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>AHR approximations of ~HR at bound</td>
    </tr>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="Bound">Bound</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col" id="Z">Z</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col" id="Nominal p&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;">Nominal p<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col" id="~HR at bound&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;2&lt;/sup&gt;&lt;/span&gt;">~HR at bound<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="Cumulative boundary crossing probability">
        <span class="gt_column_spanner">Cumulative boundary crossing probability</span>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Alternate hypothesis">Alternate hypothesis</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Null hypothesis">Null hypothesis</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="6" class="gt_group_heading" scope="colgroup" id="Analysis: 1 Time: 8 Event: 53.2 AHR: 0.9 N: 279.11 Information fraction: 0.17">Analysis: 1 Time: 8 Event: 53.2 AHR: 0.9 N: 279.11 Information fraction: 0.17</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Analysis: 1 Time: 8 Event: 53.2 AHR: 0.9 N: 279.11 Information fraction: 0.17  Bound" class="gt_row gt_left">Futility</td>
<td headers="Analysis: 1 Time: 8 Event: 53.2 AHR: 0.9 N: 279.11 Information fraction: 0.17  Z" class="gt_row gt_right">-1.28</td>
<td headers="Analysis: 1 Time: 8 Event: 53.2 AHR: 0.9 N: 279.11 Information fraction: 0.17  Nominal p" class="gt_row gt_right">0.9000</td>
<td headers="Analysis: 1 Time: 8 Event: 53.2 AHR: 0.9 N: 279.11 Information fraction: 0.17  ~HR at bound" class="gt_row gt_right">1.4210</td>
<td headers="Analysis: 1 Time: 8 Event: 53.2 AHR: 0.9 N: 279.11 Information fraction: 0.17  Alternate hypothesis" class="gt_row gt_right">0.0539</td>
<td headers="Analysis: 1 Time: 8 Event: 53.2 AHR: 0.9 N: 279.11 Information fraction: 0.17  Null hypothesis" class="gt_row gt_right">0.1000</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="6" class="gt_group_heading" scope="colgroup" id="Analysis: 2 Time: 14 Event: 137.2 AHR: 0.8 N: 418.66 Information fraction: 0.44">Analysis: 2 Time: 14 Event: 137.2 AHR: 0.8 N: 418.66 Information fraction: 0.44</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Analysis: 2 Time: 14 Event: 137.2 AHR: 0.8 N: 418.66 Information fraction: 0.44  Bound" class="gt_row gt_left">Futility</td>
<td headers="Analysis: 2 Time: 14 Event: 137.2 AHR: 0.8 N: 418.66 Information fraction: 0.44  Z" class="gt_row gt_right">0.00</td>
<td headers="Analysis: 2 Time: 14 Event: 137.2 AHR: 0.8 N: 418.66 Information fraction: 0.44  Nominal p" class="gt_row gt_right">0.5000</td>
<td headers="Analysis: 2 Time: 14 Event: 137.2 AHR: 0.8 N: 418.66 Information fraction: 0.44  ~HR at bound" class="gt_row gt_right">1.0000</td>
<td headers="Analysis: 2 Time: 14 Event: 137.2 AHR: 0.8 N: 418.66 Information fraction: 0.44  Alternate hypothesis" class="gt_row gt_right">0.1451</td>
<td headers="Analysis: 2 Time: 14 Event: 137.2 AHR: 0.8 N: 418.66 Information fraction: 0.44  Null hypothesis" class="gt_row gt_right">0.5091</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="6" class="gt_group_heading" scope="colgroup" id="Analysis: 3 Time: 24 Event: 238.4 AHR: 0.7 N: 418.66 Information fraction: 0.76">Analysis: 3 Time: 24 Event: 238.4 AHR: 0.7 N: 418.66 Information fraction: 0.76</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Analysis: 3 Time: 24 Event: 238.4 AHR: 0.7 N: 418.66 Information fraction: 0.76  Bound" class="gt_row gt_left">Efficacy</td>
<td headers="Analysis: 3 Time: 24 Event: 238.4 AHR: 0.7 N: 418.66 Information fraction: 0.76  Z" class="gt_row gt_right">2.30</td>
<td headers="Analysis: 3 Time: 24 Event: 238.4 AHR: 0.7 N: 418.66 Information fraction: 0.76  Nominal p" class="gt_row gt_right">0.0106</td>
<td headers="Analysis: 3 Time: 24 Event: 238.4 AHR: 0.7 N: 418.66 Information fraction: 0.76  ~HR at bound" class="gt_row gt_right">0.7421</td>
<td headers="Analysis: 3 Time: 24 Event: 238.4 AHR: 0.7 N: 418.66 Information fraction: 0.76  Alternate hypothesis" class="gt_row gt_right">0.5582</td>
<td headers="Analysis: 3 Time: 24 Event: 238.4 AHR: 0.7 N: 418.66 Information fraction: 0.76  Null hypothesis" class="gt_row gt_right">0.0106</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="6" class="gt_group_heading" scope="colgroup" id="Analysis: 4 Time: 36 Event: 309.5 AHR: 0.7 N: 418.66 Information fraction: 1">Analysis: 4 Time: 36 Event: 309.5 AHR: 0.7 N: 418.66 Information fraction: 1</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Analysis: 4 Time: 36 Event: 309.5 AHR: 0.7 N: 418.66 Information fraction: 1  Bound" class="gt_row gt_left">Efficacy</td>
<td headers="Analysis: 4 Time: 36 Event: 309.5 AHR: 0.7 N: 418.66 Information fraction: 1  Z" class="gt_row gt_right">2.02</td>
<td headers="Analysis: 4 Time: 36 Event: 309.5 AHR: 0.7 N: 418.66 Information fraction: 1  Nominal p" class="gt_row gt_right">0.0219</td>
<td headers="Analysis: 4 Time: 36 Event: 309.5 AHR: 0.7 N: 418.66 Information fraction: 1  ~HR at bound" class="gt_row gt_right">0.7951</td>
<td headers="Analysis: 4 Time: 36 Event: 309.5 AHR: 0.7 N: 418.66 Information fraction: 1  Alternate hypothesis" class="gt_row gt_right">0.8000</td>
<td headers="Analysis: 4 Time: 36 Event: 309.5 AHR: 0.7 N: 418.66 Information fraction: 1  Null hypothesis" class="gt_row gt_right"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>3</sup></span> 0.0244</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="6"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> One-sided p-value for experimental vs control treatment.
          Value &lt; 0.5 favors experimental, &gt; 0.5 favors control.</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="6"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span> Approximate hazard ratio to cross bound.</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="6"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>3</sup></span> Cumulative alpha for final analysis (0.0244) is less than the full alpha (0.025) when the futility bound is non-binding. The smaller value subtracts the probability of crossing a futility bound before crossing an efficacy bound at a later analysis (0.025 - 0.0006 = 0.0244) under the null hypothesis.</td>
    </tr>
  </tfoot>
</table>
</div>
