
<!-- badges: start -->

[![R-CMD-check](https://github.com/LittleBeannie/gsDesign2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LittleBeannie/gsDesign2/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/gsDesign2)](https://CRAN.R-project.org/package=gsDesign2)
<!-- badges: end -->

## Objective

The goal of **gsDesign2** is to enable fixed or group sequential design
under non-proportional hazards. Piecewise constant enrollment, failure
rates and dropout rates for a stratified population are available to
enable highly flexible enrollment, time-to-event and time-to-dropout
assumptions. Substantial flexibility on top of what is in the gsDesign
package is intended for selecting boundaries. While this work is in
progress, substantial capabilities have been enabled. Comments on
usability and features are encouraged as this is a development version
of the package.

The goal of **gsDesign2** is to enable group sequential trial design for
time-to-event endpoints under non-proportional hazards assumptions. The
package is still maturing; as the package functions become more stable,
they will likely be included in the **gsDesign2** package.

## Installation

You can install `gsDesign2` with:

``` r
remotes::install_github("LittleBeannie/gsDesign2")
```

## Use cases

### Step 1: specifying enrollment and failure rates

This is a basic example which shows you how to solve a common problem.
We assume there is a 4 month delay in treatment effect. Specifically, we
assume a hazard ratio of 1 for 4 months and 0.6 thereafter. For this
example we assume an exponential failure rate and low exponential
dropout rate. The `enrollRates` specification indicates an expected
enrollment duration of 12 months with exponential inter-arrival times.

``` r
library(gsDesign)
library(gsDesign2)
library(dplyr)
library(gt)

# Basic example

# Constant enrollment over 12 months
# Rate will be adjusted later by gsDesignNPH to get sample size
enrollRates <- tibble::tibble(Stratum = "All", duration = 12, rate = 1)

# 12 month median exponential failure rate in control
# 4 month delay in effect with HR=0.6 after
# Low exponential dropout rate
medianSurv <- 12
failRates <- tibble::tibble(
  Stratum = "All",
  duration = c(4, Inf),
  failRate = log(2) / medianSurv,
  hr = c(1, .6),
  dropoutRate = .001
)
```

The resulting failure rate specification is the following table. As many
rows and strata as needed can be specified to approximate whatever
patterns you wish.

``` r
failRates %>%
  gt() %>%
  as_raw_html(inline_css = FALSE)
```

<div id="rukhwqbnsm" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col">Stratum</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">duration</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">failRate</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">hr</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">dropoutRate</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">All</td>
<td class="gt_row gt_right">4</td>
<td class="gt_row gt_right">0.05776227</td>
<td class="gt_row gt_right">1.0</td>
<td class="gt_row gt_right">0.001</td></tr>
    <tr><td class="gt_row gt_left">All</td>
<td class="gt_row gt_right">Inf</td>
<td class="gt_row gt_right">0.05776227</td>
<td class="gt_row gt_right">0.6</td>
<td class="gt_row gt_right">0.001</td></tr>
  </tbody>
  
  
</table>
</div>

### Step 2: compute the design

Computing a fixed sample size design with 2.5% one-sided Type I error
and 90% power. We specify a trial duration of 36 months with
`analysisTimes`. Since there is a single analysis, we specify an upper
p-value bound of 0.025 with `upar = qnorm(0.975)`. There is no lower
bound which is specified with `lpar = -Inf`.

``` r
x <- gs_design_ahr(
  enrollRates, failRates,
  upper = gs_b, upar = qnorm(.975),
  lower = gs_b, lpar = -Inf,
  IF = 1, analysisTimes = 36
)
```

The input enrollment rates are scaled to achieve power:

``` r
x$enrollRates %>%
  gt() %>%
  as_raw_html(inline_css = FALSE)
```

<div id="yjtuqvrhny" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col">Stratum</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">duration</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">rate</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">All</td>
<td class="gt_row gt_right">12</td>
<td class="gt_row gt_right">35.05288</td></tr>
  </tbody>
  
  
</table>
</div>

The failure and dropout rates remain unchanged from what was input:

``` r
x$failRates %>%
  gt() %>%
  as_raw_html(inline_css = FALSE)
```

<div id="lsubyqwkcb" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col">Stratum</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">duration</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">failRate</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">hr</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">dropoutRate</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">All</td>
<td class="gt_row gt_right">4</td>
<td class="gt_row gt_right">0.05776227</td>
<td class="gt_row gt_right">1.0</td>
<td class="gt_row gt_right">0.001</td></tr>
    <tr><td class="gt_row gt_left">All</td>
<td class="gt_row gt_right">Inf</td>
<td class="gt_row gt_right">0.05776227</td>
<td class="gt_row gt_right">0.6</td>
<td class="gt_row gt_right">0.001</td></tr>
  </tbody>
  
  
</table>
</div>

Additionally, the summary of bounds and crossing probability is
available at

``` r
x$bounds %>%
  gt() %>%
  as_raw_html(inline_css = FALSE)
```

<div id="xxbcmivtvt" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">Analysis</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col">Bound</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">Probability</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">Probability0</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">Z</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">~HR at bound</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">Nominal p</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_right">1</td>
<td class="gt_row gt_left">Upper</td>
<td class="gt_row gt_right">0.9</td>
<td class="gt_row gt_right">0.025</td>
<td class="gt_row gt_right">1.959964</td>
<td class="gt_row gt_right">0.800693</td>
<td class="gt_row gt_right">0.025</td></tr>
  </tbody>
  
  
</table>
</div>

Finally, the expected analysis time is in `Time`, sample size `N`,
events required `Events` and average hazard ratio `AHR` are in
`x$analysis`. Note that `AHR` is the average hazard ratio used to
calculate the targeted event counts. The natural parameter (`log(AHR)`)
is in theta and corresponding statistical information under the
alternate hypothesis are in `info` and under the null hypothesis in
`info0`.

``` r
x$analysis %>%
  gt() %>%
  as_raw_html(inline_css = FALSE)
```

<div id="kxchevjpcx" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">Analysis</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">Time</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">N</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">Events</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">AHR</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">theta</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">info</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">info0</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">IF</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_right">1</td>
<td class="gt_row gt_right">36</td>
<td class="gt_row gt_right">420.6346</td>
<td class="gt_row gt_right">311.0028</td>
<td class="gt_row gt_right">0.6917244</td>
<td class="gt_row gt_right">0.3685676</td>
<td class="gt_row gt_right">76.74383</td>
<td class="gt_row gt_right">77.75069</td>
<td class="gt_row gt_right">1</td></tr>
  </tbody>
  
  
</table>
</div>

### Step 3: summarize the design

``` r
x %>%
  summary() %>%
  as_gt() %>%
  as_raw_html(inline_css = FALSE)
```

<div id="qdcrjlcmwp" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table">
  <thead class="gt_header">
    <tr>
      <td colspan="5" class="gt_heading gt_title gt_font_normal" style>Bound summary for AHR design</td>
    </tr>
    <tr>
      <td colspan="5" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>AHR approximations of ~HR at bound</td>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col">Bound</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col">Nominal p<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col">~HR at bound<sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup">
        <span class="gt_column_spanner">Cumulative boundary crossing probability</span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">Alternate hypothesis</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col">Null hypothesis</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">Analysis: 1 Time: 36 N: 420.6 Events: 311 AHR: 0.69 IF: 1</td>
    </tr>
    <tr class="gt_row_group_first"><td class="gt_row gt_left">Efficacy</td>
<td class="gt_row gt_right">0.025</td>
<td class="gt_row gt_right">0.8007</td>
<td class="gt_row gt_right">0.9</td>
<td class="gt_row gt_right">0.025</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="5"><sup class="gt_footnote_marks">1</sup> One-sided p-value for experimental vs control treatment. Values &lt; 0.5 favor experimental, &gt; 0.5 favor control.</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="5"><sup class="gt_footnote_marks">2</sup> Approximate hazard ratio to cross bound.</td>
    </tr>
  </tfoot>
</table>
</div>
