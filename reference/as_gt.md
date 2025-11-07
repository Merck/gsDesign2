# Convert summary table of a fixed or group sequential design object to a gt object

Convert summary table of a fixed or group sequential design object to a
gt object

## Usage

``` r
as_gt(x, ...)

# S3 method for class 'fixed_design_summary'
as_gt(x, title = NULL, footnote = NULL, ...)

# S3 method for class 'gs_design_summary'
as_gt(
  x,
  title = NULL,
  subtitle = NULL,
  colname_spanner = "Cumulative boundary crossing probability",
  colname_spannersub = c("Alternate hypothesis", "Null hypothesis"),
  footnote = NULL,
  display_bound = c("Efficacy", "Futility"),
  display_columns = NULL,
  display_inf_bound = FALSE,
  ...
)
```

## Arguments

- x:

  A summary object of a fixed or group sequential design.

- ...:

  Additional arguments (not used).

- title:

  A string to specify the title of the gt table.

- footnote:

  A list containing `content`, `location`, and `attr`. `content` is a
  vector of string to specify the footnote text; `location` is a vector
  of string to specify the locations to put the superscript of the
  footnote index; `attr` is a vector of string to specify the attributes
  of the footnotes, for example,
  `c("colname", "title", "subtitle", "analysis", "spanner")`; users can
  use the functions in the `gt` package to customize the table. To
  disable footnotes, use `footnote = FALSE`.

- subtitle:

  A string to specify the subtitle of the gt table.

- colname_spanner:

  A string to specify the spanner of the gt table.

- colname_spannersub:

  A vector of strings to specify the spanner details of the gt table.

- display_bound:

  A vector of strings specifying the label of the bounds. The default is
  `c("Efficacy", "Futility")`.

- display_columns:

  A vector of strings specifying the variables to be displayed in the
  summary table.

- display_inf_bound:

  Logical, whether to display the +/-inf bound.

## Value

A `gt_tbl` object.

## Examples

``` r
# Fixed design examples ----

# Enrollment rate
enroll_rate <- define_enroll_rate(
  duration = 18,
  rate = 20
)

# Failure rates
fail_rate <- define_fail_rate(
  duration = c(4, 100),
  fail_rate = log(2) / 12,
  dropout_rate = .001,
  hr = c(1, .6)
)

# Study duration in months
study_duration <- 36

# Experimental / Control randomization ratio
ratio <- 1

# 1-sided Type I error
alpha <- 0.025

# Type II error (1 - power)
beta <- 0.1

# Example 1 ----
fixed_design_ahr(
  alpha = alpha, power = 1 - beta,
  enroll_rate = enroll_rate, fail_rate = fail_rate,
  study_duration = study_duration, ratio = ratio
) |>
  summary() |>
  as_gt()


  


Fixed Design under AHR Method1
```

Design

N

Events

Time

AHR

Bound

alpha

Power

Average hazard ratio

463.078

324.7077

36

0.697102

1.959964

0.025

0.9

¹ Power computed with average hazard ratio method.

\# Example 2 ----
[fixed_design_fh](https://merck.github.io/gsDesign2/reference/fixed_design.md)(
alpha = alpha, power = 1 - beta, enroll_rate = enroll_rate, fail_rate =
fail_rate, study_duration = study_duration, ratio = ratio ) \|\>
[summary](https://rdrr.io/r/base/summary.html)() \|\> as_gt()

| Fixed Design under Fleming-Harrington Method¹                                        |          |          |      |           |          |       |       |
|:-------------------------------------------------------------------------------------|---------:|---------:|-----:|----------:|---------:|------:|------:|
| Design                                                                               |        N |   Events | Time |       AHR |    Bound | alpha | Power |
| Fleming-Harrington FH(0, 0) (logrank)                                                | 458.3509 | 321.3931 |   36 | 0.6969049 | 1.959964 | 0.025 |   0.9 |
| ¹ Power for Fleming-Harrington test FH(0, 0) (logrank) using method of Yung and Liu. |          |          |      |           |          |       |       |

\# \donttest{ \# Group sequential design examples --- \# Example 1 ----
\# The default output
[gs_design_ahr](https://merck.github.io/gsDesign2/reference/gs_design_ahr.md)()
\|\> [summary](https://rdrr.io/r/base/summary.html)() \|\> as_gt()

[TABLE]

[gs_power_ahr](https://merck.github.io/gsDesign2/reference/gs_power_ahr.md)(lpar
= [list](https://rdrr.io/r/base/list.html)(sf =
gsDesign::[sfLDOF](https://keaven.github.io/gsDesign/reference/sfLDOF.html),
total_spend = 0.1)) \|\>
[summary](https://rdrr.io/r/base/summary.html)() \|\> as_gt()

[TABLE]

[gs_design_wlr](https://merck.github.io/gsDesign2/reference/gs_design_wlr.md)()
\|\> [summary](https://rdrr.io/r/base/summary.html)() \|\> as_gt()

[TABLE]

[gs_power_wlr](https://merck.github.io/gsDesign2/reference/gs_power_wlr.md)(lpar
= [list](https://rdrr.io/r/base/list.html)(sf =
gsDesign::[sfLDOF](https://keaven.github.io/gsDesign/reference/sfLDOF.html),
total_spend = 0.1)) \|\>
[summary](https://rdrr.io/r/base/summary.html)() \|\> as_gt()

[TABLE]

[gs_power_combo](https://merck.github.io/gsDesign2/reference/gs_power_combo.md)()
\|\> [summary](https://rdrr.io/r/base/summary.html)() \|\> as_gt()

[TABLE]

[gs_design_rd](https://merck.github.io/gsDesign2/reference/gs_design_rd.md)()
\|\> [summary](https://rdrr.io/r/base/summary.html)() \|\> as_gt()

[TABLE]

[gs_power_rd](https://merck.github.io/gsDesign2/reference/gs_power_rd.md)()
\|\> [summary](https://rdrr.io/r/base/summary.html)() \|\> as_gt()

[TABLE]

\# Example 2 ---- \# Usage of title = ..., subtitle = ... \# to edit the
title/subtitle
[gs_power_wlr](https://merck.github.io/gsDesign2/reference/gs_power_wlr.md)(lpar
= [list](https://rdrr.io/r/base/list.html)(sf =
gsDesign::[sfLDOF](https://keaven.github.io/gsDesign/reference/sfLDOF.html),
total_spend = 0.1)) \|\>
[summary](https://rdrr.io/r/base/summary.html)() \|\> as_gt( title =
"Bound Summary", subtitle = "from gs_power_wlr" )

[TABLE]

\# Example 3 ---- \# Usage of colname_spanner = ..., colname_spannersub
= ... \# to edit the spanner and its sub-spanner
[gs_power_wlr](https://merck.github.io/gsDesign2/reference/gs_power_wlr.md)(lpar
= [list](https://rdrr.io/r/base/list.html)(sf =
gsDesign::[sfLDOF](https://keaven.github.io/gsDesign/reference/sfLDOF.html),
total_spend = 0.1)) \|\>
[summary](https://rdrr.io/r/base/summary.html)() \|\> as_gt(
colname_spanner = "Cumulative probability to cross boundaries",
colname_spannersub = [c](https://rdrr.io/r/base/c.html)("under H1",
"under H0") )

[TABLE]

\# Example 4 ---- \# Usage of footnote = ... \# to edit the footnote
[gs_power_wlr](https://merck.github.io/gsDesign2/reference/gs_power_wlr.md)(lpar
= [list](https://rdrr.io/r/base/list.html)(sf =
gsDesign::[sfLDOF](https://keaven.github.io/gsDesign/reference/sfLDOF.html),
total_spend = 0.1)) \|\>
[summary](https://rdrr.io/r/base/summary.html)() \|\> as_gt( footnote =
[list](https://rdrr.io/r/base/list.html)( content =
[c](https://rdrr.io/r/base/c.html)( "approximate weighted hazard ratio
to cross bound.", "wAHR is the weighted AHR.", "the crossing
probability.", "this table is generated by gs_power_wlr." ), location =
[c](https://rdrr.io/r/base/c.html)("~wHR at bound", NA, NA, NA), attr =
[c](https://rdrr.io/r/base/c.html)("colname", "analysis", "spanner",
"title") ) )

[TABLE]

\# Example 5 ---- \# Usage of display_bound = ... \# to either show
efficacy bound or futility bound, or both(default)
[gs_power_wlr](https://merck.github.io/gsDesign2/reference/gs_power_wlr.md)(lpar
= [list](https://rdrr.io/r/base/list.html)(sf =
gsDesign::[sfLDOF](https://keaven.github.io/gsDesign/reference/sfLDOF.html),
total_spend = 0.1)) \|\>
[summary](https://rdrr.io/r/base/summary.html)() \|\>
as_gt(display_bound = "Efficacy")

[TABLE]

\# Example 6 ---- \# Usage of display_columns = ... \# to select the
columns to display in the summary table
[gs_power_wlr](https://merck.github.io/gsDesign2/reference/gs_power_wlr.md)(lpar
= [list](https://rdrr.io/r/base/list.html)(sf =
gsDesign::[sfLDOF](https://keaven.github.io/gsDesign/reference/sfLDOF.html),
total_spend = 0.1)) \|\>
[summary](https://rdrr.io/r/base/summary.html)() \|\>
as_gt(display_columns = [c](https://rdrr.io/r/base/c.html)("Analysis",
"Bound", "Nominal p", "Z", "Probability"))

[TABLE]

\# }
