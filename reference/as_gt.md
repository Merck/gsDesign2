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
