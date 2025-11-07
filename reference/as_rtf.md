# Write summary table of a fixed or group sequential design object to an RTF file

Write summary table of a fixed or group sequential design object to an
RTF file

## Usage

``` r
as_rtf(x, ...)

# S3 method for class 'fixed_design_summary'
as_rtf(
  x,
  title = NULL,
  footnote = NULL,
  col_rel_width = NULL,
  orientation = c("portrait", "landscape"),
  text_font_size = 9,
  file,
  ...
)

# S3 method for class 'gs_design_summary'
as_rtf(
  x,
  title = NULL,
  subtitle = NULL,
  colname_spanner = "Cumulative boundary crossing probability",
  colname_spannersub = c("Alternate hypothesis", "Null hypothesis"),
  footnote = NULL,
  display_bound = c("Efficacy", "Futility"),
  display_columns = NULL,
  display_inf_bound = TRUE,
  col_rel_width = NULL,
  orientation = c("portrait", "landscape"),
  text_font_size = 9,
  file,
  ...
)
```

## Arguments

- x:

  A summary object of a fixed or group sequential design.

- ...:

  Additional arguments (not used).

- title:

  A string to specify the title of the RTF table.

- footnote:

  A list containing `content`, `location`, and `attr`. `content` is a
  vector of string to specify the footnote text; `location` is a vector
  of string to specify the locations to put the superscript of the
  footnote index; `attr` is a vector of string to specify the attributes
  of the footnotes, for example,
  `c("colname", "title", "subtitle", "analysis", "spanner")`; users can
  use the functions in the `gt` package to customize the table.

- col_rel_width:

  Column relative width in a vector e.g. c(2,1,1) refers to 2:1:1.
  Default is NULL for equal column width.

- orientation:

  Orientation in 'portrait' or 'landscape'.

- text_font_size:

  Text font size. To vary text font size by column, use numeric vector
  with length of vector equal to number of columns displayed e.g.
  c(9,20,40).

- file:

  File path for the output.

- subtitle:

  A string to specify the subtitle of the RTF table.

- colname_spanner:

  A string to specify the spanner of the RTF table.

- colname_spannersub:

  A vector of strings to specify the spanner details of the RTF table.

- display_bound:

  A vector of strings specifying the label of the bounds. The default is
  `c("Efficacy", "Futility")`.

- display_columns:

  A vector of strings specifying the variables to be displayed in the
  summary table.

- display_inf_bound:

  Logical, whether to display the +/-inf bound.

## Value

`as_rtf()` returns the input `x` invisibly.

## Examples

``` r
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

# AHR ----
# under fixed power
x <- fixed_design_ahr(
  alpha = alpha, power = 1 - beta,
  enroll_rate = enroll_rate, fail_rate = fail_rate,
  study_duration = study_duration, ratio = ratio
) |> summary()
x |> as_rtf(file = tempfile(fileext = ".rtf"))
x |> as_rtf(title = "Fixed design", file = tempfile(fileext = ".rtf"))
x |> as_rtf(
  footnote = "Power computed with average hazard ratio method given the sample size",
  file = tempfile(fileext = ".rtf")
)
x |> as_rtf(text_font_size = 10, file = tempfile(fileext = ".rtf"))

# FH ----
# under fixed power
fixed_design_fh(
  alpha = alpha, power = 1 - beta,
  enroll_rate = enroll_rate, fail_rate = fail_rate,
  study_duration = study_duration, ratio = ratio
) |>
  summary() |>
  as_rtf(file = tempfile(fileext = ".rtf"))
#'
# \donttest{
# the default output

gs_design_ahr() |>
  summary() |>
  as_rtf(file = tempfile(fileext = ".rtf"))

gs_power_ahr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
  summary() |>
  as_rtf(file = tempfile(fileext = ".rtf"))

gs_design_wlr() |>
  summary() |>
  as_rtf(file = tempfile(fileext = ".rtf"))

gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
  summary() |>
  as_rtf(file = tempfile(fileext = ".rtf"))


gs_power_combo() |>
  summary() |>
  as_rtf(file = tempfile(fileext = ".rtf"))

gs_design_rd() |>
  summary() |>
  as_rtf(file = tempfile(fileext = ".rtf"))

gs_power_rd() |>
  summary() |>
  as_rtf(file = tempfile(fileext = ".rtf"))

# usage of title = ..., subtitle = ...
# to edit the title/subtitle
gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
  summary() |>
  as_rtf(
    title = "Bound Summary",
    subtitle = "from gs_power_wlr",
    file = tempfile(fileext = ".rtf")
  )

# usage of colname_spanner = ..., colname_spannersub = ...
# to edit the spanner and its sub-spanner
gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
  summary() |>
  as_rtf(
    colname_spanner = "Cumulative probability to cross boundaries",
    colname_spannersub = c("under H1", "under H0"),
    file = tempfile(fileext = ".rtf")
  )

# usage of footnote = ...
# to edit the footnote
gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
  summary() |>
  as_rtf(
    footnote = list(
      content = c(
        "approximate weighted hazard ratio to cross bound.",
        "wAHR is the weighted AHR.",
        "the crossing probability.",
        "this table is generated by gs_power_wlr."
      ),
      location = c("~wHR at bound", NA, NA, NA),
      attr = c("colname", "analysis", "spanner", "title")
    ),
    file = tempfile(fileext = ".rtf")
  )

# usage of display_bound = ...
# to either show efficacy bound or futility bound, or both(default)
gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
  summary() |>
  as_rtf(
    display_bound = "Efficacy",
    file = tempfile(fileext = ".rtf")
  )

# usage of display_columns = ...
# to select the columns to display in the summary table
gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
  summary() |>
  as_rtf(
    display_columns = c("Analysis", "Bound", "Nominal p", "Z", "Probability"),
    file = tempfile(fileext = ".rtf")
  )
# }
```
