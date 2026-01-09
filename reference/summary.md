# Summary for fixed design or group sequential design objects

Summary for fixed design or group sequential design objects

## Usage

``` r
# S3 method for class 'fixed_design'
summary(object, ...)

# S3 method for class 'gs_design'
summary(
  object,
  analysis_vars = NULL,
  analysis_decimals = NULL,
  col_vars = NULL,
  col_decimals = NULL,
  bound_names = c("Efficacy", "Futility"),
  display_spending_time = FALSE,
  ...
)
```

## Arguments

- object:

  A design object returned by fixed_design_xxx() and gs_design_xxx().

- ...:

  Additional parameters (not used).

- analysis_vars:

  The variables to be put at the summary header of each analysis.

- analysis_decimals:

  The displayed number of digits of `analysis_vars`. If the vector is
  unnamed, it must match the length of `analysis_vars`. If the vector is
  named, you only have to specify the number of digits for the variables
  you want to be displayed differently than the defaults.

- col_vars:

  The variables to be displayed.

- col_decimals:

  The decimals to be displayed for the displayed variables in
  `col_vars`. If the vector is unnamed, it must match the length of
  `col_vars`. If the vector is named, you only have to specify the
  number of digits for the columns you want to be displayed differently
  than the defaults.

- bound_names:

  Names for bounds; default is `c("Efficacy", "Futility")`.

- display_spending_time:

  A logical value (TRUE/FALSE) indicating if the spending time is
  summarized in the table.

## Value

A summary table (data frame).

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
  hr = c(1, .6),
  dropout_rate = .001
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
fixed_design_ahr(
  alpha = alpha,
  power = 1 - beta,
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  study_duration = study_duration,
  ratio = ratio
) |> summary()
#> # A tibble: 1 × 8
#>   Design                   N Events  Time   AHR Bound alpha Power
#>   <chr>                <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Average hazard ratio  463.   325.    36 0.697  1.96 0.025   0.9

# FH ----
# under fixed power
fixed_design_fh(
  alpha = alpha,
  power = 1 - beta,
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  study_duration = study_duration,
  ratio = ratio
) |> summary()
#> # A tibble: 1 × 8
#>   Design                                  N Events  Time   AHR Bound alpha Power
#>   <chr>                               <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Fleming-Harrington FH(0, 0) (logra…  458.   321.    36 0.697  1.96 0.025   0.9

# Design parameters ----
library(gsDesign)
library(gsDesign2)

# enrollment/failure rates
enroll_rate <- define_enroll_rate(
  stratum = "All",
  duration = 12,
  rate = 1
)
fail_rate <- define_fail_rate(
  duration = c(4, 100),
  fail_rate = log(2) / 12,
  hr = c(1, .6),
  dropout_rate = .001
)

# Information fraction
info_frac <- (1:3) / 3

# Analysis times in months; first 2 will be ignored as info_frac will not be achieved
analysis_time <- c(.01, .02, 36)

# Experimental / Control randomization ratio
ratio <- 1

# 1-sided Type I error
alpha <- 0.025

# Type II error (1 - power)
beta <- .1

# Upper bound
upper <- gs_spending_bound
upar <- list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)

# Lower bound
lower <- gs_spending_bound
lpar <- list(sf = gsDesign::sfHSD, total_spend = 0.1, param = 0, timing = NULL)

# test in COMBO
fh_test <- rbind(
  data.frame(rho = 0, gamma = 0, tau = -1, test = 1, analysis = 1:3, analysis_time = c(12, 24, 36)),
  data.frame(rho = c(0, 0.5), gamma = 0.5, tau = -1, test = 2:3, analysis = 3, analysis_time = 36)
)

# Example 1 ----
# \donttest{
x_ahr <- gs_design_ahr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  info_frac = info_frac, # Information fraction
  analysis_time = analysis_time,
  ratio = ratio,
  alpha = alpha,
  beta = beta,
  upper = upper,
  upar = upar,
  lower = lower,
  lpar = lpar
)

x_ahr |> summary()
#> # A tibble: 6 × 7
#> # Groups:   Analysis [3]
#>   Analysis         Bound     Z `~HR at bound` `Nominal p` `Alternate hypothesis`
#>   <chr>            <chr> <dbl>          <dbl>       <dbl>                  <dbl>
#> 1 Analysis: 1 Tim… Futi… -0.94          1.19       0.826                  0.0338
#> 2 Analysis: 1 Tim… Effi…  3.71          0.510      0.0001                 0.0027
#> 3 Analysis: 2 Tim… Futi…  0.63          0.923      0.266                  0.0666
#> 4 Analysis: 2 Tim… Effi…  2.51          0.725      0.006                  0.414 
#> 5 Analysis: 3 Tim… Futi…  1.99          0.812      0.0233                 0.101 
#> 6 Analysis: 3 Tim… Effi…  1.99          0.812      0.0231                 0.9   
#> # ℹ 1 more variable: `Null hypothesis` <dbl>

# Customize the digits to display
x_ahr |> summary(analysis_vars = c("time", "event", "info_frac"), analysis_decimals = c(1, 0, 2))
#> # A tibble: 6 × 7
#> # Groups:   Analysis [3]
#>   Analysis         Bound     Z `~HR at bound` `Nominal p` `Alternate hypothesis`
#>   <chr>            <chr> <dbl>          <dbl>       <dbl>                  <dbl>
#> 1 Analysis: 1 Tim… Futi… -0.94          1.19       0.826                  0.0338
#> 2 Analysis: 1 Tim… Effi…  3.71          0.510      0.0001                 0.0027
#> 3 Analysis: 2 Tim… Futi…  0.63          0.923      0.266                  0.0666
#> 4 Analysis: 2 Tim… Effi…  2.51          0.725      0.006                  0.414 
#> 5 Analysis: 3 Tim… Futi…  1.99          0.812      0.0233                 0.101 
#> 6 Analysis: 3 Tim… Effi…  1.99          0.812      0.0231                 0.9   
#> # ℹ 1 more variable: `Null hypothesis` <dbl>

# Customize the labels of the crossing probability
x_ahr |> summary(bound_names = c("A is better", "B is better"))
#> # A tibble: 6 × 7
#> # Groups:   Analysis [3]
#>   Analysis         Bound     Z `~HR at bound` `Nominal p` `Alternate hypothesis`
#>   <chr>            <chr> <dbl>          <dbl>       <dbl>                  <dbl>
#> 1 Analysis: 1 Tim… B is… -0.94          1.19       0.826                  0.0338
#> 2 Analysis: 1 Tim… A is…  3.71          0.510      0.0001                 0.0027
#> 3 Analysis: 2 Tim… B is…  0.63          0.923      0.266                  0.0666
#> 4 Analysis: 2 Tim… A is…  2.51          0.725      0.006                  0.414 
#> 5 Analysis: 3 Tim… B is…  1.99          0.812      0.0233                 0.101 
#> 6 Analysis: 3 Tim… A is…  1.99          0.812      0.0231                 0.9   
#> # ℹ 1 more variable: `Null hypothesis` <dbl>

# Customize the variables to be summarized for each analysis
x_ahr |> summary(analysis_vars = c("n", "event"), analysis_decimals = c(1, 1))
#> # A tibble: 6 × 7
#> # Groups:   Analysis [3]
#>   Analysis         Bound     Z `~HR at bound` `Nominal p` `Alternate hypothesis`
#>   <chr>            <chr> <dbl>          <dbl>       <dbl>                  <dbl>
#> 1 Analysis: 1 N: … Futi… -0.94          1.19       0.826                  0.0338
#> 2 Analysis: 1 N: … Effi…  3.71          0.510      0.0001                 0.0027
#> 3 Analysis: 2 N: … Futi…  0.63          0.923      0.266                  0.0666
#> 4 Analysis: 2 N: … Effi…  2.51          0.725      0.006                  0.414 
#> 5 Analysis: 3 N: … Futi…  1.99          0.812      0.0233                 0.101 
#> 6 Analysis: 3 N: … Effi…  1.99          0.812      0.0231                 0.9   
#> # ℹ 1 more variable: `Null hypothesis` <dbl>

# Customize the digits for the columns
x_ahr |> summary(col_decimals = c(z = 4))
#> # A tibble: 6 × 7
#> # Groups:   Analysis [3]
#>   Analysis        Bound      Z `~HR at bound` `Nominal p` `Alternate hypothesis`
#>   <chr>           <chr>  <dbl>          <dbl>       <dbl>                  <dbl>
#> 1 Analysis: 1 Ti… Futi… -0.938          1.19       0.826                  0.0338
#> 2 Analysis: 1 Ti… Effi…  3.71           0.510      0.0001                 0.0027
#> 3 Analysis: 2 Ti… Futi…  0.626          0.923      0.266                  0.0666
#> 4 Analysis: 2 Ti… Effi…  2.51           0.725      0.006                  0.414 
#> 5 Analysis: 3 Ti… Futi…  1.99           0.812      0.0233                 0.101 
#> 6 Analysis: 3 Ti… Effi…  1.99           0.812      0.0231                 0.9   
#> # ℹ 1 more variable: `Null hypothesis` <dbl>

# Customize the columns to display
x_ahr |> summary(col_vars = c("z", "~hr at bound", "nominal p"))
#> Adding missing grouping variables: `Analysis`
#> # A tibble: 6 × 5
#> # Groups:   Analysis [3]
#>   Analysis                                Bound     Z `~HR at bound` `Nominal p`
#>   <chr>                                   <chr> <dbl>          <dbl>       <dbl>
#> 1 Analysis: 1 Time: 11.7 N: 479.6 Events… Futi… -0.94          1.19       0.826 
#> 2 Analysis: 1 Time: 11.7 N: 479.6 Events… Effi…  3.71          0.510      0.0001
#> 3 Analysis: 2 Time: 20.3 N: 493.1 Events… Futi…  0.63          0.923      0.266 
#> 4 Analysis: 2 Time: 20.3 N: 493.1 Events… Effi…  2.51          0.725      0.006 
#> 5 Analysis: 3 Time: 36 N: 493.1 Events: … Futi…  1.99          0.812      0.0233
#> 6 Analysis: 3 Time: 36 N: 493.1 Events: … Effi…  1.99          0.812      0.0231

# Customize columns and digits
x_ahr |> summary(col_vars = c("z", "~hr at bound", "nominal p"),
                  col_decimals = c(4, 2, 2))
#> Adding missing grouping variables: `Analysis`
#> # A tibble: 6 × 5
#> # Groups:   Analysis [3]
#>   Analysis                               Bound      Z `~HR at bound` `Nominal p`
#>   <chr>                                  <chr>  <dbl>          <dbl>       <dbl>
#> 1 Analysis: 1 Time: 11.7 N: 479.6 Event… Futi… -0.938           1.19        0.83
#> 2 Analysis: 1 Time: 11.7 N: 479.6 Event… Effi…  3.71            0.51        0   
#> 3 Analysis: 2 Time: 20.3 N: 493.1 Event… Futi…  0.626           0.92        0.27
#> 4 Analysis: 2 Time: 20.3 N: 493.1 Event… Effi…  2.51            0.72        0.01
#> 5 Analysis: 3 Time: 36 N: 493.1 Events:… Futi…  1.99            0.81        0.02
#> 6 Analysis: 3 Time: 36 N: 493.1 Events:… Effi…  1.99            0.81        0.02
# }

# Example 2 ----
# \donttest{
x_wlr <- gs_design_wlr(
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  weight = list(method = "fh", param = list(rho = 0, gamma = 0.5)),
  info_frac = NULL,
  analysis_time = sort(unique(x_ahr$analysis$time)),
  ratio = ratio,
  alpha = alpha,
  beta = beta,
  upper = upper,
  upar = upar,
  lower = lower,
  lpar = lpar
)
x_wlr |> summary()
#> # A tibble: 6 × 7
#> # Groups:   Analysis [3]
#>   Analysis        Bound     Z `~wHR at bound` `Nominal p` `Alternate hypothesis`
#>   <chr>           <chr> <dbl>           <dbl>       <dbl>                  <dbl>
#> 1 Analysis: 1 Ti… Futi… -1.17           1.28       0.880                  0.0145
#> 2 Analysis: 1 Ti… Effi…  5.94           0.289      0                      0     
#> 3 Analysis: 2 Ti… Futi…  0.57           0.919      0.284                  0.0464
#> 4 Analysis: 2 Ti… Effi…  3.16           0.627      0.0008                 0.214 
#> 5 Analysis: 3 Ti… Futi…  1.96           0.789      0.0247                 0.100 
#> 6 Analysis: 3 Ti… Effi…  1.96           0.789      0.0247                 0.9   
#> # ℹ 1 more variable: `Null hypothesis` <dbl>
# }
# Maxcombo ----
# \donttest{
x_combo <- gs_design_combo(
  ratio = 1,
  alpha = 0.025,
  beta = 0.2,
  enroll_rate = define_enroll_rate(duration = 12, rate = 500 / 12),
  fail_rate = tibble::tibble(
    stratum = "All",
    duration = c(4, 100),
    fail_rate = log(2) / 15, hr = c(1, .6), dropout_rate = .001
  ),
  fh_test = fh_test,
  upper = gs_spending_combo,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lower = gs_spending_combo,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2)
)
x_combo |> summary()
#> # A tibble: 6 × 6
#> # Groups:   Analysis [3]
#>   Analysis      Bound     Z `Nominal p` `Alternate hypothesis` `Null hypothesis`
#>   <chr>         <chr> <dbl>       <dbl>                  <dbl>             <dbl>
#> 1 Analysis: 1 … Futi… -2.72      0.997                  0.0003            0.0033
#> 2 Analysis: 1 … Effi…  6.18      0                      0                 0     
#> 3 Analysis: 2 … Futi…  0.65      0.257                  0.0847            0.743 
#> 4 Analysis: 2 … Effi…  2.8       0.0026                 0.220             0.0026
#> 5 Analysis: 3 … Futi…  2.1       0.018                  0.2               0.976 
#> 6 Analysis: 3 … Effi…  2.1       0.018                  0.8               0.0237
# }
# Risk difference ----
# \donttest{
gs_design_rd(
  p_c = tibble::tibble(stratum = "All", rate = .2),
  p_e = tibble::tibble(stratum = "All", rate = .15),
  info_frac = c(0.7, 1),
  rd0 = 0,
  alpha = .025,
  beta = .1,
  ratio = 1,
  stratum_prev = NULL,
  weight = "unstratified",
  upper = gs_b,
  lower = gs_b,
  upar = gsDesign::gsDesign(
    k = 3, test.type = 1, sfu = gsDesign::sfLDOF, sfupar = NULL
  )$upper$bound,
  lpar = c(qnorm(.1), rep(-Inf, 2))
) |> summary()
#> # A tibble: 3 × 7
#> # Groups:   Analysis [2]
#>   Analysis Bound     Z ~Risk difference at …¹ `Nominal p` `Alternate hypothesis`
#>   <chr>    <chr> <dbl>                  <dbl>       <dbl>                  <dbl>
#> 1 Analysi… Futi… -1.28                -0.0201      0.9                     0    
#> 2 Analysi… Effi…  3.71                 0.0582      0.0001                  0.298
#> 3 Analysi… Effi…  2.51                 0.033       0.006                   0.9  
#> # ℹ abbreviated name: ¹​`~Risk difference at bound`
#> # ℹ 1 more variable: `Null hypothesis` <dbl>
# }
```
