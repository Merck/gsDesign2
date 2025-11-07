# Create npsurvSS arm object

Create npsurvSS arm object

## Usage

``` r
gs_create_arm(enroll_rate, fail_rate, ratio, total_time = 1e+06)
```

## Arguments

- enroll_rate:

  Enrollment rates from
  [`define_enroll_rate()`](https://merck.github.io/gsDesign2/reference/define_enroll_rate.md).

- fail_rate:

  Failure and dropout rates from
  [`define_fail_rate()`](https://merck.github.io/gsDesign2/reference/define_fail_rate.md).

- ratio:

  Experimental:Control randomization ratio.

- total_time:

  Total analysis time.

## Value

A list of the two arms.

## Specification

The contents of this section are shown in PDF user manual only.

## Examples

``` r
enroll_rate <- define_enroll_rate(
  duration = c(2, 2, 10),
  rate = c(3, 6, 9)
)

fail_rate <- define_fail_rate(
  duration = c(3, 100),
  fail_rate = log(2) / c(9, 18),
  hr = c(.9, .6),
  dropout_rate = .001
)

gs_create_arm(enroll_rate, fail_rate, ratio = 1)
#> $arm0
#> $size
#> [1] 1
#> 
#> $accr_time
#> [1] 14
#> 
#> $accr_dist
#> [1] "pieceuni"
#> 
#> $accr_interval
#> [1]  0  2  4 14
#> 
#> $accr_param
#> [1] 0.05555556 0.11111111 0.83333333
#> 
#> $surv_cure
#> [1] 0
#> 
#> $surv_interval
#> [1]   0   3 Inf
#> 
#> $surv_shape
#> [1] 1
#> 
#> $surv_scale
#> [1] 0.07701635 0.03850818
#> 
#> $loss_shape
#> [1] 1
#> 
#> $loss_scale
#> [1] 0.001
#> 
#> $follow_time
#> [1] 999986
#> 
#> $total_time
#> [1] 1e+06
#> 
#> attr(,"class")
#> [1] "list" "arm" 
#> 
#> $arm1
#> $size
#> [1] 1
#> 
#> $accr_time
#> [1] 14
#> 
#> $accr_dist
#> [1] "pieceuni"
#> 
#> $accr_interval
#> [1]  0  2  4 14
#> 
#> $accr_param
#> [1] 0.05555556 0.11111111 0.83333333
#> 
#> $surv_cure
#> [1] 0
#> 
#> $surv_interval
#> [1]   0   3 Inf
#> 
#> $surv_shape
#> [1] 1
#> 
#> $surv_scale
#> [1] 0.06931472 0.02310491
#> 
#> $loss_shape
#> [1] 1
#> 
#> $loss_scale
#> [1] 0.001
#> 
#> $follow_time
#> [1] 999986
#> 
#> $total_time
#> [1] 1e+06
#> 
#> attr(,"class")
#> [1] "list" "arm" 
#> 
```
