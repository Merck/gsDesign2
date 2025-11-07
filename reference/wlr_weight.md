# Weight functions for weighted log-rank test

- `wlr_weight_fh` is Fleming-Harrington, FH(rho, gamma) weight function.

- `wlr_weight_1` is constant for log rank test.

- `wlr_weight_power` is Gehan-Breslow and Tarone-Ware weight function.

- `wlr_weight_mb` is Magirr (2021) weight function.

## Usage

``` r
wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 0, tau = NULL)

wlr_weight_1(x, arm0, arm1)

wlr_weight_n(x, arm0, arm1, power = 1)

wlr_weight_mb(x, arm0, arm1, tau = NULL, w_max = Inf)
```

## Arguments

- x:

  A vector of numeric values.

- arm0:

  An `arm` object defined in the npsurvSS package.

- arm1:

  An `arm` object defined in the npsurvSS package.

- rho:

  A scalar parameter that controls the type of test.

- gamma:

  A scalar parameter that controls the type of test.

- tau:

  A scalar parameter of the cut-off time for modest weighted log rank
  test.

- power:

  A scalar parameter that controls the power of the weight function.

- w_max:

  A scalar parameter of the cut-off weight for modest weighted log rank
  test.

## Value

A vector of weights.

A vector of weights.

A vector of weights.

A vector of weights.

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

gs_arm <- gs_create_arm(enroll_rate, fail_rate, ratio = 1)
arm0 <- gs_arm$arm0
arm1 <- gs_arm$arm1

wlr_weight_fh(1:3, arm0, arm1, rho = 0, gamma = 0, tau = NULL)
#> [1] 1 1 1
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

gs_arm <- gs_create_arm(enroll_rate, fail_rate, ratio = 1)
arm0 <- gs_arm$arm0
arm1 <- gs_arm$arm1

wlr_weight_1(1:3, arm0, arm1)
#> [1] 1
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

gs_arm <- gs_create_arm(enroll_rate, fail_rate, ratio = 1)
arm0 <- gs_arm$arm0
arm1 <- gs_arm$arm1

wlr_weight_n(1:3, arm0, arm1, power = 2)
#> [1] 3.448634 2.973357 2.563657
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

gs_arm <- gs_create_arm(enroll_rate, fail_rate, ratio = 1)
arm0 <- gs_arm$arm0
arm1 <- gs_arm$arm1

wlr_weight_mb(1:3, arm0, arm1, tau = -1, w_max = 1.2)
#> [1] 1.075901 1.157545 1.200000
```
