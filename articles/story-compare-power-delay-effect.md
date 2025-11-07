# Power for delayed effect scenarios

``` r
library(gt)
library(tidyr)
library(ggplot2)
library(gsDesign)
library(gsDesign2)
```

## Overview

We consider a delayed effect scenario where

- The control group time-to-event distribution is exponential with a
  median of 15 months.
- The experimental group has a hazard ratio vs. control of 1 for 6
  months and 0.6 thereafter.
- Enrollment at a constant rate for 12 months.
- Total study duration from 20 to 48 months.
- Exponential dropout rate of 0.001 per month.

``` r
enroll_rate <- define_enroll_rate(duration = 12, rate = 1)
fail_rate <- define_fail_rate(
  duration = c(6, 100),
  fail_rate = log(2) / 15,
  hr = c(1, .6),
  dropout_rate = 0.001
)

enroll_rate |>
  gt() |>
  tab_header(title = "Enrollment Table of Scenario 1")
```

| Enrollment Table of Scenario 1 |          |      |
|--------------------------------|----------|------|
| stratum                        | duration | rate |
| All                            | 12       | 1    |

``` r
fail_rate |>
  gt() |>
  tab_header(title = "Failure Table of Scenario 1")
```

| Failure Table of Scenario 1 |          |            |              |     |
|-----------------------------|----------|------------|--------------|-----|
| stratum                     | duration | fail_rate  | dropout_rate | hr  |
| All                         | 6        | 0.04620981 | 0.001        | 1.0 |
| All                         | 100      | 0.04620981 | 0.001        | 0.6 |

For the above scenarios, we investigate the power, sample size and
events under 6 tests:

- `fh_05`: The Fleming-Harrington with \\\rho=0, \gamma=0.5\\ test to
  obtain power of 85% given 1-sided Type I error of 0.025.
- `fh_00`: The regular logrank test with \\\rho=0, \gamma=0\\ under
  fixed study duration \\\in\\20, 24, 28, \ldots, 60\\\\.
- `mc2_test`: The MaxCombo test including 2 WLR tests, i.e.,
  \\\\(\rho=0, \gamma=0, \tau = -1), (\rho=0, \gamma=0.5, \tau =
  -1)\\\\.
- `mc2_test`: The MaxCombo test including 3 WLR tests, i.e.,
  \\\\(\rho=0, \gamma=0, \tau = -1), (\rho=0, \gamma=0.5, \tau = -1),
  (\rho=0.5, \gamma=0.5, \tau = -1)\\\\.
- `mc4_test`: The MaxCombo test including 4 WLR tests, i.e.,
  \\\\(\rho=0, \gamma=0, \tau = -1), (\rho=0, \gamma=0.5, \tau = -1),
  (\rho=0.5, \gamma=0.5, \tau = -1), (\rho=0.5, \gamma=0, \tau =
  -1)\\\\.
- `mb_6`: The Magirr-Burman with \\\rho=-1, \gamma=0, \tau = 6\\ test
  with fixed study duration \\\in\\20, 24, 28, \ldots, 60\\\\.

We then compute power for the logrank test. The general summary is that
the Fleming-Harrington test has a meaningful power gain relative to
logrank regardless of the study durations evaluated.

``` r
tab <- NULL

for (trial_duration in seq(24, 60, 4)) {
  # Fleming-Harrington rho=0, gamma=0.5 test
  fh_05 <- gs_design_wlr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = 1,
    alpha = 0.025, beta = 0.15,
    weight = list(method = "fh", param = list(rho = 0, gamma = 0.5)),
    upper = gs_b,
    lower = gs_b,
    upar = qnorm(.975),
    lpar = -Inf,
    analysis_time = trial_duration
  ) |> to_integer()

  # Regular logrank test
  fh_00 <- gs_power_wlr(
    enroll_rate = fh_05$enroll_rate,
    fail_rate = fail_rate,
    ratio = 1,
    weight = list(method = "fh", param = list(rho = 0, gamma = 0)),
    upper = gs_b,
    lower = gs_b,
    upar = qnorm(.975),
    lpar = -Inf,
    analysis_time = trial_duration,
    event = .1
  )

  # MaxCombo test 1
  mc2_test <- data.frame(
    rho = 0, gamma = c(0, .5), tau = -1,
    test = 1:2, analysis = 1, analysis_time = trial_duration
  )

  mc_2 <- gs_power_combo(
    enroll_rate = fh_05$enroll_rate,
    fail_rate = fail_rate,
    fh_test = mc2_test,
    upper = gs_spending_combo,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_combo,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.01)
  )

  # MaxCombo test 2
  mc3_test <- data.frame(
    rho = c(0, 0, .5), gamma = c(0, .5, .5), tau = -1,
    test = 1:3, analysis = 1, analysis_time = trial_duration
  )

  mc_3 <- gs_power_combo(
    enroll_rate = fh_05$enroll_rate,
    fail_rate = fail_rate,
    fh_test = mc3_test,
    upper = gs_spending_combo,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_combo,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.01)
  )

  # MaxCombo test
  mc4_test <- data.frame(
    rho = c(0, 0, .5, .5), gamma = c(0, .5, .5, 0), tau = -1,
    test = 1:4, analysis = 1, analysis_time = trial_duration
  )

  mc_4 <- gs_power_combo(
    enroll_rate = fh_05$enroll_rate,
    fail_rate = fail_rate,
    fh_test = mc4_test,
    upper = gs_spending_combo,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_combo,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.01)
  )

  # Magirr-Burman rho=-1, gamma=0, tau = 6 test
  mb_6 <- gs_power_wlr(
    enroll_rate = fh_05$enroll_rate,
    fail_rate = fail_rate,
    ratio = 1,
    weight = list(method = "fh", param = list(rho = -1, gamma = 0, tau = 15)),
    upper = gs_b,
    lower = gs_b,
    upar = qnorm(.975),
    lpar = -Inf,
    analysis_time = trial_duration,
    event = .1
  )

  tab_new <- tibble(
    `Study duration` = trial_duration,
    N = fh_05$analysis$n[1],
    Events = fh_05$analysi$event[1],
    `Events/N` = Events / N,
    # We use the AHR from regular WLR as the AHR of different MaxCombo test
    AHR = as.numeric(fh_00$analysis$ahr[1]),
    `FH(0, 0.5) power` = fh_05$bound$probability[1],
    `FH(0, 0) power` = fh_00$bound$probability[1],
    `MC2 power` = mc_2$bound$probability[1],
    `MC4 power` = mc_4$bound$probability[1],
    `MC3 power` = mc_3$bound$probability[1],
    `MB6 power` = mb_6$bound$probability[1]
  )
  tab <- rbind(tab, tab_new)
}

tab |>
  gt() |>
  fmt_number(columns = c(2, 3), decimals = 1) |>
  fmt_number(columns = 4, decimals = 2) |>
  fmt_number(columns = 5, decimals = 4) |>
  fmt_number(columns = 6:11, decimals = 2)
```

| Study duration | N     | Events | Events/N | AHR    | FH(0, 0.5) power | FH(0, 0) power | MC2 power | MC4 power | MC3 power | MB6 power |
|----------------|-------|--------|----------|--------|------------------|----------------|-----------|-----------|-----------|-----------|
| 24             | 696.0 | 350.0  | 0.50     | 0.7688 | 0.85             | 0.69           | 0.82      | 0.81      | 0.82      | 0.82      |
| 28             | 522.0 | 297.0  | 0.57     | 0.7473 | 0.85             | 0.71           | 0.82      | 0.81      | 0.82      | 0.86      |
| 32             | 428.0 | 267.0  | 0.62     | 0.7325 | 0.85             | 0.72           | 0.82      | 0.81      | 0.82      | 0.89      |
| 36             | 370.0 | 248.0  | 0.67     | 0.7218 | 0.85             | 0.73           | 0.82      | 0.81      | 0.82      | 0.92      |
| 40             | 332.0 | 235.0  | 0.71     | 0.7138 | 0.85             | 0.74           | 0.82      | 0.81      | 0.82      | 0.94      |
| 44             | 304.0 | 226.0  | 0.74     | 0.7076 | 0.85             | 0.74           | 0.82      | 0.81      | 0.82      | 0.96      |
| 48             | 282.0 | 219.0  | 0.78     | 0.7027 | 0.85             | 0.74           | 0.81      | 0.80      | 0.82      | 0.97      |
| 52             | 266.0 | 213.0  | 0.80     | 0.6987 | 0.85             | 0.75           | 0.81      | 0.80      | 0.81      | 0.98      |
| 56             | 254.0 | 209.0  | 0.82     | 0.6955 | 0.85             | 0.75           | 0.81      | 0.80      | 0.81      | 0.99      |
| 60             | 244.0 | 206.0  | 0.84     | 0.6929 | 0.85             | 0.75           | 0.81      | 0.80      | 0.81      | 0.99      |

## An Alternative Scenario

Now we consider an alternate scenario where the placebo group starts
with the same median, but then has a piecewise change to a median of 30
after 16 months and with a hazard ratio of 0.85 during that late period.

``` r
enroll_rate <- define_enroll_rate(duration = 12, rate = 1)
fail_rate <- define_fail_rate(
  duration = c(6, 10, 100),
  # In Scenario 1: fail_rate = log(2) / 15,
  fail_rate = log(2) / c(15, 15, 30),
  dropout_rate = 0.001,
  # In Scenario 1: hr = c(1, .6)
  hr = c(1, .6, .85)
)
enroll_rate |>
  gt() |>
  tab_header(title = "Enrollment Table of Scenario 2")
```

| Enrollment Table of Scenario 2 |          |      |
|--------------------------------|----------|------|
| stratum                        | duration | rate |
| All                            | 12       | 1    |

``` r
fail_rate |>
  gt() |>
  tab_header(title = "Failure Table of Scenario 2")
```

| Failure Table of Scenario 2 |          |            |              |      |
|-----------------------------|----------|------------|--------------|------|
| stratum                     | duration | fail_rate  | dropout_rate | hr   |
| All                         | 6        | 0.04620981 | 0.001        | 1.00 |
| All                         | 10       | 0.04620981 | 0.001        | 0.60 |
| All                         | 100      | 0.02310491 | 0.001        | 0.85 |

``` r
tab <- NULL

for (trial_duration in seq(20, 60, 4)) {
  # Fleming-Harrington rho=0, gamma=0.5 test
  fh_05 <- gs_design_wlr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = 1,
    alpha = 0.025, beta = 0.15,
    weight = list(method = "fh", param = list(rho = 0, gamma = 0.5)),
    upper = gs_b,
    upar = qnorm(.975),
    lower = gs_b,
    lpar = -Inf,
    analysis_time = trial_duration
  ) |> to_integer()

  # Regular logrank test
  fh_00 <- gs_power_wlr(
    enroll_rate = fh_05$enroll_rate,
    fail_rate = fail_rate,
    ratio = 1,
    weight = "logrank",
    upper = gs_b,
    upar = qnorm(.975),
    lower = gs_b,
    lpar = -Inf,
    analysis_time = trial_duration,
    event = .1
  )

  # MaxCombo test
  mc2_test <- data.frame(
    rho = 0, gamma = c(0, .5), tau = -1,
    test = 1:2, analysis = 1, analysis_time = trial_duration
  )
  mc_2 <- gs_power_combo(
    enroll_rate = fh_05$enroll_rate,
    fail_rate = fail_rate,
    fh_test = mc2_test,
    upper = gs_spending_combo,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_combo,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.01)
  )

  # MaxCombo test
  mc3_test <- data.frame(
    rho = c(0, 0, .5), gamma = c(0, .5, .5), tau = -1,
    test = 1:3, analysis = 1, analysis_time = trial_duration
  )

  mc_3 <- gs_power_combo(
    enroll_rate = fh_05$enroll_rate,
    fail_rate = fail_rate,
    fh_test = mc3_test,
    upper = gs_spending_combo,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_combo,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.01)
  )

  # MaxCombo test
  mc4_test <- data.frame(
    rho = c(0, 0, .5, .5), gamma = c(0, .5, .5, 0), tau = -1,
    test = 1:4, analysis = 1, analysis_time = trial_duration
  )

  mc_4 <- gs_power_combo(
    enroll_rate = fh_05$enroll_rate,
    fail_rate = fail_rate, fh_test = mc4_test,
    upper = gs_spending_combo,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
    lower = gs_spending_combo,
    lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.01)
  )

  # Magirr-Burman rho=-1, gamma=0, tau = 6 test
  mb_6 <- gs_power_wlr(
    enroll_rate = fh_05$enroll_rate,
    fail_rate = fail_rate,
    ratio = 1,
    weight = list(method = "fh", param = list(rho = -1, gamma = 0, tau = 15)),
    upper = gs_b,
    lower = gs_b,
    upar = qnorm(.975),
    lpar = -Inf,
    analysis_time = trial_duration,
    event = .1
  )

  tab_new <- tibble(
    `Study duration` = trial_duration,
    N = fh_05$analysis$n[1],
    Events = fh_05$analysi$event[1],
    `Events/N` = Events / N,
    # We use the AHR from regular WLR as the AHR of different MaxCombo test
    AHR = as.numeric(fh_00$analysis$ahr[1]),
    `FH(0, 0.5) power` = fh_05$bound$probability[1],
    `FH(0, 0) power` = fh_00$bound$probability[1],
    `MC2 power` = mc_2$bound$probability[1],
    `MC4 power` = mc_4$bound$probability[1],
    `MC3 power` = mc_3$bound$probability[1],
    `MB6 power` = mb_6$bound$probability[1]
  )

  tab <- rbind(tab, tab_new)
}

tab |>
  gt() |>
  fmt_number(columns = c(2, 3), decimals = 1) |>
  fmt_number(columns = 4, decimals = 2) |>
  fmt_number(columns = 5, decimals = 4) |>
  fmt_number(columns = 6:11, decimals = 2)
```

| Study duration | N       | Events | Events/N | AHR    | FH(0, 0.5) power | FH(0, 0) power | MC2 power | MC4 power | MC3 power | MB6 power |
|----------------|---------|--------|----------|--------|------------------|----------------|-----------|-----------|-----------|-----------|
| 20             | 1,226.0 | 518.0  | 0.42     | 0.8104 | 0.85             | 0.67           | 0.82      | 0.81      | 0.82      | 0.79      |
| 24             | 928.0   | 450.0  | 0.48     | 0.7912 | 0.85             | 0.70           | 0.82      | 0.81      | 0.82      | 0.81      |
| 28             | 876.0   | 466.0  | 0.53     | 0.7892 | 0.85             | 0.72           | 0.82      | 0.82      | 0.82      | 0.82      |
| 32             | 894.0   | 507.0  | 0.57     | 0.7929 | 0.85             | 0.74           | 0.83      | 0.82      | 0.83      | 0.84      |
| 36             | 904.0   | 544.0  | 0.60     | 0.7960 | 0.85             | 0.76           | 0.83      | 0.82      | 0.83      | 0.85      |
| 40             | 912.0   | 577.0  | 0.63     | 0.7984 | 0.85             | 0.77           | 0.83      | 0.83      | 0.84      | 0.87      |
| 44             | 916.0   | 605.0  | 0.66     | 0.8005 | 0.85             | 0.78           | 0.83      | 0.83      | 0.84      | 0.88      |
| 48             | 918.0   | 630.0  | 0.69     | 0.8022 | 0.85             | 0.79           | 0.83      | 0.84      | 0.84      | 0.90      |
| 52             | 918.0   | 652.0  | 0.71     | 0.8037 | 0.85             | 0.80           | 0.83      | 0.84      | 0.85      | 0.91      |
| 56             | 920.0   | 672.0  | 0.73     | 0.8050 | 0.85             | 0.80           | 0.84      | 0.84      | 0.85      | 0.92      |
| 60             | 918.0   | 690.0  | 0.75     | 0.8061 | 0.85             | 0.81           | 0.84      | 0.85      | 0.85      | 0.93      |
