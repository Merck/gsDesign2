# Computing spending boundaries in group sequential design

``` r
library(gsDesign2)
library(gsDesign)
```

## Introduction

We compare derivation of different spending bounds using the gsDesign2
and gsDesign packages. In gsDesign, there are 6 types of bounds. We
demonstrate here how to replicate these using gsDesign2. In gsDesign2,
the
[`gs_spending_bound()`](https://merck.github.io/gsDesign2/reference/gs_spending_bound.md)
function can be used to derive spending boundaries for all group
sequential design derivations and power calculations. We demonstrate
with the
[`gs_design_ahr()`](https://merck.github.io/gsDesign2/reference/gs_design_ahr.md)
function here, using designs under proportional hazards assumptions to
compare with
[`gsDesign::gsSurv()`](https://keaven.github.io/gsDesign/reference/nSurv.html).
Since the sample size methods differ between the
[`gsDesign2::gs_design_ahr()`](https://merck.github.io/gsDesign2/reference/gs_design_ahr.md)
and
[`gsDesign::gsSurv()`](https://keaven.github.io/gsDesign/reference/nSurv.html)
functions, we use continuous sample sizes so that spending bounds
(Z-values, nominal \\p\\-values, spending) should be identical except
where noted. Indeed, we are able to reproduce bounds to a high degree of
accuracy. Due to the different sample size methods, sample size and
other boundary approximations vary slightly.

We also present a seventh example to implement a futility bound based on
observed hazard ratio as well as a Haybittle-Peto-like efficacy bound.
In particular, the futility bound would be difficult to implement using
the gsDesign package while it is straightforward using gsDesign2.

For the last two examples, we implement integer sample size and event
counts using the
[`to_integer()`](https://merck.github.io/gsDesign2/reference/to_integer.md)
function for the gsDesign2 package and the
[`toInteger()`](https://keaven.github.io/gsDesign/reference/toInteger.html)
function for the gsDesign package. This would generally would be used
for all cases other than when we are comparing package computations as
in Examples 1–5.

For all of our examples, we will use the following design assumptions:

``` r
trial_duration <- 36 # Planned trial duration
info_frac <- c(.35, .7, 1) # Information fraction at analyses
# 16 month planned enrollment with constant rate
enroll_rate <- define_enroll_rate(duration = 16, rate = 1)
# Minimum follow-up for gsSurv() (computed)
minfup <- trial_duration - sum(enroll_rate$duration)
# Failure rates
fail_rate <- define_fail_rate(
  duration = Inf, # Single time period, exponential failure
  fail_rate = log(2) / 12, # Exponential time-to-event with 12 month median
  hr = .7, # Proportional hazards
  dropout_rate = -log(.99) / 12 # 1% dropout rate per year
)
alpha <- 0.025 # Type I error (one-sided)
beta <- 0.15 # 85% power = 15% Type II error
ratio <- 1 # Randomization ratio (experimental / control)
```

The choice of Type II error of 0.15 corresponding to 85% power is
intentional. This allows for more impactful futility bounds at interim
analyses. Many teams may decide on the more typical 90% power
(`beta = .1`), but this can make futility bounds less likely to impact
early decisions.

## Examples

Analogous to the gsDesign package, we look at 6 variations on
combinations of efficacy and futility bounds.

### Example 1: Efficacy bound only

One-sided design has only an efficacy bound. An easy way to do this is
to use a fixed bound (`lower = gs_b`) with negative infinite bounds
(`lpar = rep(-Inf, 3)`); in the summary table produced, infinite bounds
do not appear. The upper bound implements a spending bound
(`upper = gs_spending_bound`) and the list of objects provided in `upar`
describe the spending function and any associated parameters. The only
parts of the `upar` list used here are `sf = gsDesign::sfLDOF` to select
a Lan-DeMets spending function that approximates an O’Brien-Fleming
bound. The `total_spend = alpha` sets the total spending to the targeted
Type I error for the study. The upper bound provides the Type I error
control for the design as it is not specified elsewhere.

``` r
upar <- list(sf = gsDesign::sfLDOF, total_spend = alpha, param = NULL)

one_sided <- gsDesign2::gs_design_ahr(
  enroll_rate = enroll_rate, fail_rate = fail_rate,
  ratio = ratio, beta = beta,
  # Information fraction at analyses and trial duration
  info_frac = info_frac, analysis_time = trial_duration,
  # Precision parameters for computations
  r = 32, tol = 1e-08,
  # Use NULL information for Type I error, H1 information for Type II error (power)
  info_scale = "h0_h1_info", # Default
  # Upper spending bound and corresponding parameter(s)
  upper = gs_spending_bound, upar = upar,
  # No lower bound
  lower = gs_b, lpar = rep(-Inf, 3)
)

one_sided |>
  summary() |>
  gsDesign2::as_gt(title = "Efficacy bound only", subtitle = "alpha-spending")
```

[TABLE]

Now we check this with `gsDesign::gsSurv().` As noted above, sample size
and event counts vary slightly from the design derived using
[`gs_design_ahr()`](https://merck.github.io/gsDesign2/reference/gs_design_ahr.md).
This also results in slightly different crossing probabilities under the
alternate hypothesis at interim analyses as well as slightly different
approximate hazard ratios required to cross bounds.

``` r
oneSided <- gsSurv(
  alpha = alpha, beta = beta, timing = info_frac, T = trial_duration, minfup = minfup,
  lambdaC = fail_rate$fail_rate, eta = fail_rate$dropout_rate, hr = fail_rate$hr,
  r = 32, tol = 1e-08, # Precision parameters for computations
  test.type = 1, # One-sided bound; efficacy only
  # Upper bound parameters
  sfu = upar$sf, sfupar = upar$param,
)
oneSided |> gsBoundSummary()
#>     Analysis              Value Efficacy
#>    IA 1: 35%                  Z   3.6128
#>       N: 356        p (1-sided)   0.0002
#>  Events: 100       ~HR at bound   0.4852
#>    Month: 14   P(Cross) if HR=1   0.0002
#>              P(Cross) if HR=0.7   0.0338
#>    IA 2: 70%                  Z   2.4406
#>       N: 394        p (1-sided)   0.0073
#>  Events: 200       ~HR at bound   0.7079
#>    Month: 23   P(Cross) if HR=1   0.0074
#>              P(Cross) if HR=0.7   0.5341
#>        Final                  Z   2.0002
#>       N: 394        p (1-sided)   0.0227
#>  Events: 286       ~HR at bound   0.7891
#>    Month: 36   P(Cross) if HR=1   0.0250
#>              P(Cross) if HR=0.7   0.8500
```

Comparing Z-value bounds directly we see they are the same through
approximately 6 digits with precision parameters chosen (`r=32`,
`tol=1e-08`):

``` r
one_sided$bound$z - oneSided$upper$bound
#> [1] -1.349247e-07  9.218765e-07  3.515345e-07
```

### Example 2: Symmetric 2-sided design

We now derive a symmetric 2-sided design. This requires use of the
argument `h1_spending = FALSE` to use \\\alpha\\-spending for both the
upper and lower bounds. While the lower bound is labeled as a futility
bound in the table, it would be better termed an efficacy bound for
control better than experimental treatment.

``` r
upar <- list(sf = gsDesign::sfLDOF, total_spend = alpha, param = NULL)
lpar <- list(sf = gsDesign::sfLDOF, total_spend = alpha, param = NULL)

symmetric <- gs_design_ahr(
  enroll_rate = enroll_rate, fail_rate = fail_rate,
  ratio = ratio, beta = beta,
  # Information fraction at analyses and trial duration
  info_frac = info_frac, analysis_time = trial_duration,
  # Precision parameters for computations
  r = 32, tol = 1e-08,
  # Use NULL information for Type I error, H1 information for power
  info_scale = "h0_h1_info", # Default
  # Function and parameter(s) for upper spending bound
  upper = gs_spending_bound, upar = upar,
  lower = gs_spending_bound, lpar = lpar,
  # Symmetric designs use binding bounds
  binding = TRUE,
  h1_spending = FALSE # Use null hypothesis spending for lower bound
)

symmetric |>
  summary() |>
  gsDesign2::as_gt(
    title = "2-sided Symmetric Design",
    subtitle = "Single spending function"
  )
```

[TABLE]

We compare with
[`gsDesign::gsSurv()`](https://keaven.github.io/gsDesign/reference/nSurv.html).

``` r
Symmetric <-
  gsSurv(
    test.type = 2, # Two-sided symmetric bound
    alpha = alpha, beta = beta, timing = info_frac, T = trial_duration, minfup = minfup, r = 32, tol = 1e-08,
    lambdaC = fail_rate$fail_rate, eta = fail_rate$dropout_rate, hr = fail_rate$hr,
    sfu = upar$sf, sfupar = upar$param
  )
Symmetric |> gsBoundSummary()
#>     Analysis              Value Efficacy Futility
#>    IA 1: 35%                  Z   3.6128  -3.6128
#>       N: 356        p (1-sided)   0.0002   0.0002
#>  Events: 100       ~HR at bound   0.4852   2.0609
#>    Month: 14   P(Cross) if HR=1   0.0002   0.0002
#>              P(Cross) if HR=0.7   0.0338   0.0000
#>    IA 2: 70%                  Z   2.4406  -2.4406
#>       N: 394        p (1-sided)   0.0073   0.0073
#>  Events: 200       ~HR at bound   0.7079   1.4126
#>    Month: 23   P(Cross) if HR=1   0.0074   0.0074
#>              P(Cross) if HR=0.7   0.5341   0.0000
#>        Final                  Z   2.0002  -2.0002
#>       N: 394        p (1-sided)   0.0227   0.0227
#>  Events: 286       ~HR at bound   0.7891   1.2673
#>    Month: 36   P(Cross) if HR=1   0.0250   0.0250
#>              P(Cross) if HR=0.7   0.8500   0.0000
```

Comparing Z-value bounds directly, we again see approximately 6 digits
of accuracy.

``` r
dplyr::filter(symmetric$bound, bound == "upper")$z - Symmetric$upper$bound
#> [1] -1.349247e-07  9.218765e-07  4.092976e-07
dplyr::filter(symmetric$bound, bound == "lower")$z - Symmetric$lower$bound
#> [1]  1.349247e-07 -9.218765e-07 -4.092976e-07
```

### Example 3: Asymmetric 2-sided design with \\\beta\\-spending and binding futility

Designs with binding futility bounds are generally not considered
acceptable for Phase 3 trials as Type I error is not controlled if a
futility bound is crossed and the trial continues, a not infrequent
occurrence. A binding futility bound means that Type I error
computations assume that a trial stops when a futility bound is crossed.
If the trial continues after a futility bound has been crossed, Type I
error is no longer controlled with the computed efficacy bound. For a
Phase 2b study, this may be acceptable and results in a slightly smaller
sample size and less stringent efficacy bounds after the first analysis
than a comparable design with a non-binding futility bound presented in
Example 4.

``` r
upar <- list(sf = gsDesign::sfLDOF, total_spend = alpha, param = NULL)
lpar <- list(sf = gsDesign::sfHSD, total_spend = beta, param = -.5)

asymmetric_binding <- gs_design_ahr(
  enroll_rate = enroll_rate, fail_rate = fail_rate,
  ratio = ratio, beta = beta,
  # Information fraction at analyses and trial duration
  info_frac = info_frac, analysis_time = trial_duration,
  # Precision parameters for computations
  r = 32, tol = 1e-08,
  # Use NULL information for Type I error, H1 information for Type II error and power
  info_scale = "h0_h1_info",
  # Function and parameter(s) for upper spending bound
  upper = gs_spending_bound, upar = upar,
  lower = gs_spending_bound, lpar = lpar,
  # Asymmetric beta-spending design using binding bounds
  binding = TRUE,
  h1_spending = TRUE # Use beta-spending for futility
)

asymmetric_binding |>
  summary() |>
  gsDesign2::as_gt(
    title = "2-sided asymmetric design with binding futility",
    subtitle = "Both alpha- and beta-spending used"
  )
```

[TABLE]

We compare with
[`gsDesign::gsSurv()`](https://keaven.github.io/gsDesign/reference/nSurv.html).

``` r
asymmetricBinding <- gsSurv(
  test.type = 3, # Two-sided asymmetric bound, binding futility
  alpha = alpha, beta = beta, timing = info_frac, T = trial_duration, minfup = minfup, r = 32, tol = 1e-07,
  lambdaC = fail_rate$fail_rate, eta = fail_rate$dropout_rate, hr = fail_rate$hr,
  sfu = upar$sf, sfupar = upar$param, sfl = lpar$sf, sflpar = lpar$param
)
asymmetricBinding |> gsBoundSummary()
#>     Analysis              Value Efficacy Futility
#>    IA 1: 35%                  Z   3.6128   0.1436
#>       N: 380        p (1-sided)   0.0002   0.4429
#>  Events: 107       ~HR at bound   0.4971   0.9726
#>    Month: 14   P(Cross) if HR=1   0.0002   0.5571
#>              P(Cross) if HR=0.7   0.0387   0.0442
#>    IA 2: 70%                  Z   2.4382   1.1807
#>       N: 422        p (1-sided)   0.0074   0.1189
#>  Events: 214       ~HR at bound   0.7164   0.8509
#>    Month: 23   P(Cross) if HR=1   0.0074   0.8913
#>              P(Cross) if HR=0.7   0.5679   0.0969
#>        Final                  Z   1.9232   1.9232
#>       N: 422        p (1-sided)   0.0272   0.0272
#>  Events: 306       ~HR at bound   0.8024   0.8024
#>    Month: 36   P(Cross) if HR=1   0.0250   0.9750
#>              P(Cross) if HR=0.7   0.8500   0.1500
```

Comparing Z-value bounds directly, we again see approximately 6 digits
of accuracy in spite of needing to relaxing accuracy to `tol = 1e-07` in
the call to
[`gsSurv()`](https://keaven.github.io/gsDesign/reference/nSurv.html) in
order to get convergence.

``` r
dplyr::filter(asymmetric_binding$bound, bound == "upper")$z - asymmetricBinding$upper$bound
#> [1] -1.349247e-07  2.505869e-04  6.494308e-03
dplyr::filter(asymmetric_binding$bound, bound == "lower")$z - asymmetricBinding$lower$bound
#> [1] -0.02803395 -0.02670879 -0.01598602
```

### Example 4: Asymmetric 2-sided design with \\\beta\\-spending and non-binding futility bound

In the gsDesign package, asymmetric designs with non-binding
\\\beta\\-spending used for futility are the default design. The
objectives of this type of design include:

- Meaningful futility bounds to stop a trial early if no treatment
  benefit is emerging for the experimental treatment vs. control.
- Type I error is controlled even if the trial continues after a
  futility bound is crossed.

``` r
upar <- list(sf = gsDesign::sfLDOF, total_spend = alpha, param = NULL)
lpar <- list(sf = gsDesign::sfHSD, total_spend = beta, param = -.5)

asymmetric_nonbinding <- gs_design_ahr(
  enroll_rate = enroll_rate, fail_rate = fail_rate,
  ratio = ratio, beta = beta,
  # Information fraction at analyses and trial duration
  info_frac = info_frac, analysis_time = trial_duration,
  # Precision parameters for computations
  r = 32, tol = 1e-08,
  # Use NULL information for Type I error, H1 info for Type II error and power
  info_scale = "h0_h1_info", # Default
  # Function and parameter(s) for upper spending bound
  upper = gs_spending_bound, upar = upar,
  lower = gs_spending_bound, lpar = lpar,
  # Asymmetric beta-spending design use binding bounds
  binding = FALSE,
  h1_spending = TRUE # Use beta-spending for futility
)

asymmetric_nonbinding |>
  summary() |>
  gsDesign2::as_gt(
    title = "2-sided asymmetric design with non-binding futility",
    subtitle = "Both alpha- and beta-spending used"
  )
```

[TABLE]

We compare with
[`gsDesign::gsSurv()`](https://keaven.github.io/gsDesign/reference/nSurv.html).

``` r
asymmetricNonBinding <- gsSurv(
  test.type = 4, # Two-sided asymmetric bound, non-binding futility
  alpha = alpha, beta = beta, timing = info_frac, T = trial_duration, minfup = minfup, r = 32, tol = 1e-08,
  lambdaC = fail_rate$fail_rate, eta = fail_rate$dropout_rate, hr = fail_rate$hr,
  sfu = upar$sf, sfupar = upar$param, sfl = lpar$sf, sflpar = lpar$param
)
asymmetricNonBinding |> gsBoundSummary()
#>     Analysis              Value Efficacy Futility
#>    IA 1: 35%                  Z   3.6128   0.1860
#>       N: 398        p (1-sided)   0.0002   0.4262
#>  Events: 112       ~HR at bound   0.5050   0.9654
#>    Month: 14   P(Cross) if HR=1   0.0002   0.5738
#>              P(Cross) if HR=0.7   0.0424   0.0442
#>    IA 2: 70%                  Z   2.4406   1.2406
#>       N: 440        p (1-sided)   0.0073   0.1074
#>  Events: 224       ~HR at bound   0.7215   0.8471
#>    Month: 23   P(Cross) if HR=1   0.0073   0.9020
#>              P(Cross) if HR=0.7   0.5901   0.0969
#>        Final                  Z   2.0002   2.0002
#>       N: 440        p (1-sided)   0.0227   0.0227
#>  Events: 320       ~HR at bound   0.7995   0.7995
#>    Month: 36   P(Cross) if HR=1   0.0215   0.9785
#>              P(Cross) if HR=0.7   0.8500   0.1500
```

Comparing Z-value bounds directly, we again see approximately 6 digits
of accuracy.

``` r
dplyr::filter(asymmetric_nonbinding$bound, bound == "upper")$z - asymmetricNonBinding$upper$bound
#> [1] -1.349247e-07  9.218765e-07  3.515345e-07
dplyr::filter(asymmetric_nonbinding$bound, bound == "lower")$z - asymmetricNonBinding$lower$bound
#> [1] -0.03267431 -0.03311079 -0.02427000
```

### Example 5: Asymmetric 2-sided design with null hypothesis spending and binding futility bound

Now we use null hypothesis probabilities to set futility bounds. The
parameter `alpha_star` is used to set the total spending for the
futility bound under the null hypothesis. For our example, this is set
to 0.5 which is a 50% probability of crossing the futility bound at the
interim and final analyses combined. The futility bound at the final
analysis really has no role, so we use the `test_lower` argument to
eliminate this evaluation at the final analysis. This is arbitrary and
largely selected so that the interim futility bounds can be meaningful
tests. In this case, more than a minor trend in favor of control at the
first or second interim will cross a futility bound. This is less
stringent than the \\\beta\\-spending bounds previously described, but
still address a potential ethical issue of continuing the trial when
more than a minor trend in favor of control is present.

``` r
alpha_star <- .5
upar <- list(sf = gsDesign::sfLDOF, total_spend = alpha, param = NULL)
lpar <- list(sf = gsDesign::sfHSD, total_spend = alpha_star, param = 1)

asymmetric_safety_binding <- gs_design_ahr(
  enroll_rate = enroll_rate, fail_rate = fail_rate,
  ratio = ratio, beta = beta,
  # Information fraction at analyses and trial duration
  info_frac = info_frac, analysis_time = trial_duration,
  # Precision parameters for computations
  r = 32, tol = 1e-08,
  # Use NULL information for Type I error, H1 information for Type II error
  info_scale = "h0_info",
  # Function and parameter(s) for upper spending bound
  upper = gs_spending_bound, upar = upar,
  lower = gs_spending_bound, lpar = lpar,
  test_lower = c(TRUE, TRUE, FALSE),
  # Asymmetric design use binding bounds
  binding = TRUE,
  h1_spending = FALSE # Use null-spending for futility
)

asymmetric_safety_binding |>
  summary() |>
  gsDesign2::as_gt(
    title = "2-sided asymmetric safety design with binding futility",
    subtitle = "Alpha-spending used for both bounds, asymmetrically"
  )
```

[TABLE]

``` r
asymmetricSafetyBinding <- gsSurv(
  test.type = 5, # Two-sided asymmetric bound, binding futility, H0 futility spending
  astar = alpha_star, # Total Type I error spend for futility
  alpha = alpha, beta = beta, timing = info_frac, T = trial_duration, minfup = minfup,
  lambdaC = fail_rate$fail_rate, eta = fail_rate$dropout_rate, hr = fail_rate$hr,
  sfu = upar$sf, sfupar = upar$param, sfl = lpar$sf, sflpar = lpar$param
)
asymmetricSafetyBinding |> gsBoundSummary()
#>     Analysis              Value Efficacy Futility
#>    IA 1: 35%                  Z   3.6128  -0.7271
#>       N: 356        p (1-sided)   0.0002   0.7664
#>  Events: 101       ~HR at bound   0.4856   1.1565
#>    Month: 14   P(Cross) if HR=1   0.0002   0.2336
#>              P(Cross) if HR=0.7   0.0340   0.0060
#>    IA 2: 70%                  Z   2.4405  -0.4203
#>       N: 394        p (1-sided)   0.0073   0.6629
#>  Events: 201       ~HR at bound   0.7082   1.0612
#>    Month: 23   P(Cross) if HR=1   0.0074   0.3982
#>              P(Cross) if HR=0.7   0.5353   0.0070
#>        Final                  Z   1.9979  -0.2531
#>       N: 394        p (1-sided)   0.0229   0.5999
#>  Events: 286       ~HR at bound   0.7895   1.0304
#>    Month: 36   P(Cross) if HR=1   0.0250   0.5000
#>              P(Cross) if HR=0.7   0.8500   0.0072
```

Comparing Z-value bounds directly, we again see approximately 6 digits
of accuracy. For
[`gsSurv()`](https://keaven.github.io/gsDesign/reference/nSurv.html)
this did not require the alternate arguments for `r` and `tol`.

``` r
dplyr::filter(asymmetric_safety_binding$bound, bound == "upper")$z - asymmetricSafetyBinding$upper$bound
#> [1] -1.349247e-07  9.211210e-07  4.185954e-07
dplyr::filter(asymmetric_safety_binding$bound, bound == "lower")$z - asymmetricSafetyBinding$lower$bound[1:2]
#> [1]  4.348992e-08 -3.276118e-08
```

### Example 6: Asymmetric 2-sided design with null hypothesis spending and non-binding futility bound

Again, we would recommend a non-binding bound presented here over the
binding bound in example 5. We again eliminate the final futility bound
using the `test_lower` argument. Addition, we show how to eliminate the
efficacy bound at interim 1 allowing a team to decide that it is too
early to stop a trial for efficacy without longer-term data.

``` r
upar <- list(sf = gsDesign::sfLDOF, total_spend = alpha, param = NULL)
lpar <- list(sf = gsDesign::sfHSD, total_spend = alpha_star, param = 1)

asymmetric_safety_nonbinding <- gs_design_ahr(
  enroll_rate = enroll_rate, fail_rate = fail_rate,
  ratio = ratio, beta = beta,
  # Information fraction at analyses and trial duration
  info_frac = info_frac, analysis_time = trial_duration,
  # Precision parameters for computations
  r = 32, tol = 1e-08,
  # Use NULL information for Type I error, H1 information for Type II error
  info_scale = "h0_info",
  # Function and parameter(s) for upper spending bound
  upper = gs_spending_bound, upar = upar,
  test_upper = c(FALSE, TRUE, TRUE),
  lower = gs_spending_bound, lpar = lpar,
  test_lower = c(TRUE, TRUE, FALSE),
  # Asymmetric design use non-binding bounds
  binding = FALSE,
  h1_spending = FALSE # Use null-spending for futility
) |> to_integer()

asymmetric_safety_nonbinding |>
  summary() |>
  gsDesign2::as_gt(
    title = "2-sided asymmetric safety design with non-binding futility",
    subtitle = "Alpha-spending used for both bounds, asymmetrically"
  ) |>
  gt::tab_footnote(footnote = "Integer-based sample size and event counts")
```

[TABLE]

The corresponding
[`gsDesign::gsSurv()`](https://keaven.github.io/gsDesign/reference/nSurv.html)
design is not strictly comparable since the option to eliminate some
futility and efficacy analyses is not enabled.

``` r
asymmetricSafetyNonBinding <- gsSurv(
  test.type = 6, # Two-sided asymmetric bound, binding futility, H0 futility spending
  astar = alpha_star, # Total Type I error spend for futility
  alpha = alpha, beta = beta, timing = info_frac, T = trial_duration, minfup = minfup, r = 32, tol = 1e-08,
  lambdaC = fail_rate$fail_rate, eta = fail_rate$dropout_rate, hr = fail_rate$hr,
  sfu = upar$sf, sfupar = upar$param, sfl = lpar$sf, sflpar = lpar$param
)
asymmetricSafetyBinding |> gsBoundSummary()
#>     Analysis              Value Efficacy Futility
#>    IA 1: 35%                  Z   3.6128  -0.7271
#>       N: 356        p (1-sided)   0.0002   0.7664
#>  Events: 101       ~HR at bound   0.4856   1.1565
#>    Month: 14   P(Cross) if HR=1   0.0002   0.2336
#>              P(Cross) if HR=0.7   0.0340   0.0060
#>    IA 2: 70%                  Z   2.4405  -0.4203
#>       N: 394        p (1-sided)   0.0073   0.6629
#>  Events: 201       ~HR at bound   0.7082   1.0612
#>    Month: 23   P(Cross) if HR=1   0.0074   0.3982
#>              P(Cross) if HR=0.7   0.5353   0.0070
#>        Final                  Z   1.9979  -0.2531
#>       N: 394        p (1-sided)   0.0229   0.5999
#>  Events: 286       ~HR at bound   0.7895   1.0304
#>    Month: 36   P(Cross) if HR=1   0.0250   0.5000
#>              P(Cross) if HR=0.7   0.8500   0.0072
```

### Example 7: Alternate bound types

We consider two types of alternative boundary computation approaches.

- Computing futility bounds based on a hazard ratio.
- Computing efficacy bounds with a Haybittle-Peto or a related
  Fleming-Harrington-O’Brien approach.

We begin with a futility bound. We will consider a non-binding futility
bound as it does not impact the efficacy bound. Assume the clinical
trial team wishes to stop the trial at the first two interim analyses if
a targeted interim hazard ratio is not achieved. This approach can
require a bit of iteration (trial and error) to incorporate the final
design endpoint count; we skip over this iteration here. We assume we
wish to consider stopping for futility if a hazard ratio greater than 1
and 0.9 are observed at interim analyses 1 and 2 with 104 and 209 events
observed, respectively. The final analysis is planned for 300 events.

``` r
# Targeted events at interim and final analysis
# This is based on above designs and then adjusted, as necessary
targeted_events <- c(104, 209, 300)
```

We wish to translate the hazard ratios specified to corresponding
Z-values; this can be done as follows.

``` r
interim_futility_z <- -gsDesign::hrn2z(hr = c(1, .9), n = targeted_events[1:2])
interim_futility_z
#> [1] 0.0000000 0.7615897
```

We will add a final futility bound of `-Inf`, indicating no final
futility analysis; this gives us a vector of Z-value bounds for all
analyses. For this type of bound, Type II error will be computed rather
based on bounds rather than the spending approach were bounds are
computed based on specified spending.

``` r
lower <- gs_b
# Allows specifying fixed Z-values for futility
# Translated HR bounds to Z-value scale
lpar <- c(interim_futility_z, -Inf)
```

For the efficacy bound, we first consider a Haybittle-Peto fixed bound
for interim analyses. Using a Bonferroni approach, we test at nominal
levels 0.001, 0.001, and 0.023 at the 3 analyses. By not accounting for
correlations, this will actually not quite use all of the 0.025 1-sided
Type I error allowed. We allow the user to substitute this code for what
follows to verify this.

``` r
upper <- gs_b
upar <- qnorm(c(.001, .001, .0023), lower.tail = FALSE)
```

The alternative approach is to use a fixed spending approach at each
analysis as suggested by Fleming, Harrington, and O’Brien (1984). Again,
with some iteration not shown, we use a piecewise linear spending
function to select interim bounds that match the desired Haybittle-Peto
interim bounds. However, using this approach a slightly more liberal
final bound is achieved that still controls Type I error.

``` r
upper <- gs_spending_bound
upar <- list(
  sf = gsDesign::sfLinear,
  total_spend = alpha,
  param = c(targeted_events[1:2] / targeted_events[3], c(.001, .0018) / .025),
  timing = NULL
)

asymmetric_fixed_bounds <- gs_design_ahr(
  enroll_rate = enroll_rate, fail_rate = fail_rate,
  ratio = ratio, beta = beta,
  # Information fraction at analyses and trial duration
  info_frac = info_frac, analysis_time = trial_duration,
  # Precision parameters for computations
  r = 32, tol = 1e-08,
  # Use NULL information for Type I error, H1 information for Type II error
  info_scale = "h0_info",
  # Function and parameter(s) for upper spending bound
  upper = upper, upar = upar,
  lower = lower, lpar = lpar,
  # Non-binding futility bounds
  binding = FALSE
) |> to_integer()

asymmetric_fixed_bounds |>
  summary() |>
  gsDesign2::as_gt(
    title = "2-sided asymmetric safety design with fixed non-binding futility",
    subtitle = "Futility bounds computed to approximate HR"
  ) |>
  gt::tab_footnote(footnote = "Integer-based sample size and event counts")
```

[TABLE]

We see that the targeted bounds are achieved with nominal \\p\\-values
of 0.0001 for each interim efficacy bound and the targeted hazard ratios
at interim futility bounds. With these methods, trial designers have
more control over design characteristics they may desire. In particular,
we note that the Haybittle-Peto efficacy bounds are less stringent at
the first interim and more stringent at the second interim than
corresponding O’Brien-Fleming-like bounds we computed with the spending
approach. This may or may not be desirable.

## References

Fleming, Thomas R, David P Harrington, and Peter C O’Brien. 1984.
“Designs for Group Sequential Tests.” *Controlled Clinical Trials* 5
(4): 348–61.
