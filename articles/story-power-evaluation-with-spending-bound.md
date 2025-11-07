# Power evaluation with spending bounds

``` r
library(tibble)
library(gt)
library(gsDesign2)
```

## Overview

This vignette covers how to compute power or Type I error for a design
derived with a spending bound. We will write this with a general
non-constant treatment effect using
[`gs_design_npe()`](https://merck.github.io/gsDesign2/reference/gs_power_design_npe.md)
to derived the design under one parameter setting and computing power
under another setting. We will use a trial with a binary endpoint to
enable a full illustration.

## Scenario for Consideration

We consider a scenario largely based on the CAPTURE study (Capture
Investigators et al. (1997)) where the primary endpoint was a composite
of death, acute myocardial infarction or the need for recurrent
percutaneous intervention within 30 days of randomization.

The detailed introduction of this trial is listed as follows.

- We consider a 2-arm trial with an experimental arm and a control arm.

- We will assume \\K=3\\ analyses after \\350\\, \\700\\, and \\1400\\
  patients have been observed with equal randomization between the
  treatment groups.

- The primary endpoint for the trial is a binary indicator for each
  participant if they have a failed outcome. For this case, we consider
  the parameter \\ \theta = p_1 - p \_2 \\ where \\p_1\\ denotes the
  probability that a trial participant in the control group experiences
  a failure and \\p_2\\ represents the same probability for a trial
  participant in the experimental group.

- The study was designed with approximately 80% power (Type II error
  \\\beta = 1 - 0.8 = 0.2\\) and 2.5% one-sided Type I error (\\\alpha =
  0.025\\) to detect a reduction from a 15% event rate (\\p_1 = 0.15\\)
  in the control group to 10% (\\p_2 = 0.1\\) in the experimental group.

``` r
p0 <- 0.15 # assumed failure rate in control group
p1 <- 0.10 # assumed failure rate in experimental group
alpha <- 0.025 # type I error
beta <- 0.2 # type II error for 80% power
```

In this example, the parameter of interest is \\\theta = p_1 - p_2\\. We
denote the alternate hypothesis as \\ H_1: \theta = \theta_1= p_1^1 -
p_2^1 = 0.15 - 0.10 = 0.05 \\ and the null hypothesis \\ H_0: \theta =
\theta_0 = 0 = p_1^0 - p_2^0 \\ where \\p^0_1 = p^0_2= (p_1^1+p_2^1)/2 =
0.125\\ as laid out in Lachin (2009).

We note that had we considered a success outcome such as objective
response in an oncology study, we would let \\p_1\\ denote the
experimental group and \\p_2\\ the control group response rate. Thus, we
always set up the notation so the \\p_1\>p_2\\ represents superiority
for the experimental group.

## Notations

We assume

- \\k\\: the index of analysis, i.e., \\k = 1, \ldots, K\\;
- \\i\\: the index of arm, i.e., \\i = 1\\ for the control group and \\i
  = 2\\ for the experimental group;
- \\n\_{ik}\\: the number of subjects in group \\i\\ and analysis \\k\\;
- \\n_k\\: the number of subjects at analysis \\k\\, i.e., \\n_k =
  n\_{1k} + n\_{2k}\\;
- \\X\_{ij}\\: the independent random variable whether the \\j\\-th
  subject in group \\i\\ has response, i.e, \\ X\_{ij} \sim
  \text{Bernoulli}(p_i); \\
- \\Y\_{ik}\\: the number of subject having response in group \\i\\ and
  analysis \\k\\, i.e., \\ Y\_{ik} = \sum\_{j = 1}^{n\_{ik}} X\_{ij}; \\

## Statistical Testing

In this section, we will discuss the estimation of statistical
information and variance of proportion under both null hypothesis \\H_0:
p_1^0 = p_2^0 \equiv p_0\\ and alternative hypothesis \\H_1: \theta =
\theta_1= p_1^1 - p_2^1\\. Then, we will introduce the test statistics
in the group sequential design.

### Estimation of Statistical Information under H1

Under the alternative hypothesis, one can estimate the proportion of
failures in group \\i\\ at analysis \\k\\ as \\ \hat{p}\_{ik} =
Y\_{ik}/n\_{ik}. \\ We note its variance is \\ \text{Var}(\hat
p\_{ik})=\frac{p\_{i}(1-p_i)}{n\_{ik}}, \\ and its consistent estimator
\\ \widehat{\text{Var}}(\hat p\_{ik})=\frac{\hat p\_{ik}(1-\hat
p\_{ik})}{n\_{ik}}, \\ for any \\i = 1, 2\\ and \\k = 1, 2, \ldots, K\\.
Letting \\\hat\theta_k = \hat p\_{1k} - \hat p\_{2k},\\ we also have \\
\sigma^2_k \equiv \text{Var}(\hat\theta_k) =
\frac{p_1(1-p_1)}{n\_{1k}}+\frac{p_2(1-p_2)}{n\_{2k}}, \\ its consistent
estimator \\ \hat\sigma^2_k = \frac{\hat p\_{1k}(1-\hat
p\_{1k})}{n\_{1k}}+\frac{\hat p\_{2k}(1-\hat p\_{2k})}{n\_{2k}}, \\

Statistical information for each of these quantities and their
corresponding estimators are denoted by \\ \left\\ \begin{align}
\mathcal{I}\_k = &1/\sigma^2_k,\\ \mathcal{\hat I}\_k = &1/\hat
\sigma^2_k, \end{align} \right. \\

### Estimation of Statistical Information under H0

Under the null hypothesis, one can estimate the proportion of failures
in group \\i\\ at analysis \\k\\ as we estimate \\ \hat{p}\_{0k} =
\frac{Y\_{1k}+ Y\_{2k}}{n\_{1k}+ n\_{2k}} = \frac{n\_{1k}\hat p\_{1k} +
n\_{2k}\hat p\_{2k}}{n\_{1k} + n\_{2k}}. \\ The corresponding null
hypothesis estimator \\ \hat\sigma^2\_{0k} \equiv
\widehat{\text{Var}}(\hat{p}\_{0k}) = \hat p\_{0k}(1-\hat
p\_{0k})\left(\frac{1}{n\_{1k}}+ \frac{1}{n\_{2k}}\right), \\ for any
\\k = 1,2, \ldots, K\\.

Statistical information for each of these quantities and their
corresponding estimators are denoted by \\ \left\\ \begin{align}
\mathcal{I}\_{0k} =& 1/ \sigma^2\_{0k},\\ \mathcal{\hat I}\_{0k} =&
1/\hat \sigma^2\_{0k}, \end{align} \right. \\ for any \\k = 1, 2,
\ldots, K\\.

### Testing Statistics

Testing, as recommended by Lachin (2009), is done with the large sample
test with the null hypothesis variance estimate and without continuity
correction: \\ Z_k = \hat\theta_k/\hat\sigma\_{0k}=\frac{\hat p\_{1k} -
\hat p\_{2k}}{\sqrt{(1/n\_{1k}+ 1/n\_{2k})\hat p\_{0k}(1-\hat p\_{0k})}
}, \\ which is asymptotically \\\text{Normal}(0,1)\\ if \\p_1 = p_2\\
and \\\text{Normal}(0, \sigma\_{0k}^2/\sigma_k^2)\\ more generally for
any \\p_1, p_2\\ and \\k = 1, 2, \ldots, K\\.

If we further assume a constant proportion \\\xi_i\\ randomized to each
group \\i=1,2.\\ Thus, \\ Z_k \approx \frac{\sqrt{n_k}(\hat p\_{1k} -
\hat p\_{2k})}{\sqrt{(1/\xi_1+ 1/\xi_2)p\_{0}(1- p_0)} }. \\

Then, we have the asymptotic distribution \\ Z_k \sim \text{Normal}
\left( \sqrt{n_k}\frac{p_1 - p_2}{\sqrt{(1/\xi_1+ 1/\xi_2) p_0(1- p_0)}
}, \sigma^2\_{0k}/\sigma^2\_{1k} \right), \\ where we note that \\
\sigma^2\_{0k}/\sigma^2\_{1k} = \frac{ p_0(1-p_0)\left(1/\xi_1+
1/\xi_2\right)}{p_1(1-p_1)/\xi_1+p_2(1-p_2)/\xi_2}. \\ We also note by
definition that \\\sigma^2\_{0k}/\sigma^2\_{1k}=\mathcal I_k/\mathcal
I\_{0k}.\\ Based on an input \\p_1, p_2, n_k, \xi_1, \xi_2 = 1-\xi_1\\
we will compute \\\theta, \mathcal{I}\_k, \mathcal{I}\_{0k}\\ for any
\\k = 1, 2, \ldots, K\\.

We note that \\\chi^2=Z^2_k\\ is the \\\chi^2\\ test without continuity
correction as recommended by Gordon and Watson (1996). Note finally that
this extends in a straightforward way the non-inferiority test of
Farrington and Manning (1990) if the null hypothesis is \\\theta = p_1 -
p_2 - \delta = 0\\ for some non-inferiority margin \\\delta \> 0\\;
\\\delta \< 0\\ would correspond to what is referred to as
super-superiority Chan (2002), requiring that experimental therapy has
been shown to be superior to control by at least a margin
\\-\delta\>0\\.

## Power Calculations

We begin with developing a function `gs_info_binomial()` to calculate
the statistical information discussed above.

``` r
gs_info_binomial <- function(p1, p2, xi1, n, delta = NULL) {
  if (is.null(delta)) delta <- p1 - p2
  # Compute (constant) effect size at each analysis theta
  theta <- rep(p1 - p2, length(n))
  # compute null hypothesis rate, p0
  p0 <- xi1 * p1 + (1 - xi1) * p2
  # compute information based on p1, p2
  info <- n / (p1 * (1 - p1) / xi1 + p2 * (1 - p2) / (1 - xi1))
  # compute information based on null hypothesis rate of p0
  info0 <- n / (p0 * (1 - p0) * (1 / xi1 + 1 / (1 - xi1)))
  # compute information based on H1 rates of p1star, p2star
  p1star <- p0 + delta * xi1
  p2star <- p0 - delta * (1 - xi1)
  info1 <- n / (p1star * (1 - p1star) / xi1 + p2star * (1 - p2star) / (1 - xi1))

  out <- tibble(
    Analysis = seq_along(n),
    n = n,
    theta = theta,
    theta1 = rep(delta, length(n)),
    info = info,
    info0 = info0,
    info1 = info1
  )
  return(out)
}
```

For the CAPTURE trial, we have

``` r
h1 <- gs_info_binomial(p1 = .15, p2 = .1, xi1 = .5, n = c(350, 700, 1400))
h1 |> gt()
```

| Analysis | n    | theta | theta1 | info      | info0 | info1     |
|----------|------|-------|--------|-----------|-------|-----------|
| 1        | 350  | 0.05  | 0.05   | 804.5977  | 800   | 804.5977  |
| 2        | 700  | 0.05  | 0.05   | 1609.1954 | 1600  | 1609.1954 |
| 3        | 1400 | 0.05  | 0.05   | 3218.3908 | 3200  | 3218.3908 |

We can plug these into
[`gs_power_npe()`](https://merck.github.io/gsDesign2/reference/gs_power_design_npe.md)
with the intended spending functions. We begin with power under the
alternate hypothesis

``` r
gs_power_npe(
  theta = h1$theta,
  theta1 = h1$theta,
  info = h1$info,
  info0 = h1$info0,
  info1 = h1$info1,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfHSD, param = -2, total_spend = 0.2)
) |>
  gt() |>
  fmt_number(columns = 3:10, decimals = 4)
```

| analysis | bound | z       | probability | theta  | theta1 | info_frac | info       | info0      | info1      |
|----------|-------|---------|-------------|--------|--------|-----------|------------|------------|------------|
| 1        | upper | 4.3326  | 0.0017      | 0.0500 | 0.0500 | 0.2500    | 804.5977   | 800.0000   | 804.5977   |
| 2        | upper | 2.9632  | 0.1692      | 0.0500 | 0.0500 | 0.5000    | 1,609.1954 | 1,600.0000 | 1,609.1954 |
| 3        | upper | 1.9686  | 0.7939      | 0.0500 | 0.0500 | 1.0000    | 3,218.3908 | 3,200.0000 | 3,218.3908 |
| 1        | lower | −0.6292 | 0.0202      | 0.0500 | 0.0500 | 0.2500    | 804.5977   | 800.0000   | 804.5977   |
| 2        | lower | 0.2947  | 0.0537      | 0.0500 | 0.0500 | 0.5000    | 1,609.1954 | 1,600.0000 | 1,609.1954 |
| 3        | lower | 1.9441  | 0.1999      | 0.0500 | 0.0500 | 1.0000    | 3,218.3908 | 3,200.0000 | 3,218.3908 |

Now we examine information for a smaller assumed treatment difference
than the alternative:

``` r
h <- gs_info_binomial(p1 = .15, p2 = .12, xi1 = .5, delta = .05, n = c(350, 700, 1400))

gs_power_npe(
  theta = h$theta, theta1 = h$theta1, info = h$info,
  info0 = h$info0, info1 = h$info1,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfHSD, param = -2, total_spend = 0.2)
) |>
  gt() |>
  fmt_number(columns = 3:10, decimals = 4)
```

| analysis | bound | z       | probability | theta  | theta1 | info_frac | info       | info0      | info1      |
|----------|-------|---------|-------------|--------|--------|-----------|------------|------------|------------|
| 1        | upper | 4.3326  | 0.0002      | 0.0300 | 0.0500 | 0.2500    | 750.7508   | 749.3042   | 753.3362   |
| 2        | upper | 2.9632  | 0.0359      | 0.0300 | 0.0500 | 0.5000    | 1,501.5015 | 1,498.6084 | 1,506.6724 |
| 3        | upper | 1.9686  | 0.3644      | 0.0300 | 0.0500 | 1.0000    | 3,003.0030 | 2,997.2169 | 3,013.3448 |
| 1        | lower | −0.6751 | 0.0671      | 0.0300 | 0.0500 | 0.2500    | 750.7508   | 749.3042   | 753.3362   |
| 2        | lower | 0.2298  | 0.1945      | 0.0300 | 0.0500 | 0.5000    | 1,501.5015 | 1,498.6084 | 1,506.6724 |
| 3        | lower | 1.8514  | 0.5943      | 0.0300 | 0.0500 | 1.0000    | 3,003.0030 | 2,997.2169 | 3,013.3448 |

## References

Capture Investigators et al. 1997. “Randomised Placebo-Controlled Trial
of Abciximab Before and During Coronary Intervention in Refractory
Unstable Angina: The CAPTURE Study.” *The Lancet* 349 (9063): 1429–35.

Chan, Ivan SF. 2002. “Power and Sample Size Determination for
Noninferiority Trials Using an Exact Method.” *Journal of
Biopharmaceutical Statistics* 12 (4): 457–69.

Farrington, Conor P, and Godfrey Manning. 1990. “Test Statistics and
Sample Size Formulae for Comparative Binomial Trials with Null
Hypothesis of Non-Zero Risk Difference or Non-Unity Relative Risk.”
*Statistics in Medicine* 9 (12): 1447–54.

Gordon, Ian, and Ray Watson. 1996. “The Myth of Continuity-Corrected
Sample Size Formulae.” *Biometrics* 52 (1): 71–76.

Lachin, John M. 2009. *Biostatistical Methods: The Assessment of
Relative Risks*. John Wiley & Sons.
