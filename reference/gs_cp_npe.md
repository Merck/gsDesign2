# Conditional power computation with non-constant effect size

Conditional power computation with non-constant effect size

## Usage

``` r
gs_cp_npe(theta = NULL, info = NULL, a = NULL, b = NULL)
```

## Arguments

- theta:

  A vector of length two, which specifies the natural parameter for
  treatment effect. The first element of `theta` is the treatment effect
  of an interim analysis i. The second element of `theta` is the
  treatment effect of a future analysis j.

- info:

  A vector of two, which specifies the statistical information under the
  treatment effect `theta`.

- a:

  Interim z-value at analysis i (scalar).

- b:

  Future target z-value at analysis j (scalar).

## Value

A scalar with the conditional power \\P(Z_2\>b\mid Z_1=a)\\.

## Details

We assume \\Z_1\\ and \\Z_2\\ are the z-values at an interim analysis
and later analysis, respectively. We assume further \\Z_1\\ and \\Z_2\\
are bivariate normal with standard group sequential assumptions on
independent increments where for \\i=1,2\\ \$\$E(Z_i) =
\theta_i\sqrt{I_i}\$\$ \$\$Var(Z_i) = 1/I_i\$\$ \$\$Cov(Z_1, Z_2) = t
\equiv I_1/I_2\$\$ where \\\theta_1, \theta_2\\ are real values and
\\0\<I_1\<I_2\\. See
https://merck.github.io/gsDesign2/articles/story-npe-background.html for
assumption details. Returned value is \$\$P(Z_2 \> b \mid Z_1 = a) = 1 -
\Phi\left(\frac{b - \sqrt{t}a - \sqrt{I_2}(\theta_2 -
\theta_1\sqrt{t})}{\sqrt{1 - t}}\right)\$\$

## Examples

``` r
library(gsDesign2)

# Calculate conditional power under arbitrary theta and info
# In practice, the value of theta and info commonly comes from a design.
# More examples are available at the pkgdown vignettes.
gs_cp_npe(theta = c(.1, .2),
          info = c(15, 35),
          a = 1.5, b = 1.96)
#> [1] 0.4745007
```
