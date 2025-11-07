# Numerical integration non-proportional effect size in group sequential design

``` r
library(tibble)
library(knitr)
library(gsDesign2)
```

### Overview

We have provided asymptotic distribution theory and notation for group
sequential boundaries in `vignettes/articles/story-npe-background.Rmd`.

This vignettes generalize computational algorithms provided in Chapter
19 of Jennison and Turnbull (1999) that are used to compute boundary
crossing probabilities as well as derive boundaries for group sequential
designs.

### Asymptotic normal and boundary crossing probabilities

We assume \\Z_1,\cdots,Z_K\\ has a multivariate normal distribution with
variance for \\1\leq k\leq K\\ of \\\text{Var}(Z_k) = 1\\ and the
expected value is

\\E(Z\_{k})= \sqrt{\mathcal{I}\_k}\theta(t\_{k})= \sqrt{n_k}E(\bar X_k)
.\\

### Notation for boundary crossing probabilities

We use a shorthand notation in this section to have \\\theta\\ represent
\\\theta()\\ and \\\theta=0\\ to represent \\\theta(t)\equiv 0\\ for all
\\t\\. We denote the probability of crossing the upper boundary at
analysis \\k\\ without previously crossing a bound by

\\\alpha\_{k}(\theta)=P\_{\theta}(\\Z\_{k}\geq
b\_{k}\\\cap\_{j=1}^{i-1}\\a\_{j}\leq Z\_{j}\< b\_{j}\\),\\
\\k=1,2,\ldots,K.\\

Next, we consider analogous notation for the lower bound. For
\\k=1,2,\ldots,K\\ denote the probability of crossing a lower bound at
analysis \\k\\ without previously crossing any bound by

\\\beta\_{k}(\theta)=P\_{\theta}((Z\_{k}\< a\_{k}\\\cap\_{j=1}^{k-1}\\
a\_{j}\leq Z\_{j}\< b\_{j}\\).\\ For symmetric testing for analysis
\\k\\ we would have \\a_k= - b_k\\, \\\beta_k(0)=\alpha_k(0),\\
\\k=1,2,\ldots,K\\. The total lower boundary crossing probability for a
trial is denoted by
\\\beta(\theta)\equiv\sum\_{k=1}^{K}\beta\_{k}(\theta).\\ Note that we
can also set \\a_k= -\infty\\ for any or all analyses if a lower bound
is not desired, \\k=1,2,\ldots,K\\; thus, we will not use the
\\\alpha^+(\theta)\\ notation here. For \\k\<K\\, we can set
\\b_k=\infty\\ where an upper bound is not desired. Obviously, for each
\\k\\, we want either \\a_k\>-\infty\\ or \\b_k\<\infty\\.

### Recursive algorithms for numerical integration

We now provide a small update to the algorithm of Chapter 19 of Jennison
and Turnbull (1999) to do the numerical integration required to compute
the boundary crossing probabilities of the previous section and also
identifying group sequential boundaries satisfying desired
characteristics. The key to these calculations is the conditional power
identity in equation (1) above which allows building recursive numerical
integration identities to enable simple, efficient numerical
integration.

We define

\\g_1(z;\theta) = \frac{d}{dz}P(Z_1\leq z) = \phi\left(z -
\sqrt{\mathcal{I}\_1}\theta(t_1)\right)\tag{2}\\

and for \\k=2,3,\ldots K\\ we recursively define the subdensity function

\\\begin{align} g_k(z; \theta) &= \frac{d}{dz}P\_\theta(\\Z_k\leq
z\\\cap\_{j=1}^{k-1}\\a_j\leq Z_j\<b_j\\) \\
&=\int\_{a\_{k-1}}^{b\_{k-1}}\frac{d}{dz}P\_\theta(\\Z_k\leq z
\|Z\_{k-1}=z\_{k-1}\\)g\_{k-1}(z\_{k-1}; \theta)dz\_{k-1}\\
&=\int\_{a\_{k-1}}^{b\_{k-1}}f_k(z\_{k-1},z;\theta)g\_{k-1}(z\_{k-1};
\theta)dz\_{k-1}.\tag{3} \end{align}\\

The bottom line notation here is the same as on p. 347 in Jennison and
Turnbull (1999). However, \\f_k()\\ here takes a slightly different
form.

\\\begin{align} f_k(z\_{k-1},z;\theta) &=\frac{d}{dz}P\_\theta(\\Z_k\leq
z \|Z\_{k-1}=z\_{k-1}\\)\\ &=\frac{d}{dz}P\_\theta(B_k - B\_{k-1} \leq
z\sqrt{t_k}-z\_{k-1}\sqrt{t\_{k-1}})\\
&=\frac{d}{dz}\Phi\left(\frac{z\sqrt{t_k}-z\_{k-1}\sqrt{t\_{k-1}}-\sqrt{\mathcal{I}\_K}(t_k\theta(t_k)-
t\_{k-1}\theta(t\_{k-1}))}{\sqrt{t_k-t\_{k-1}}}\right)\\
&=\frac{\sqrt{t_k}}{\sqrt{t_k-t\_{k-1}}}\phi\left(\frac{z\sqrt{t_k}-z\_{k-1}\sqrt{t\_{k-1}}-\sqrt{\mathcal{I}\_K}(t_k\theta(t_k)-
t\_{k-1}\theta(t\_{k-1}))}{\sqrt{t_k-t\_{k-1}}}\right)\\
&=\frac{\sqrt{\mathcal{I}\_k}}{\sqrt{\mathcal{I}\_k-\mathcal{I}\_{k-1}}}\phi\left(\frac{z\sqrt{\mathcal{I}\_k}-z\_{k-1}\sqrt{\mathcal{I}\_{k-1}}-(\mathcal{I}\_k\theta(t_k)-
\mathcal{I}\_{k-1}\theta(t\_{k-1}))}{\sqrt{\mathcal{I}\_k-\mathcal{I}\_{k-1}}}\right).\tag{3}
\end{align}\\

We have worked towards this last line due to its comparability to
equation (19.4) on p. 347 of Jennison and Turnbull (1999) which assumes
\\\theta(t_k)=\theta\\ for some constant \\\theta\\; we re-write that
equation slightly here as:

\\f_k(z\_{k-1},z;\theta) =
\frac{\sqrt{\mathcal{I}\_k}}{\sqrt{\mathcal{I}\_k-\mathcal{I}\_{k-1}}}\phi\left(\frac{z\sqrt{\mathcal{I}\_k}-z\_{k-1}\sqrt{\mathcal{I}\_{k-1}}-\theta(\mathcal{I}\_k-
\mathcal{I}\_{k-1})}{\sqrt{\mathcal{I}\_k-\mathcal{I}\_{k-1}}}\right).\tag{4}\\
This is really the only difference in the computational algorithm for
boundary crossing probabilities from the Jennison and Turnbull (1999)
algorithm. Using the above recursive approach we can compute for
\\k=1,2,\ldots,K\\

\\\alpha\_{k}(\theta)=\int\_{b_k}^\infty g_k(z;\theta)dz\tag{5}\\ and
\\\beta\_{k}(\theta)=\int\_{-\infty}^{a_k} g_k(z;\theta)dz.\tag{6}\\

### Deriving spending boundaries

We can now derive boundaries satisfying given boundary crossing
probabilities using equations (2-6) above. Suppose for we have specified
\\b_1,\ldots,b\_{k-1}\\ and \\a_1,\ldots,a\_{k-1}\\ and now wish to
derive \\a_k\\ and \\b_k\\ such that equations (5) and (6) hold. We
write the upper bound as a function of the probability of crossing we
wish to derive.

\\\pi_k(b;\theta) = \int_b^\infty g_k(z;\theta)dz\\
\\\pi_k^\prime(b;\theta) =\frac{d}{db}\pi_k(b;\theta)= -g_k(b;
\theta).\tag{7}\\ If we have a value \\\pi_k(b^{(i)};\theta)\\ we can
use a first order Taylor’s series expansion to approximate

\\\pi_k(b;\theta)\approx
\pi_k(b^{(i)};\theta)+(b-b^{(i)})\pi_k^\prime(b^{(i)};\theta)\\ We set
\\b^{(i+1)}\\ such that

\\\alpha_k(\theta)=\pi_k(b^{(i)}; \theta) +
(b^{(i+1)}-b^{(i)})\pi^\prime(b^{(i)};\theta).\\

Solving for \\b^{(i+1)}\\ we have

\\b^{(i+i)} = b^{(i)} + \frac{\alpha_k(\theta) -
\pi_k(b^{(i)};\theta)}{\pi_k^\prime(b^{(i)}; \theta)}= b^{(i)} -
\frac{\alpha_k(\theta) - \pi_k(b^{(i)};\theta)}{g_k(b^{(i)};
\theta)}\tag{8}\\ and iterate until \\\|b^{(i+1)}-b^{(i)}\|\<\epsilon\\
for some tolerance level \\\epsilon\>0\\ and
\\\pi_k(b^{(i+1)};\theta)-\alpha_k(\theta)\\ is suitably small. A simple
starting value for any \\k\\ is

\\b^{(0)} = \Phi^{-1}(1- \alpha_k(\theta)) +
\sqrt{\mathcal{I}\_k}\theta(t_k).\tag{9}\\ Normally, \\b_k\\ will be
calculated with \\\theta(t_k)=0\\ for \\k=1,2,\ldots,K\\ which
simplifies the above. However, \\a_k\\ computed analogously will often
use a non-zero \\\theta\\ to enable so-called \\\beta\\-spending.

### Numerical integration

The numerical integration required to compute boundary probabilities and
derive boundaries is the same as that defined in section 19.3 of
Jennison and Turnbull (1999). The single change is the replacement of
the non-proportional effect size assumption of equation (3) above
replacing the equivalent of equation (4) used for a constant effect size
as in Jennison and Turnbull (1999).

#### Demonstrating calculations

We walk through how to perform the basic calculations above. The basic
scenario will have one interim analysis in addition to the final
analysis. We will target Type I error \\\alpha=0.025\\ and Type II error
\\\beta = 0.1\\, the latter corresponding to a target of 90% power. We
will assume a power spending function with \\\rho=2\\ for both bounds.
That is, for information fraction \\t\\, the cumulative spending will be
\\\alpha \times t^2\\ for the upper bound and \\\beta \times t^2\\ for
the lower bound. Statistical information will be 1 for the first
analysis and 4 for the final analysis, leading to information fraction
\\t_1= 1/4, t_2=1\\ for the interim and final, respectively. We assume
\\\theta_1 = .5\\, \\\theta_3=1.5\\.

- Set up overall study parameters

``` r
# Information for both null and alternative
info <- c(1, 4)
# information fraction
timing <- info / max(info)
# Type I error
alpha <- 0.025
# Type II error (1 - power)
beta <- 0.1
# Cumulative alpha-spending at IA, Final
alphaspend <- alpha * timing^2
# Cumulative beta-spending at IA, Final
betaspend <- beta * timing^2
# Average treatment effect at analyses
theta <- c(1, 3) / 2
```

- Calculate interim bounds

``` r
# Upper bound under null hypothesis
b1 <- qnorm(alphaspend[1], lower.tail = FALSE)
# Lower bound under alternate hypothesis
a1 <- qnorm(betaspend[1], mean = sqrt(info[1]) * theta[1])
# Compare probability of crossing vs target for bounds:
cat(
  "Upper bound =", b1, "Target spend =", alphaspend[1],
  "Actual spend =", pnorm(b1, lower.tail = FALSE)
)
#> Upper bound = 2.955167 Target spend = 0.0015625 Actual spend = 0.0015625
```

``` r
# Lower bound under alternate hypothesis
a1 <- qnorm(betaspend[1], mean = sqrt(info[1]) * theta[1])
# Compare probability of crossing vs target for bounds:
cat(
  "Lower bound =", a1, "Target spend =", betaspend[1],
  "Actual spend =", pnorm(a1, mean = sqrt(info[1]) * theta[1])
)
#> Lower bound = -1.997705 Target spend = 0.00625 Actual spend = 0.00625
```

- Set up numerical integration grid for next (final) analysis

We set up a table for numerical integration over the continuation region
which we can subsequently use to compute boundary crossing probabilities
for bounds at the second interim analysis. We begin with the null
hypothesis. The columns in the resulting table are - `z` - \\Z\\-values
for the grid; recall that each interim test statistic is normally
distributed with variance 1 - `w` - weights for numerical integration -
`h` - weights `w` times the normal density that can be used for
numerical integration; we will demonstrate use below

``` r
# Set up grid over continuation region
# Null hypothesis
grid1_0 <- gsDesign2:::h1(theta = 0, info = info[1], a = a1, b = b1)
grid1_0 |> head()
#> $z
#>   [1] -1.99770547 -1.95718607 -1.91666667 -1.87500000 -1.83333333 -1.79166667
#>   [7] -1.75000000 -1.70833333 -1.66666667 -1.62500000 -1.58333333 -1.54166667
#>  [13] -1.50000000 -1.45833333 -1.41666667 -1.37500000 -1.33333333 -1.29166667
#>  [19] -1.25000000 -1.20833333 -1.16666667 -1.12500000 -1.08333333 -1.04166667
#>  [25] -1.00000000 -0.95833333 -0.91666667 -0.87500000 -0.83333333 -0.79166667
#>  [31] -0.75000000 -0.70833333 -0.66666667 -0.62500000 -0.58333333 -0.54166667
#>  [37] -0.50000000 -0.45833333 -0.41666667 -0.37500000 -0.33333333 -0.29166667
#>  [43] -0.25000000 -0.20833333 -0.16666667 -0.12500000 -0.08333333 -0.04166667
#>  [49]  0.00000000  0.04166667  0.08333333  0.12500000  0.16666667  0.20833333
#>  [55]  0.25000000  0.29166667  0.33333333  0.37500000  0.41666667  0.45833333
#>  [61]  0.50000000  0.54166667  0.58333333  0.62500000  0.66666667  0.70833333
#>  [67]  0.75000000  0.79166667  0.83333333  0.87500000  0.91666667  0.95833333
#>  [73]  1.00000000  1.04166667  1.08333333  1.12500000  1.16666667  1.20833333
#>  [79]  1.25000000  1.29166667  1.33333333  1.37500000  1.41666667  1.45833333
#>  [85]  1.50000000  1.54166667  1.58333333  1.62500000  1.66666667  1.70833333
#>  [91]  1.75000000  1.79166667  1.83333333  1.87500000  1.91666667  1.95833333
#>  [97]  2.00000000  2.04166667  2.08333333  2.12500000  2.16666667  2.20833333
#> [103]  2.25000000  2.29166667  2.33333333  2.37500000  2.41666667  2.45833333
#> [109]  2.50000000  2.54166667  2.58333333  2.62500000  2.66666667  2.70833333
#> [115]  2.75000000  2.79166667  2.83333333  2.87500000  2.91666667  2.93591676
#> [121]  2.95516685
#> 
#> $w
#>   [1] 0.013506468 0.054025872 0.027395357 0.055555556 0.027777778 0.055555556
#>   [7] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#>  [13] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#>  [19] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#>  [25] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#>  [31] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#>  [37] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#>  [43] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#>  [49] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#>  [55] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#>  [61] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#>  [67] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#>  [73] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#>  [79] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#>  [85] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#>  [91] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#>  [97] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#> [103] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#> [109] 0.027777778 0.055555556 0.027777778 0.055555556 0.027777778 0.055555556
#> [115] 0.027777778 0.055555556 0.027777778 0.055555556 0.020305586 0.025666787
#> [121] 0.006416697
#> 
#> $h
#>   [1] 7.325795e-04 3.174772e-03 1.741296e-03 3.821460e-03 2.064199e-03
#>   [6] 4.452253e-03 2.396592e-03 5.151272e-03 2.763254e-03 5.918793e-03
#>  [11] 3.163964e-03 6.753608e-03 3.597711e-03 7.652841e-03 4.062610e-03
#>  [16] 8.611793e-03 4.555835e-03 9.623842e-03 5.073586e-03 1.068040e-02
#>  [21] 5.611075e-03 1.177092e-02 6.162560e-03 1.288302e-02 6.721409e-03
#>  [26] 1.400261e-02 7.280204e-03 1.511417e-02 7.830885e-03 1.620106e-02
#>  [31] 8.364929e-03 1.724594e-02 8.873556e-03 1.823116e-02 9.347967e-03
#>  [36] 1.913930e-02 9.779592e-03 1.995361e-02 1.016034e-02 2.065862e-02
#>  [41] 1.048287e-02 2.124051e-02 1.074078e-02 2.168766e-02 1.092888e-02
#>  [46] 2.199098e-02 1.104332e-02 2.214423e-02 1.108173e-02 2.214423e-02
#>  [51] 1.104332e-02 2.199098e-02 1.092888e-02 2.168766e-02 1.074078e-02
#>  [56] 2.124051e-02 1.048287e-02 2.065862e-02 1.016034e-02 1.995361e-02
#>  [61] 9.779592e-03 1.913930e-02 9.347967e-03 1.823116e-02 8.873556e-03
#>  [66] 1.724594e-02 8.364929e-03 1.620106e-02 7.830885e-03 1.511417e-02
#>  [71] 7.280204e-03 1.400261e-02 6.721409e-03 1.288302e-02 6.162560e-03
#>  [76] 1.177092e-02 5.611075e-03 1.068040e-02 5.073586e-03 9.623842e-03
#>  [81] 4.555835e-03 8.611793e-03 4.062610e-03 7.652841e-03 3.597711e-03
#>  [86] 6.753608e-03 3.163964e-03 5.918793e-03 2.763254e-03 5.151272e-03
#>  [91] 2.396592e-03 4.452253e-03 2.064199e-03 3.821460e-03 1.765603e-03
#>  [96] 3.257338e-03 1.499749e-03 2.757277e-03 1.265110e-03 2.317833e-03
#> [101] 1.059795e-03 1.934941e-03 8.816570e-04 1.604123e-03 7.283858e-04
#> [106] 1.320661e-03 5.975956e-04 1.079765e-03 4.868972e-04 8.767006e-04
#> [111] 3.939593e-04 7.068990e-04 3.165552e-04 5.660405e-04 2.525990e-04
#> [116] 4.501131e-04 2.001694e-04 3.554511e-04 1.151506e-04 1.375807e-04
#> [121] 3.249917e-05
```

The probability of not crossing a bound under the null hypothesis is
computed as follows:

``` r
prob_h0_continue <- sum(grid1_0$h)
cat(
  "Probability of continuing trial under null hypothesis\n",
  " Using numerical integration:", prob_h0_continue,
  "\n  Using normal CDF:", pnorm(b1) - pnorm(a1), "\n"
)
#> Probability of continuing trial under null hypothesis
#>   Using numerical integration: 0.9755632 
#>   Using normal CDF: 0.9755632
```

We now set up numerical integration grid under the alternate hypothesis
and the compute continuation probability.

``` r
grid1_1 <- gsDesign2:::h1(theta = theta[1], info = info[1], a = a1, b = b1)
prob_h1_continue <- sum(grid1_1$h)
h1mean <- sqrt(info[1]) * theta[1]
cat(
  "Probability of continuing trial under alternate hypothesis\n",
  " Using numerical integration:", prob_h1_continue,
  "\n  Using normal CDF:", pnorm(b1, mean = h1mean) - pnorm(a1, h1mean), "\n"
)
#> Probability of continuing trial under alternate hypothesis
#>   Using numerical integration: 0.986709 
#>   Using normal CDF: 0.986709
```

- Compute initial iteration for analysis 2 bounds

The initial estimate of the second analysis bounds are computed the same
way as the actual first analysis bounds.

``` r
# Upper bound under null hypothesis
# incremental spend
spend0 <- alphaspend[2] - alphaspend[1]
# H0 bound at 2nd analysis; 1st approximation
b2_0 <- qnorm(spend0, lower.tail = FALSE)
# Lower bound under alternate hypothesis
spend1 <- betaspend[2] - betaspend[1]
a2_0 <- qnorm(spend1, mean = sqrt(info[2]) * theta[2])
cat("Initial bound approximation for 2nd analysis\n (",
  a2_0, ", ", b2_0, ")\n",
  sep = ""
)
#> Initial bound approximation for 2nd analysis
#>  (1.681989, 1.987428)
```

- Compute actual boundary crossing probabilities with initial
  approximations

To get actual boundary crossing probabilities at the second analysis, we
update our numerical integration grids. Under the null hypothesis, we
need to update to the interval above `b2_0`.

``` r
# Upper rejection region grid under H0
grid2_0 <- gsDesign2:::hupdate(theta = 0, info = info[2], a = b2_0, b = Inf, im1 = info[1], gm1 = grid1_0)
pupper_0 <- sum(grid2_0$h)
cat(
  "Upper spending at analysis 2\n Target:", spend0, "\n Using initial bound approximation:",
  pupper_0, "\n"
)
#> Upper spending at analysis 2
#>  Target: 0.0234375 
#>  Using initial bound approximation: 0.02290683
```

To get a first order Taylor’s series approximation to update this bound,
we need the derivative of the above probability with respect to the
Z-value cutoff. This was given above as the subdensity computed in the
grid. As before, the grid contains the numerical integration weight in
`w` and that weight times the subdensity in `h`. Thus, to get the
subdensity at the bound, which is the estimated derivative in the
boundary crossing probability, we compute:

``` r
# First point in grid is at bound
# Compute derivative
dpdb2 <- grid2_0$h[1] / grid2_0$w[1]
# Compute difference between target and actual bound crossing probability
pdiff <- spend0 - pupper_0
# Taylor's series update
b2_1 <- b2_0 - pdiff / dpdb2
# Compute boundary crossing probability at updated bound
cat(
  "Original bound approximation:", b2_0,
  "\nUpdated bound approximation:", b2_1
)
#> Original bound approximation: 1.987428 
#> Updated bound approximation: 1.977726
grid2_0 <- gsDesign2:::hupdate(theta = 0, info = info[2], a = b2_1, b = Inf, im1 = info[1], gm1 = grid1_0)
pupper_1 <- sum(grid2_0$h)
cat(
  "\nOriginal boundary crossing probability:", pupper_0,
  "\nUpdated boundary crossing probability:", pupper_1,
  "\nTarget:", spend0, "\n"
)
#> 
#> Original boundary crossing probability: 0.02290683 
#> Updated boundary crossing probability: 0.02344269 
#> Target: 0.0234375
```

We see that the Taylor’s series update has gotten us substantially
closer to the targeted boundary probability. We now update the lower
bound in an analogous fashion.

``` r
# Lower rejection region grid under H1
grid2_1 <- gsDesign2:::hupdate(
  theta = theta[2], info = info[2], a = -Inf, b = a2_0,
  thetam1 = theta[1], im1 = info[1], gm1 = grid1_1
)
plower_0 <- sum(grid2_1$h)
# Last point in grid is at bound
# Compute derivative
indx <- length(grid2_1$h)
dpda2 <- grid2_1$h[indx] / grid2_1$w[indx]
# Compute difference between target and actual bound crossing probability
pdiff <- spend1 - plower_0
# Taylor's series update
a2_1 <- a2_0 + pdiff / dpda2
# Compute boundary crossing probability at updated bound
cat(
  "Original bound approximation:", a2_0,
  "\nUpdated bound approximation:", a2_1
)
#> Original bound approximation: 1.681989 
#> Updated bound approximation: 1.702596

grid2_1 <- gsDesign2:::hupdate(
  theta = theta[2], info = info[2], a = -Inf, b = a2_1,
  thetam1 = theta[1], im1 = info[1], gm1 = grid1_1
)
plower_1 <- sum(grid2_1$h)
cat(
  "\nOriginal boundary crossing probability:", plower_0,
  "\nUpdated boundary crossing probability:", plower_1,
  "\nTarget:", spend1, "\n"
)
#> 
#> Original boundary crossing probability: 0.09035972 
#> Updated boundary crossing probability: 0.09379707 
#> Target: 0.09375
```

- Confirm with
  [`gs_power_npe()`](https://merck.github.io/gsDesign2/reference/gs_power_design_npe.md)

``` r
gs_power_npe(
  theta = theta, theta1 = theta, info = info, binding = TRUE,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfPower, total_spend = 0.025, param = 2),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfPower, total_spend = 0.1, param = 2)
)
#> # A tibble: 4 × 10
#>   analysis bound     z probability theta theta1 info_frac  info info0 info1
#>      <int> <chr> <dbl>       <dbl> <dbl>  <dbl>     <dbl> <dbl> <dbl> <dbl>
#> 1        1 upper  2.96     0.00704   0.5    0.5      0.25     1     1     1
#> 2        2 upper  1.98     0.845     1.5    1.5      1        4     4     4
#> 3        1 lower -2.00     0.00625   0.5    0.5      0.25     1     1     1
#> 4        2 lower  1.70     0.100     1.5    1.5      1        4     4     4
```

## References

Jennison, Christopher, and Bruce W Turnbull. 1999. *Group Sequential
Methods with Applications to Clinical Trials*. Chapman & Hall/CRC.
