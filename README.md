  <!-- badges: start -->
  [![CRAN status](https://www.r-pkg.org/badges/version/gsDesign2)](https://CRAN.R-project.org/package=gsDesign2)
  [![Codecov test coverage](https://codecov.io/gh/Merck/gsDesign2/branch/master/graph/badge.svg)](https://codecov.io/gh/Merck/gsDesign2?branch=main)
  [![R build status](https://github.com/Merck/gsDesign2/workflows/R-CMD-check/badge.svg)](https://github.com/Merck/gsDesign2/actions)
  <!-- badges: end -->
  
## Installation

You can install `gsDesign2` via CRAN:

```r
install.packages("gsDesign2")
```

Or, install from GitHub:

```r
remotes::install_github("Merck/gsDesign2")
```



## Overview

The gsDesign2 package supports recent innovations group sequential clinical trial design including non-proportional 
hazards and graphical multiplicity control with group sequential design.
Computations are based on piecewise constant enrollment and piecewise exponential failure rates.
Stratified populations are supported. 
Power and sample size calculations based on using testing based on the logrank test.
