# Helper functions used by test-independent-gs_power_ahr.R

test_gs_power_ahr <- function() {
  x <- gsDesign::gsSurv(
    k = 2,
    test.type = 1,
    alpha = 0.025,
    beta = 0.2,
    astar = 0,
    timing = 0.7,
    sfu = gsDesign::sfLDOF,
    sfupar = c(0),
    sfl = gsDesign::sfLDOF,
    sflpar = c(0),
    lambdaC = log(2) / 9,
    hr = 0.65,
    hr0 = 1,
    eta = 0.001,
    gamma = c(6, 12, 18, 24),
    R = c(2, 2, 2, 6),
    S = NULL,
    T = NULL,
    minfup = NULL,
    ratio = 1
  )

  # Update x with gsDesign() to get integer event counts
  x <- gsDesign::gsDesign(
    k = x$k,
    test.type = 1,
    alpha = x$alpha,
    beta = x$beta,
    sfu = x$upper$sf,
    sfupar = x$upper$param,
    n.I = ceiling(x$n.I),
    maxn.IPlan = ceiling(x$n.I[x$k]),
    delta = x$delta,
    delta1 = x$delta1,
    delta0 = x$delta0
  )
  y <- gsDesign::gsBoundSummary(
    x,
    ratio = 1,
    digits = 4,
    ddigits = 2,
    tdigits = 1,
    timename = "Month",
    logdelta = TRUE
  )

  list("x" = x, "y" = y)
}
