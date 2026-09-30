fail_rate_strat <- rbind(
  define_fail_rate(
    duration = c(6, Inf), fail_rate = log(2) / c(8, 10),
    hr = c(1, .7), dropout_rate = .001, stratum = "A"
  ),
  define_fail_rate(
    duration = c(6, Inf), fail_rate = log(2) / c(20, 24),
    hr = c(1, .5), dropout_rate = .001, stratum = "B")
)
x_time <- c(0, 3, 6, 12, 24)
surv_a <- ppwe(x_time, c(6, Inf), log(2) / c(8, 10))
surv_b <- ppwe(x_time, c(6, Inf), log(2) / c(20, 24))
# ppwe() on the stratified failure rates, with the given stratum weights
ppwe_strat <- function(weights, d = fail_rate_strat, ...) with(d, ppwe(
  x_time, duration, fail_rate, stratum = stratum, weights = weights, ...
))

assert("ppwe() averages the per-stratum survival curves", {
  expected <- .3 * surv_a + .7 * surv_b
  (ppwe_strat(c(A = 3, B = 7)) %==% expected)
})

assert("ppwe() normalizes the stratum weights to sum to 1", {
  (ppwe_strat(c(A = 30, B = 70)) %==% ppwe_strat(c(A = .3, B = .7)))
})

assert("ppwe() matches named weights to strata regardless of their order", {
  (ppwe_strat(c(B = 7, A = 3)) %==% ppwe_strat(c(A = 3, B = 7)))
})

assert("ppwe() matches unnamed weights to the strata in order of appearance", {
  (ppwe_strat(c(3, 7)) %==% ppwe_strat(c(A = 3, B = 7)))
})

assert("ppwe() is unstratified when there is nothing to mix", {
  fail_rate_1 <- fail_rate_strat[fail_rate_strat$stratum == "A", ]
  # a single stratum, with and without weights
  (ppwe_strat(NULL, fail_rate_1) %==% surv_a)
  (ppwe_strat(c(A = 3), fail_rate_1) %==% surv_a)
  # no stratum at all
  (ppwe(x_time, fail_rate_1$duration, fail_rate_1$fail_rate) %==% surv_a)
  # all the weight on one stratum
  (ppwe_strat(c(A = 1, B = 0)) %==% surv_a)
  (ppwe_strat(c(A = 0, B = 1)) %==% surv_b)
})

assert("ppwe() returns the marginal CDF when lower_tail = TRUE", {
  weights <- c(A = 3, B = 7)
  (ppwe_strat(weights, lower_tail = TRUE) %==% (1 - ppwe_strat(weights)))
})

assert("the marginal ppwe() curve is a survival curve between its strata", {
  res <- ppwe_strat(c(A = 3, B = 7))
  (res[1] %==% 1)
  (diff(res) < 0)
  (res >= pmin(surv_a, surv_b))
  (res <= pmax(surv_a, surv_b))
})

assert("the marginal ppwe() curve is not that of the pooled failure rates", {
  # a mixture of exponentials is not exponential, so the marginal curve cannot
  # be recovered by averaging the failure rates
  res <- ppwe_strat(c(A = 5, B = 5))
  pooled <- ppwe(x_time, c(6, Inf), (log(2) / c(8, 10) + log(2) / c(20, 24)) / 2)
  (!isTRUE(all.equal(res, pooled)))
  # the mixture has the higher survival of the two (Jensen's inequality)
  (res[-1] > pooled[-1])
})

assert("ppwe() rejects invalid stratum weights", {
  (has_error(ppwe_strat(NULL), "must be provided"))
  (has_error(ppwe_strat(c(A = 1, C = 1)), "must match the strata"))
  (has_error(ppwe_strat(c(1, 2, 3)), "must be of length 2"))
  (has_error(ppwe_strat(c(A = -1, B = 2)), "must not be negative"))
  (has_error(ppwe_strat(c(A = 0, B = 0)), "must not be all zero"))
  (has_error(ppwe_strat(c(A = "1", B = "2")), "must be numeric"))
})

assert("ppwe() rejects a stratum of the wrong length", {
  (has_error(with(fail_rate_strat, ppwe(
    x_time, duration[-1], fail_rate[-1], stratum = stratum, weights = c(A = 1, B = 1)
  )), "same length as"))
})
