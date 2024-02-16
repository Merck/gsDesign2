test_event <- function(enroll_rate, fail_rate, td = 15) {
  enroll_rate_1 <- enroll_rate
  enroll_rate_1$rate <- enroll_rate$rate / 2

  fail_rate_c <- fail_rate
  fail_rate_t <- fail_rate
  fail_rate_t$fail_rate <- fail_rate_t$fail_rate * fail_rate_t$hr

  event_c <- gsDesign2::expected_event(
    enroll_rate = enroll_rate_1,
    fail_rate = fail_rate_c,
    total_duration = td,
    simple = FALSE
  )

  event_t <- gsDesign2::expected_event(
    enroll_rate = enroll_rate_1,
    fail_rate = fail_rate_t,
    total_duration = td,
    simple = FALSE
  )

  total_e <- sum(event_c$event + event_t$event)

  total_e
}

gt_to_latex <- function(data) cat(as.character(gt::as_latex(data)))

with_seed <- function(seed, code) {
  code <- substitute(code)
  original_seed <- .Random.seed
  on.exit(.Random.seed <<- original_seed)
  set.seed(seed)
  eval.parent(code)
}
