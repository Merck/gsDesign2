test_that("expected events is different from gsDesign::eEvents and expected_event", {
  enroll_rate <- define_enroll_rate(duration = c(2, 1, 2), rate = c(5, 10, 20))
  fail_rate <- define_fail_rate(duration = c(1, 1, 1), fail_rate = c(.05, .02, .01), hr = 1, dropout_rate = .01)
  total_duration <- 20

  expect_equal(
    expected_event(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      total_duration = total_duration,
      simple = TRUE
    ),
    gsDesign::eEvents(
      lambda = fail_rate$fail_rate,
      S = fail_rate$duration[1:(nrow(fail_rate) - 1)],
      eta = fail_rate$dropout_rate,
      gamma = enroll_rate$rate,
      R = enroll_rate$duration,
      T = total_duration
    )$d,
    ignore_attr = TRUE
  )
})

test_that("data frame returned from expected_event not as expected", {
  # Test case from gsSurvNPH
  enroll_rate <- define_enroll_rate(duration = c(1, 1, 8), rate = c(3, 2, 0))
  fail_rate <- define_fail_rate(duration = c(4, Inf), fail_rate = c(.03, .06), dropout_rate = c(.001, .002), hr = 1)
  total_duration <- 7

  xx <- expected_event(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    total_duration = total_duration,
    simple = FALSE
  ) %>%
    data.frame()
  # Expected checked with alternate calculations in gsSurvNPH vignette
  expected <- data.frame(
    t = c(0, 4),
    fail_rate = c(0.03, 0.06),
    event = c(0.5642911, 0.5194821)
  )
  expect_equal(xx, expected)
})

# Double programming tests

# Test 1: with multiple fail rates, short FU
test_that("expected events is different from double-programmed vs. expected_event, with mutiple failrates, short FU", {
  res <- params_expected_event()
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  failRates <- res$failRates
  total_duration <- res$total_duration
  simple <- res$simple

  expect_equal(
    test_expected_event(
      enrollRates = enroll_rate,
      failRates = failRates,
      totalDuration = total_duration
    ),
    expected_event(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      total_duration = total_duration,
      simple = simple
    )
  )
})

# Test 2: with multiple fail rates, long FU
test_that("expected events is different from double-programmed vs. expected_event, with mutiple failrates, long FU", {
  res <- params_expected_event()
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  failRates <- res$failRates
  total_duration <- 80
  simple <- res$simple

  expect_equal(
    test_expected_event(
      enrollRates = enroll_rate,
      failRates = failRates,
      totalDuration = total_duration
    ),
    expected_event(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      total_duration = total_duration,
      simple = simple
    )
  )
})

# Test 3: with multiple fail rates and with multiple enrollment duration
test_that("expected events is different from double-programmed vs. expected_event, with mutiple enrollment duration", {
  enroll_rate <- define_enroll_rate(
    duration = c(50, 10),
    rate = c(10, 5)
  )

  fail_rate <- define_fail_rate(
    duration = c(10, 20, 10),
    fail_rate = log(2) / c(5, 10, 5),
    dropout_rate = c(0.1, 0.2, 0),
    hr = 1
  )

  fail_rate$failRate <- fail_rate$fail_rate
  fail_rate$dropoutRate <- fail_rate$dropout_rate
  failRates <- fail_rate

  total_duration <- 80
  simple <- TRUE

  expect_equal(
    test_expected_event(
      enrollRates = enroll_rate,
      failRates = failRates,
      totalDuration = total_duration
    ),
    expected_event(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      total_duration = total_duration,
      simple = simple
    )
  )
})
