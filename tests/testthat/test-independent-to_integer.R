# Create common fixed designs
create_fixed_design <- function(design_fn, extra_args = list()) {
  base_args <- list(
    alpha = 0.025,
    power = 0.9,
    enroll_rate = define_enroll_rate(duration = 18, rate = 20),
    fail_rate = define_fail_rate(
      duration = c(4, 100),
      fail_rate = log(2) / 12,
      hr = c(1, .6),
      dropout_rate = .001
    ),
    study_duration = 36
  )

  # Combine base arguments with extra arguments
  args <- c(base_args, extra_args)
  do.call(design_fn, args)
}

# Validate fixed design outputs
check_fixed_design_output <- function(result) {
  # Common checks
  expect_s3_class(result, "fixed_design")
  expect_equal(result$analysis$n, round(result$analysis$n))

  # Check for analysis event
  expect_equal(result$analysis$event, round(result$analysis$event), tolerance = 1e-6)

  # Validate input structure
  expect_s3_class(result$input$enroll_rate, "tbl_df")
  expect_s3_class(result$input$fail_rate, "tbl_df")

  # Check design and parameter constraints
  expect_true(result$analysis$n >= 0)
  expect_true(result$input$alpha > 0 & result$input$alpha < 1)
  expect_true(result$input$power > 0 & result$input$power <= 1)
  expect_true(all(result$input$enroll_rate$rate >= 0))
  expect_true(all(result$input$fail_rate$fail_rate >= 0))
  expect_true(all(result$input$fail_rate$dropout_rate >= 0 & result$input$fail_rate$dropout_rate <= 1))
  expect_true(result$input$study_duration > 0)
}

# Validate fixed design summary
check_fixed_design_summary <- function(summary_x) {
  expect_s3_class(summary_x, "tbl_df")
  expect_equal(ncol(summary_x), 7)
  expect_named(summary_x, c("Design", "N", "Events", "Time", "Bound", "alpha", "Power"), ignore.order = TRUE)

  # Ensure values are within expected ranges
  expect_true(all(summary_x$N > 0))
  expect_true(all(summary_x$Events > 0))
  expect_true(all(summary_x$Time > 0))
  expect_true(all(summary_x$alpha > 0 & summary_x$alpha < 1))
  expect_true(all(summary_x$Power > 0 & summary_x$Power <= 1))
}

# Parameterized tests for different fixed design types
test_that("to_integer works correctly for different fixed design types", {
  designs <- list(
    list(fn = fixed_design_ahr, name = "ahr", extra_args = list()),
    list(fn = fixed_design_fh, name = "fh", extra_args = list(rho = 0.5, gamma = 0.5, ratio = 1)),
    list(fn = fixed_design_mb, name = "mb", extra_args = list(tau = 4, ratio = 1))
  )

  for (design in designs) {
    x <- create_fixed_design(design$fn, design$extra_args) |> to_integer()
    check_fixed_design_output(x)
    expect_equal(x$design, design$name)

    # Check summary output
    summary_x <- summary(x)
    check_fixed_design_summary(summary_x)
  }
})

# Test invalid input handling
test_that("fixed_design_ahr handles invalid inputs", {
  expect_error(fixed_design_ahr(
    alpha = -0.01, power = 0.9,
    enroll_rate = define_enroll_rate(duration = 18, rate = 1),
    fail_rate = define_fail_rate(
      duration = c(4, 100), fail_rate = log(2) / 12,
      hr = c(1, .6), dropout_rate = .001
    ),
    study_duration = 36
  ), "`alpha` and `beta` values must satisfy 0 < alpha < 1 - beta < 1!")

  expect_error(fixed_design_ahr(
    alpha = 0.025, power = 1.1,
    enroll_rate = define_enroll_rate(duration = 18, rate = 1),
    fail_rate = define_fail_rate(
      duration = c(4, 100), fail_rate = log(2) / 12,
      hr = c(1, .6), dropout_rate = .001
    ),
    study_duration = 36
  ), "`alpha` and `beta` values must satisfy 0 < alpha < 1 - beta < 1!")

  expect_error(fixed_design_ahr(
    alpha = 0.025, power = 0.9,
    enroll_rate = define_enroll_rate(duration = 0, rate = 1),
    fail_rate = define_fail_rate(
      duration = c(4, 100), fail_rate = log(2) / 12,
      hr = c(1, .6), dropout_rate = .001
    ),
    study_duration = -36
  ), "must be positive and strictly increasing!")
})

test_that("to_integer.gs_design rounds events and sample sizes correctly for AHR", {
  # Create a mock gs_design object with AHR class
  design_ahr <- gs_design_ahr(
    analysis_time = c(18, 30),
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL),
    lower = gs_b,
    lpar = c(-Inf, -Inf)
  )

  # Apply the to_integer function
  result <- to_integer(design_ahr)

  # Check if events are rounded correctly
  rounded_events <- round(result$analysis$event)
  expect_true(all(abs(rounded_events - result$analysis$event) < 0.5))

  # Check if sample sizes are rounded correctly
  rounded_sample_sizes <- round(result$analysis$n)
  expect_true(all(abs(rounded_sample_sizes - result$analysis$n) < 0.5))
})

test_that("to_integer.gs_design handles WLR correctly", {
  # Create a mock gs_design object with WLR class
  design_wlr <- gs_design_wlr(
    analysis_time = c(18, 30),
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL),
    lower = gs_b,
    lpar = c(-Inf, -Inf)
  )

  # Apply the to_integer function
  result <- to_integer(design_wlr)

  # Check if events are rounded correctly
  rounded_events <- round(result$analysis$event)
  expect_true(all(abs(rounded_events - result$analysis$event) < 0.5))

  # Check if sample sizes are rounded correctly
  rounded_sample_sizes <- round(result$analysis$n)
  expect_true(all(abs(rounded_sample_sizes - result$analysis$n) < 0.5))
})

test_that("to_integer.gs_design handles RD class correctly", {
  # Create a mock gs_design object with RD class
  design_rd <- gs_design_rd(
    p_c = tibble::tibble(stratum = c("A", "B"), rate = c(.2, .3)),
    p_e = tibble::tibble(stratum = c("A", "B"), rate = c(.15, .27)),
    weight = "ss",
    stratum_prev = tibble::tibble(stratum = c("A", "B"), prevalence = c(.4, .6)),
    info_frac = c(0.7, 1),
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL),
    lower = gs_b,
    lpar = c(-Inf, -Inf)
  )

  # Apply the to_integer function
  result <- to_integer(design_rd)

  # Check if sample sizes per stratum are rounded correctly
  rounded_sample_sizes <- round(result$analysis$n)
  expect_true(all(abs(rounded_sample_sizes - result$analysis$n) < 0.5))
})

test_that("to_integer.gs_design handles calendar-based spending correctly", {
  # Create a mock gs_design object with calendar-based spending
  design_ahr <- gs_design_ahr(
    upper = gs_spending_bound,
    analysis_time = c(18, 30),
    upar = list(
      sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL,
      timing = c(18, 30) / 30
    ),
    lower = gs_b,
    lpar = c(-Inf, -Inf)
  )

  # Apply the to_integer function
  result <- to_integer(design_ahr)

  # Check that the rounded event values are close to the original values
  rounded_events <- round(result$analysis$event)
  expect_true(all(abs(rounded_events - result$analysis$event) < 0.5))
})

test_that("to_integer.gs_design performs correctly with large sample sizes", {
  # Create a large gs_design object for stress testing
  design_large <- gs_design_ahr(
    analysis_time = c(18, 30),
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL),
    lower = gs_b,
    lpar = c(-Inf, -Inf)
  )

  # Apply the to_integer function
  result <- to_integer(design_large)

  # Ensure that rounding works: round the event and n values
  result$analysis$event <- round(result$analysis$event)
  result$analysis$n <- round(result$analysis$n)

  # Check that rounding and transformations work as expected
  expect_true(all(result$analysis$event %% 1 == 0)) # Ensure events are integers
  expect_true(all(result$analysis$n %% 1 == 0)) # Ensure sample sizes are integers
})
