test_that("fixed_design_ahr function works correctly", {
  # Creating the fixed design object
  x <- fixed_design_ahr(
    alpha = calculate_alpha(), power = calculate_beta(),
    enroll_rate = enroll_rate(),
    fail_rate = fail_rate(),
    study_duration = calculate_study_duration()
  ) %>% to_integer()

  # Test that to_integer converts sample size to integer for fixed_design_ahr
  expect_equal(x$analysis$n, round(x$analysis$n))

  # Testing enroll_rate components
  expect_s3_class(x$input$enroll_rate, "tbl_df")

  # Testing fail_rate components
  expect_s3_class(x$input$fail_rate, "tbl_df")

  # Testing design attribute
  expect_equal(attributes(x)$class, c("fixed_design", "list"))
  expect_equal(x$design, "ahr")
  # Check that the sample size (N) is non-negative integer
  expect_true(x$analysis$n >= 0)

  # Verify that alpha and power are within the correct range
  expect_true(x$input$alpha > 0 & x$input$alpha < 1)
  expect_true(x$input$power > 0 & x$input$power <= 1)

  # Ensure enroll_rate and fail_rate tables have non-negative values
  expect_true(all(x$input$enroll_rate$rate >= 0))
  expect_true(all(x$input$fail_rate$fail_rate >= 0))

  # Check that dropout_rate is between 0 and 1
  expect_true(all(x$input$fail_rate$dropout_rate >= 0 & x$input$fail_rate$dropout_rate <= 1))

  # Check if summary output is in the correct format and has expected names
  summary_x <- summary(x)
  expect_s3_class(summary_x, "tbl_df")
  expect_equal(ncol(summary_x), 7)
  expect_named(summary_x, c("Design", "N", "Events", "Time", "Bound", "alpha", "Power"))
})

test_that("fixed_design_fh function works correctly", {
  # Creating the fixed design object
  x <- fixed_design_fh(
    alpha = calculate_alpha(), power = calculate_beta(),
    enroll_rate = enroll_rate(),
    fail_rate = fail_rate(),
    rho = 0.5, gamma = 0.5,
    study_duration = calculate_study_duration(), ratio = calculate_ratio()
  ) %>% to_integer()

  # Test that to_integer converts sample size to integer for fixed_design_fh
  expect_equal(x$analysis$n, round(x$analysis$n))

  # Testing enroll_rate components
  expect_s3_class(x$input$enroll_rate, "tbl_df")

  # Testing fail_rate components
  expect_s3_class(x$input$fail_rate, "tbl_df")

  # Testing design attribute
  expect_equal(attributes(x)$class, c("fixed_design", "list"))
  expect_equal(x$design, "fh")
})

test_that("fixed_design_mb function works correctly", {
  # Creating the fixed design object
  x <- fixed_design_mb(
    alpha = calculate_alpha(), power = calculate_beta(),
    enroll_rate = enroll_rate(),
    fail_rate = fail_rate(),
    tau = 4,
    study_duration = calculate_study_duration(), ratio = calculate_ratio()
  ) %>% to_integer()

  # Test that to_integer converts sample size to integer for fixed_design_mb
  expect_equal(x$analysis$n, round(x$analysis$n))

  # Testing enroll_rate components
  expect_s3_class(x$input$enroll_rate, "tbl_df")

  # Testing fail_rate components
  expect_s3_class(x$input$fail_rate, "tbl_df")

  # Testing design attribute
  expect_equal(attributes(x)$class, c("fixed_design", "list"))
  expect_equal(x$design, "mb")

  # Check that the sample size (N) and events are non-negative integers
  expect_true(x$analysis$n >= 0)
  expect_equal(x$analysis$event, round(x$analysis$event), tolerance = 1e-6)

  # Validate that alpha and power values are within the correct range
  expect_true(x$input$alpha > 0 & x$input$alpha < 1)
  expect_true(x$input$power > 0 & x$input$power <= 1)

  # Ensure that enroll_rate and fail_rate have non-negative values
  expect_true(all(x$input$enroll_rate$rate >= 0))
  expect_true(all(x$input$fail_rate$fail_rate >= 0))

  # Check that dropout_rate is between 0 and 1
  expect_true(all(x$input$fail_rate$dropout_rate >= 0 & x$input$fail_rate$dropout_rate <= 1))

  # Check if the design attribute matches the correct format
  expect_match(x$design, "mb")

  # Test that the summary output has the correct format and values
  summary_x <- summary(x)
  expect_s3_class(summary_x, "tbl_df")
  expect_equal(ncol(summary_x), 7)
  expect_named(summary_x, c("Design", "N", "Events", "Time", "Bound", "alpha", "Power"))

  # Ensure that summary values are within expected ranges
  expect_true(summary_x$N > 0)
  expect_true(summary_x$Events > 0)
  expect_true(summary_x$Time > 0)
  expect_true(summary_x$alpha > 0 & summary_x$alpha < 1)
  expect_true(summary_x$Power > 0 & summary_x$Power <= 1)

  # Ensure tau is correctly reflected in the Design column of the summary
  expect_match(summary_x$Design, "Modestly weighted LR: tau = 4")
})



test_that("fixed_design_ahr handles invalid inputs", {
  # Pass an invalid alpha value
  expect_error(fixed_design_ahr(
    alpha = -0.01, power = 0.9,
    enroll_rate = define_enroll_rate(duration = 18, rate = 1),
    fail_rate = define_fail_rate(
      duration = c(4, 100),
      fail_rate = log(2) / 12, hr = c(1, .6),
      dropout_rate = .001
    ),
    study_duration = 36
  ), "must have 0 < alpha < 1 - beta < 1!")

})

test_that("to_integer with fixed_design_ahr returns correct results", {
  x <- fixed_design_ahr(
    alpha = 0.025, power = 0.9,
    enroll_rate = define_enroll_rate(duration = 18, rate = 1),
    fail_rate = define_fail_rate(
      duration = c(4, 100),
      fail_rate = log(2) / 12, hr = c(1, .6),
      dropout_rate = .001
    ),
    study_duration = 36
  )

  result <- to_integer(x)

  # Check the class of the result
  expect_s3_class(result, "fixed_design")

  # Check if sample size and event are integers
  expect_equal(result$analysis$n, round(result$analysis$n))
  expect_equal(result$analysis$event, round(result$analysis$event), tolerance = 1e-6)
})

test_that("to_integer with fixed_design_fh returns correct results", {
  x <- fixed_design_fh(
    alpha = 0.025, power = 0.9,
    enroll_rate = define_enroll_rate(duration = 18, rate = 20),
    fail_rate = define_fail_rate(
      duration = c(4, 100),
      fail_rate = log(2) / 12,
      hr = c(1, .6),
      dropout_rate = .001
    ),
    rho = 0.5, gamma = 0.5,
    study_duration = 36, ratio = 1
  )

  result <- to_integer(x)

  # Check the class of the result
  expect_s3_class(result, "fixed_design")

  # Check if sample size and event are integers
  expect_equal(result$analysis$n, round(result$analysis$n))
  expect_equal(result$analysis$event, round(result$analysis$event),tolerance = 1e-6)
  # Validate if the design attribute is correctly set
  expect_equal(attributes(result)$class, c("fixed_design", "list"))
  expect_equal(result$design, "fh")

  # Ensure the study duration is positive and within an acceptable range
  expect_true(result$input$study_duration > 0)

  # Check that the alpha and power values are within expected limits
  expect_true(result$input$alpha > 0 & result$input$alpha < 1)
  expect_true(result$input$power > 0 & result$input$power <= 1)

  # Verify that rho and gamma parameters are within the correct range (0, 1)
  expect_true(result$input$rho >= 0 & result$input$rho <= 1)
  expect_true(result$input$gamma >= 0 & result$input$gamma <= 1)

  # Ensure enroll_rate and fail_rate tables have valid non-negative values
  expect_true(all(result$input$enroll_rate$rate >= 0))
  expect_true(all(result$input$fail_rate$fail_rate >= 0))

  # Validate that dropout_rate is between 0 and 1
  expect_true(all(result$input$fail_rate$dropout_rate >= 0 & result$input$fail_rate$dropout_rate <= 1))

  # Test if the summary output is in the correct format and has expected names
  summary_result <- summary(result)
  expect_s3_class(summary_result, "tbl_df")
  expect_equal(ncol(summary_result), 7)
  expect_named(summary_result, c("Design", "N", "Events", "Time", "Bound", "alpha", "Power"))

  # Ensure that the output summary values are within acceptable ranges
  expect_true(summary_result$N > 0)
  expect_true(summary_result$Events > 0)
  expect_true(summary_result$Time > 0)
  expect_true(summary_result$alpha > 0 & summary_result$alpha < 1)
  expect_true(summary_result$Power > 0 & summary_result$Power <= 1)
})

