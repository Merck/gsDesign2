test_that("fixed_design_ahr function works correctly", {
  # Creating the fixed design object
  x <- fixed_design_ahr(
    alpha = calculate_alpha(), power = calculate_beta(),
    enroll_rate = enroll_rate(),
    fail_rate = fail_rate(),
    study_duration = calculate_study_duration()
  )

  # Testing to_integer method
  integer_output <- x %>% to_integer()

  expect_true(is.list(integer_output))

  # Testing input components
  expect_equal(integer_output$input$alpha, calculate_alpha())
  expect_equal(integer_output$input$power, calculate_beta())
  expect_equal(integer_output$input$ratio, calculate_ratio())
  expect_equal(integer_output$input$study_duration, calculate_study_duration())

  # Testing enroll_rate components
  expect_s3_class(integer_output$input$enroll_rate, "tbl_df")

  # Testing fail_rate components
  expect_s3_class(integer_output$input$fail_rate, "tbl_df")

  # Testing design attribute
  expect_equal(attributes(integer_output)$class, c("fixed_design", "list"))
  expect_equal(integer_output$design, "ahr")
})

test_that("Error message is returned for an invalid object", {
  x <- fixed_design_ahr(
    alpha = calculate_alpha(), power = calculate_beta(),
    enroll_rate = enroll_rate(),
    fail_rate = fail_rate(),
    study_duration = calculate_study_duration()
  )
  x %>% to_integer()
  expect_message(to_integer.gs_design(x), "The input object is not applicable to get an integer sample size.")
})

test_that("Error message is returned for an invalid object", {
  x <- fixed_design_fh(
    alpha = calculate_alpha(), power = calculate_beta(),
    enroll_rate = enroll_rate(),
    fail_rate = fail_rate(),
    rho = 0.5, gamma = 0.5,
    study_duration = calculate_study_duration(), ratio = calculate_ratio()
  )
  x %>% to_integer()
  expect_message(to_integer.gs_design(x), "The input object is not applicable to get an integer sample size.")
})

test_that("Error message is returned for an invalid object", {
  x <- fixed_design_mb(
    alpha = calculate_alpha(),
    power = calculate_beta(),
    enroll_rate = define_enroll_rate(),
    fail_rate = fail_rate(),
    tau = 4,
    study_duration = calculate_study_duration(), ratio = calculate_ratio()
  )
  x %>% to_integer()

  # Expect a warning message with the correct content
  expect_message(to_integer(x), "The input object is not applicable to get an integer sample size.")
})
