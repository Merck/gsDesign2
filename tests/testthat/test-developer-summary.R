# See helper functions in helper-developer-summary.R

# Maintain previous behavior
test_that("summary.gs_design() accepts same-length vectors for analysis_vars and analysis_decimals", {
  x <- gs_design_ahr(analysis_time = c(12, 24))

  # default decimals
  observed <- x |>
    summary() |>
    attr("groups") |>
    extract_summary_analysis()
  expect_identical(
    observed,
    paste0("Analysis: 1 Time: 12 N: 707.3 Events: 160.4 AHR: 0.81 Information fraction: ", round(x$analysis$event[1]/ max(x$analysis$event), 2))
  )

  # specify the decimals for each variable
  observed <- x |>
    summary(
      analysis_vars = c("time", "n", "event", "ahr", "info_frac"),
      analysis_decimals = c(2, 0, 0, 4, 4)
    ) |>
    attr("groups") |>
    extract_summary_analysis()
  expect_identical(
    observed,
    "Analysis: 1 Time: 12 N: 707 Events: 160 AHR: 0.8108 Information fraction: 0.4191"
  )

  # Drop variables and also specify the decimals
  observed <- x |>
    summary(
      analysis_vars = c("ahr", "info_frac"),
      analysis_decimals = c(4, 4)
    ) |>
    attr("groups") |>
    extract_summary_analysis()
  expect_identical(
    observed,
    "Analysis: 1 AHR: 0.8108 Information fraction: 0.4191"
  )

  # Rearrange variables
  observed <- x |>
    summary(
      analysis_vars = c("info_frac", "ahr", "event", "n", "time"),
      analysis_decimals = c(4, 4, 0, 0, 2)
    ) |>
    attr("groups") |>
    extract_summary_analysis()
  expect_identical(
    observed,
    "Analysis: 1 Information fraction: 0.4191 AHR: 0.8108 Events: 160 N: 707 Time: 12"
  )

  # Throw error if unnamed analysis_decimals does not match length of analysis_vars
  expect_error(
    summary(
      x,
      analysis_vars = c("info_frac", "ahr", "event", "n", "time"),
      analysis_decimals = c(4, 4),
    ),
    "'analysis_vars' and 'analysis_decimals' must be of the same length"
  )
})

test_that("summary.gs_design() accepts a named vector for analysis_decimals", {
  x <- gs_design_ahr(analysis_time = c(12, 24))

  # Specify decimals
  observed <- x |>
    summary(analysis_decimals = c(ahr = 4, info_frac0 = 4)) |>
    attr("groups") |>
    extract_summary_analysis()
  expect_identical(
    observed,
    paste0("Analysis: 1 Time: 12 N: 707.3 Events: 160.4 AHR: 0.8108 Information fraction: ", round(x$analysis$event[1]/ max(x$analysis$event), 4))
  )

  # Specify decimals and also drop some variables
  observed <- x |>
    summary(
      analysis_vars = c("event", "ahr", "info_frac0"),
      analysis_decimals = c(ahr = 4, info_frac0 = 4)
    ) |>
    attr("groups") |>
    extract_summary_analysis()
  expect_identical(
    observed,
    paste0("Analysis: 1 Events: 160.4 AHR: 0.8108 Information fraction: ", round(x$analysis$event[1]/max(x$analysis$event), 4))
  )

  # Specify decimals and rearrange some variables
  observed <- x |>
    summary(
      analysis_vars = c("info_frac0", "ahr", "event"),
      analysis_decimals = c(ahr = 4, info_frac0 = 4)
    ) |>
    attr("groups") |>
    extract_summary_analysis()
  expect_identical(
    observed,
    paste0("Analysis: 1 Information fraction: ", round(x$analysis$event[1]/max(x$analysis$event), 4), " AHR: 0.8108 Events: 160.4")
  )

  # Only drop variables
  observed <- x |>
    summary(
      analysis_vars = c("info_frac0", "ahr", "event")
    ) |>
    attr("groups") |>
    extract_summary_analysis()
  expect_identical(
    observed,
    "Analysis: 1 Information fraction: 0.42 AHR: 0.81 Events: 160.4"
  )

  # Throw error is analysis_decimals is unnamed
  expect_error(
    summary(x, analysis_decimals = c(4, 4)),
    "'analysis_decimals' must be a named vector if 'analysis_vars' is not provided"
  )
})

test_that("The column 'Bound' is always included in summary.gs_design() output", {
  x <- gs_design_ahr()

  # without col_vars
  observed <- summary(x)
  expect_true("Bound" %in% colnames(observed))

  # including "bound" in col_vars
  observed <- summary(
    x,
    col_vars = c(
      "bound", "z", "~hr at bound", "nominal p", "Alternate hypothesis", "Null hypothesis"
    ),
    col_decimals = c(NA, 4, 4, 4, 4, 4)
  )
  expect_true("Bound" %in% colnames(observed))

  # excluding "bound" in col_vars
  observed <- summary(
    x,
    col_vars = c(
      "z", "~hr at bound", "nominal p", "Alternate hypothesis", "Null hypothesis"
    ),
    col_decimals = c(4, 4, 4, 4, 4)
  )
  expect_true("Bound" %in% colnames(observed))
})

test_that("The full alpha is correctly carried over", {
  a_level <- 0.02
  x <- gs_power_ahr(
    upper = gs_spending_bound,
    upar = list(
      sf = gsDesign::sfLDOF,
      total_spend = a_level
    ),
    test_lower = FALSE
  )

  # without col_vars
  observed <- summary(x)

  expect_equal(attributes(observed)$full_alpha, a_level)
})

# Maintain previous behavior
test_that("summary.gs_design() accepts same-length vectors for col_vars and col_decimals", {
  x <- gs_design_ahr()

  # default decimals
  x_sum <- summary(x)
  observed <- as.data.frame(x_sum)[, -1:-2]
  expected <- data.frame(
    Z = 1.96,
    `~HR at bound` = 0.795,
    `Nominal p` = 0.025,
    `Alternate hypothesis` = 0.9,
    `Null hypothesis` = 0.025,
    check.names = FALSE
  )
  expect_equal(observed, expected)

  # specify the decimals for each variable
  x_sum <- summary(
    x,
    col_vars = c("z", "~hr at bound", "nominal p", "Alternate hypothesis", "Null hypothesis"),
    col_decimals = c(0, 0, 0, 0, 0)
  )
  observed <- as.data.frame(x_sum)[, -1:-2]
  expected <- data.frame(
    Z = 2,
    `~HR at bound` = 1,
    `Nominal p` = 0,
    `Alternate hypothesis` = 1,
    `Null hypothesis` = 0,
    check.names = FALSE
  )
  expect_equal(observed, expected)

  # Drop variables and also specify the decimals
  x_sum <- summary(
    x,
    col_vars = c("nominal p", "Null hypothesis"),
    col_decimals = c(0, 0)
  )
  observed <- as.data.frame(x_sum)[, -1:-2]
  expected <- data.frame(`Nominal p` = 0, `Null hypothesis` = 0, check.names = FALSE)
  expect_equal(observed, expected)

  # Rearrange variables
  x_sum <- summary(
    x,
    col_vars = c("Null hypothesis", "Alternate hypothesis", "nominal p", "~hr at bound", "z"),
    col_decimals = c(0, 0, 0, 0, 0)
  )
  observed <- as.data.frame(x_sum)[, -1:-2]
  expected <- data.frame(
    `Null hypothesis` = 0,
    `Alternate hypothesis` = 1,
    `Nominal p` = 0,
    `~HR at bound` = 1,
    Z = 2,
    check.names = FALSE
  )
  expect_equal(observed, expected)

  # Throw error if unnamed col_decimals does not match length of col_vars
  expect_error(
    summary(
      x,
      col_vars = c("Null hypothesis", "Alternate hypothesis", "nominal p"),
      col_decimals = c(0, 0),
    ),
    "'col_vars' and 'col_decimals' must be of the same length"
  )
})

test_that("summary.gs_design() accepts a named vector for col_decimals", {
  x <- gs_design_ahr()

  # Specify decimals
  x_sum <- summary(x, col_decimals = c(z = 0, `nominal p` = 0))
  observed <- as.data.frame(x_sum)[, -1:-2]
  expected <- data.frame(
    Z = 2,
    `~HR at bound` = 0.795,
    `Nominal p` = 0,
    `Alternate hypothesis` = 0.9,
    `Null hypothesis` = 0.025,
    check.names = FALSE
  )
  expect_equal(observed, expected)

  # Specify decimals and also drop some variables
  x_sum <- summary(
    x,
    col_vars = c("z", "nominal p", "Null hypothesis"),
    col_decimals = c(z = 0, `nominal p` = 0)
  )
  observed <- as.data.frame(x_sum)[, -1:-2]
  expected <- data.frame(Z = 2, `Nominal p` = 0, `Null hypothesis` = 0.025, check.names = FALSE)
  expect_equal(observed, expected)

  # Specify decimals and rearrange some variables
  x_sum <- summary(
    x,
    col_vars = c("Null hypothesis", "nominal p", "z"),
    col_decimals = c(z = 0, `nominal p` = 0)
  )
  observed <- as.data.frame(x_sum)[, -1:-2]
  expected <- data.frame(`Null hypothesis` = 0.025, `Nominal p` = 0, Z = 2, check.names = FALSE)
  expect_equal(observed, expected)

  # Only drop variables
  x_sum <- summary(x, col_vars = c("z", "nominal p", "Null hypothesis"))
  observed <- as.data.frame(x_sum)[, -1:-2]
  expected <- data.frame(Z = 1.96, `Nominal p` = 0.025, `Null hypothesis` = 0.025, check.names = FALSE)
  expect_equal(observed, expected)

  # Throw error is col_decimals is unnamed
  expect_error(
    summary(x, col_decimals = c(4, 4)),
    "'col_decimals' must be a named vector if 'col_vars' is not provided"
  )
})
