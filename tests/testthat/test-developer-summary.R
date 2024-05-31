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
    "Analysis: 1 Time: 12 N: 707.3 Event: 160.4 AHR: 0.81 Information fraction: 0.42"
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
    "Analysis: 1 Time: 12 N: 707 Event: 160 AHR: 0.8108 Information fraction: 0.4191"
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
    "Analysis: 1 Information fraction: 0.4191 AHR: 0.8108 Event: 160 N: 707 Time: 12"
  )

  # Throw error if unnamed analysis_decimals does not match length of analysis_vars
  expect_error(
    summary(
      x,
      analysis_vars = c("info_frac", "ahr", "event", "n", "time"),
      analysis_decimals = c(4, 4),
    ),
    "summary: please input analysis_vars and analysis_decimals in pairs!"
  )
})

test_that("summary.gs_design() accepts a named vector for analysis_decimals", {
  x <- gs_design_ahr(analysis_time = c(12, 24))

  # Specify decimals
  observed <- x |>
    summary(analysis_decimals = c(ahr = 4, info_frac = 4)) |>
    attr("groups") |>
    extract_summary_analysis()
  expect_identical(
    observed,
    "Analysis: 1 Time: 12 N: 707.3 Event: 160.4 AHR: 0.8108 Information fraction: 0.4191"
  )

  # Specify decimals and also drop some variables
  observed <- x |>
    summary(
      analysis_vars = c("event", "ahr", "info_frac"),
      analysis_decimals = c(ahr = 4, info_frac = 4)
    ) |>
    attr("groups") |>
    extract_summary_analysis()
  expect_identical(
    observed,
    "Analysis: 1 Event: 160.4 AHR: 0.8108 Information fraction: 0.4191"
  )

  # Specify decimals and rearrange some variables
  observed <- x |>
    summary(
      analysis_vars = c("info_frac", "ahr", "event"),
      analysis_decimals = c(ahr = 4, info_frac = 4)
    ) |>
    attr("groups") |>
    extract_summary_analysis()
  expect_identical(
    observed,
    "Analysis: 1 Information fraction: 0.4191 AHR: 0.8108 Event: 160.4"
  )

  # Only drop variables
  observed <- x |>
    summary(
      analysis_vars = c("info_frac", "ahr", "event")
    ) |>
    attr("groups") |>
    extract_summary_analysis()
  expect_identical(
    observed,
    "Analysis: 1 Information fraction: 0.42 AHR: 0.81 Event: 160.4"
  )

  # Throw error is analysis_decimals is unnamed
  expect_error(
    summary(x, analysis_decimals = c(4, 4)),
    "summary: analysis_decimals must be a named vector if analysis_vars is not provided"
  )
})
