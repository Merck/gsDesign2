params_gs_power_combo <- test_gs_power_combo()

test_that("calculate analysis number as planned", {
  res <- params_gs_power_combo
  fh_test <- res$fh_test
  gs_power_combo_test1 <- res$gs_power_combo_test1

  expect_equal(max(fh_test$analysis), max(gs_power_combo_test1$analysis$analysis))
})

test_that("calculate analysisTimes as planned", {
  res <- params_gs_power_combo
  fh_test <- res$fh_test
  gs_power_combo_test1 <- res$gs_power_combo_test1

  expect_equal(unique(fh_test$analysis_time), unique(gs_power_combo_test1$analysis$time))
})

test_that("calculate N and each analysis Events N as planned", {
  res <- params_gs_power_combo
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  fh_test <- res$fh_test
  gs_power_combo_test1 <- res$gs_power_combo_test1

  for (i in 1:max(fh_test$analysis)) {
    event <- test_event(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      td = unique(fh_test$analysis_time)[i]
    )
    expect_equal(event, unique(gs_power_combo_test1$analysis$event)[i], tolerance = 0.01)
  }
})

test_that("calculate analysis number as planned", {
  res <- params_gs_power_combo
  fh_test <- res$fh_test
  gs_power_combo_test2 <- res$gs_power_combo_test2

  expect_equal(max(fh_test$analysis), max(gs_power_combo_test2$analysis$analysis))
})

test_that("calculate analysisTimes as planned", {
  res <- params_gs_power_combo
  fh_test <- res$fh_test
  gs_power_combo_test2 <- res$gs_power_combo_test2

  expect_equal(unique(fh_test$analysis_time), unique(gs_power_combo_test2$analysis$time))
})

test_that("calculate N and each analysis Events N as planned", {
  res <- params_gs_power_combo
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  fh_test <- res$fh_test
  gs_power_combo_test2 <- res$gs_power_combo_test2

  for (i in 1:max(fh_test$analysis)) {
    event <- test_event(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      td = unique(fh_test$analysis_time)[i]
    )
    expect_equal(event, unique(gs_power_combo_test2$analysis$event)[i], tolerance = 0.01)
  }
})

test_that("arguments are passed via ... to mvtnorm::pmvnorm()", {
  x1 <- gs_power_combo(seed = 1)
  x2 <- gs_power_combo(seed = 1)
  x3 <- gs_power_combo(seed = 2)
  expect_identical(x1, x2)
  expect_false(identical(x1, x3))
})
