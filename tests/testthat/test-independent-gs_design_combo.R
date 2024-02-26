params_gs_design_combo <- test_gs_design_combo()

test_that("calculate analysis number as planned", {
  res <- params_gs_design_combo
  fh_test <- res$fh_test
  gs_design_combo_test2 <- res$gs_design_combo_test2

  expect_equal(max(fh_test$analysis), max(gs_design_combo_test2$analysis$analysis))
})

test_that("calculate analysisTimes as planned", {
  res <- params_gs_design_combo
  fh_test <- res$fh_test
  gs_design_combo_test2 <- res$gs_design_combo_test2

  expect_equal(unique(fh_test$analysis_time), unique(gs_design_combo_test2$analysis$time))
})

test_that("calculate N and each analysis Events N as planned", {
  res <- params_gs_design_combo
  fh_test <- res$fh_test
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  gs_design_combo_test2 <- res$gs_design_combo_test2

  for (i in 1:max(fh_test$analysis)) {
    event <- test_event(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      td = unique(fh_test$analysis_time)[i]
    )
    enrollsum <- enroll_rate$duration * enroll_rate$rate
    N <- max(gs_design_combo_test2$analysis$n)

    expect_equal(
      event * N / enrollsum,
      unique(gs_design_combo_test2$analysis$event)[i],
      tolerance = 0.01
    )
  }
})

test_that("calculate probability under alternative", {
  res <- params_gs_design_combo
  beta <- res$beta
  gs_design_combo_test2 <- res$gs_design_combo_test2

  expect_equal(
    1 - beta,
    max((gs_design_combo_test2$bounds %>% dplyr::filter(bound == "upper"))$probability),
    tolerance = 0.0001
  )
})

test_that("calculate probability under null", {
  res <- params_gs_design_combo
  alpha <- res$alpha
  gs_design_combo_test2 <- res$gs_design_combo_test2

  expect_equal(
    alpha,
    max((gs_design_combo_test2$bounds %>% dplyr::filter(bound == "upper"))$probability0),
    tolerance = 0.005
  )
})

test_that("arguments are passed via ... to mvtnorm::pmvnorm()", {
  x1 <- gs_design_combo(seed = 1)
  x2 <- gs_design_combo(seed = 1)
  x3 <- gs_design_combo(seed = 2)
  expect_identical(x1, x2)
  expect_false(identical(x1, x3))
})
