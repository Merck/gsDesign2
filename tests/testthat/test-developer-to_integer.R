test_that("The IA nominal p-value is the same as the IA alpha spending.", {
  x <- gs_design_ahr(
    upper = gs_spending_bound,
    analysis_time = c(18, 30),
    upar = list(
      sf = gsDesign::sfLDOF,
      total_spend = 0.025,
      param = NULL,
      timing = c(18, 30) / 30
    ),
    lower = gs_b,
    lpar = c(-Inf, -Inf)
  ) |> to_integer()

  expect_equal(
    x$bound$`nominal p`[1],
    gsDesign::sfLDOF(alpha = 0.025, t = 18 / 30)$spend[1]
  )
})

test_that("The statistcial information under null equals to event/4 udner equal randomization.", {
  enroll_rate <- define_enroll_rate(duration = c(2, 2, 2, 6),
                                    rate = 1:4)
  fail_rate <- define_fail_rate(duration = Inf,
                                fail_rate = log(2) / 10,
                                hr = .7,
                                dropout_rate = 0.001)

  alpha <- 0.025
  beta <- 0.1
  ratio <- 1

  x <- gs_design_ahr(
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    ratio = ratio,
    beta = beta,
    alpha = alpha,
    # Information fraction at analyses and trial duration
    info_frac = c(0.6, 0.8, 1),
    analysis_time = 48,
    # Function and parameter(s) for upper spending bound
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = alpha, param = NULL),
    test_upper = c(FALSE, TRUE, TRUE),
    lower = gs_spending_bound,
    lpar = list(sf = gsDesign::sfHSD, total_spend = beta, param = -4) ,
    test_lower = c(TRUE, FALSE,FALSE),
    binding = FALSE
    ) |>
    to_integer()

  expect_true(all(x$analysis$info0 - x$analysis$event / 4 == 0))
})

test_that("Validate the sample size rounding under equal randomization.", {

  x <- gs_design_ahr(analysis_time = c(24, 36))

  y1 <- x |> to_integer(round_up_final = TRUE)
  y2 <- x |> to_integer(round_up_final = FALSE)

  expect_equal(ceiling(x$analysis$n[2] / 2) * 2, y1$analysis$n[2])
  expect_equal(round(x$analysis$n[2] / 2, 0) * 2, y2$analysis$n[2])
  expect_error(x |> to_integer(ratio = -2))
})

test_that("Validate the sample size rounding under unequal randomization (3:2).", {

  x <- gs_design_ahr(analysis_time = c(24, 36), ratio = 1.5)

  y1 <- x |> to_integer(round_up_final = TRUE)
  y2 <- x |> to_integer(round_up_final = TRUE, ratio = 5)
  y3 <- x |> to_integer(round_up_final = FALSE)
  y4 <- x |> to_integer(round_up_final = FALSE, ratio = 5)

  expect_equal(ceiling(x$analysis$n[2]), y1$analysis$n[2])
  expect_equal(ceiling(x$analysis$n[2] / 5) * 5, y2$analysis$n[2])
  expect_equal(round(x$analysis$n[2]), y3$analysis$n[2])
  expect_equal(round(x$analysis$n[2] / 2, 0) * 2, y4$analysis$n[2])
  expect_error(x |> to_integer(ratio = -2))
})
