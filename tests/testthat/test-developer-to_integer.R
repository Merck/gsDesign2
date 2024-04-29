test_that("The IA nominal p-value is the same as the IA alpha spending.", {
  x <- gs_design_ahr(
    upper = gs_spending_bound,
    analysis_time = c(18, 30),
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL,
                timing = c(18, 30) / 30),
    lower = gs_b,
    lpar = c(-Inf, -Inf)) |> to_integer()

  expect_equal(x$bound$`nominal p`[1], gsDesign::sfLDOF(alpha = 0.025, t = 18/30)$spend[1])
})
