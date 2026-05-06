params_gs_design_combo <- test_gs_design_combo()

assert("calculate analysis number as planned", {
  res <- params_gs_design_combo
  fh_test <- res$fh_test
  gs_design_combo_test2 <- res$gs_design_combo_test2

  (all_equal(max(fh_test$analysis), max(gs_design_combo_test2$analysis$analysis)))
})

assert("calculate analysisTimes as planned", {
  res <- params_gs_design_combo
  fh_test <- res$fh_test
  gs_design_combo_test2 <- res$gs_design_combo_test2

  (all_equal(unique(fh_test$analysis_time), unique(gs_design_combo_test2$analysis$time)))
})

assert("calculate N and each analysis Events N as planned", {
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

    (all_equal(event * N / enrollsum, unique(gs_design_combo_test2$analysis$event)[i], tolerance = 0.01))
  }
})

assert("calculate probability under alternative", {
  res <- params_gs_design_combo
  beta <- res$beta
  gs_design_combo_test2 <- res$gs_design_combo_test2

  (all_equal(1 - beta, max((gs_design_combo_test2$bounds |> dplyr::filter(bound == "upper"))$probability), tolerance = 0.0001))
})

assert("calculate probability under null", {
  res <- params_gs_design_combo
  alpha <- res$alpha
  gs_design_combo_test2 <- res$gs_design_combo_test2

  (all_equal(alpha, max((gs_design_combo_test2$bounds |> dplyr::filter(bound == "upper"))$probability0), tolerance = 0.005))
})

assert("arguments are passed via ... to mvtnorm::pmvnorm()", {
  with_seed <- function(seed, code) {
    code <- substitute(code)
    original_seed <- .Random.seed
    on.exit(.Random.seed <<- original_seed)
    set.seed(seed)
    eval.parent(code)
  }

  x1 <- gs_design_combo(seed = 1)
  x2 <- gs_design_combo(seed = 1)
  x3 <- gs_design_combo(seed = 2)
  (x1 %==% x2)
  (!(identical(x1, x3)))
})
