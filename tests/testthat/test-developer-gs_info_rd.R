test_that("Testing weights calculation", {

  # The following function is almost the same as gs_info_rd.
  # The only difference is that:
  # gs_info_rd returns `ans`
  # gs_info_rd_ returns `tbl` with out removing columns
  # If there is a suggested way to avoid copy-pasting these similar functions, please let me know!
  gs_info_rd_ <- function(p_c = tibble::tibble(stratum = "All", rate = .2),
    p_e = tibble::tibble(stratum = "All", rate = .15),
    n = tibble::tibble(stratum = "All", n = c(100, 200, 300), analysis = 1:3),
    rd0 = 0, ratio = 1, weight = c("unstratified", "ss", "invar", "mr")) {

    n_analysis <- max(n$analysis)
    weight <- match.arg(weight)

    # Pool the input arguments together ----
    suppressMessages(
      tbl <- n |>
        left_join(p_c) |>
        rename(p_c = rate) |>
        left_join(p_e) |>
        rename(p_e = rate) |>
        left_join(if ("data.frame" %in% class(rd0)) {
          rd0
        } else {
          tibble(analysis = 1:n_analysis, rd0 = rd0)
        }) |>
        mutate(
          n_e = n / (1 + ratio),
          n_c = n * ratio / (1 + ratio),
          d = ifelse(p_c > p_e, 1, -1),
          p_pool_per_k_per_s = (n_c * p_c + n_e * p_e) / n,
          p_e0 = (p_c + ratio * p_e - d * rd0) / (ratio + 1),
          p_c0 = p_e0 + d * rd0
        )
    )

    # Calculate the variance of the risk difference ----
    if (is.numeric(rd0) && rd0 == 0) {
      tbl <- tbl |> mutate(
        sigma2_H0_per_k_per_s = p_pool_per_k_per_s * (1 - p_pool_per_k_per_s) * (1 / n_c + 1 / n_e),
        sigma2_H1_per_k_per_s = p_c * (1 - p_c) / n_c + p_e * (1 - p_e) / n_e
      )
    } else if ("data.frame" %in% class(rd0) || rd0 != 0) {
      tbl <- tbl |> mutate(
        sigma2_H0_per_k_per_s = p_c0 * (1 - p_c0) / n_c + p_e0 * (1 - p_e0) / n_e,
        sigma2_H1_per_k_per_s = p_c * (1 - p_c) / n_c + p_e * (1 - p_e) / n_e
      )
    }

    # Assign weights ----
    if (weight == "unstratified") {
      tbl <- tbl |> mutate(weight_per_k_per_s = 1)
    } else if (weight == "ss") {
      suppressMessages(
        tbl <- tbl |>
          left_join(
            tbl |>
              group_by(analysis) |>
              summarize(sum_ss = sum(n_c * n_e / (n_c + n_e)))
          ) |>
          mutate(weight_per_k_per_s = n_c * n_e / (n_c + n_e) / sum_ss) |>
          select(-sum_ss)
      )
    } else if (weight == "invar") {
      suppressMessages(
        tbl <- tbl |>
          left_join(
            tbl |>
              group_by(analysis) |>
              summarize(sum_inv_var_per_s = sum(1 / sigma2_H1_per_k_per_s))
          ) |>
          mutate(weight_per_k_per_s = 1 / sigma2_H1_per_k_per_s / sum_inv_var_per_s) |>
          select(-sum_inv_var_per_s)
      )
    } else if (weight == "mr") {
      suppressMessages(
        tbl <- tbl |>
          left_join(
            tbl |>
              group_by(analysis) |>
              summarize(sum_inv_var_per_s = sum(1 / sigma2_H1_per_k_per_s))
          ) |>
          ungroup() |>
          group_by(analysis) |>
          mutate(alpha_per_k_per_s = (p_e - p_c) * sum_inv_var_per_s - sum((p_e - p_c) / sigma2_H1_per_k_per_s),
                 beta_per_k_per_s = 1/sigma2_H1_per_k_per_s * (1 + alpha_per_k_per_s * sum((p_e - p_c) * n / sum(n))),
                 weight_per_k_per_s = beta_per_k_per_s / sum_inv_var_per_s -
                                        (alpha_per_k_per_s / sigma2_H1_per_k_per_s / (sum_inv_var_per_s + sum(alpha_per_k_per_s * (p_e - p_c) / sigma2_H1_per_k_per_s))) *
                                        (sum((p_e - p_c) * beta_per_k_per_s) / sum_inv_var_per_s)
          ) # |>
          # select(-c(sum_inv_var_per_s, alpha_per_k_per_s, beta_per_k_per_s))
      )
    }

    return(tbl)
  }

  # This example following the second example in the paper "Minimum risk weights for comparing treatments in stratified binomial trials"
  p_c <- data.frame(stratum = c("Stratum1", "Stratum2"), rate = c(0.48, 0.8))
  p_e <- data.frame(stratum = c("Stratum1", "Stratum2"), rate = c(0.53, 0.95))
  n <- data.frame(stratum = c("Stratum1", "Stratum2"), n = c(63, 37), analysis = 1)

  # Testing the INVAR weight
  weight_invar <- gs_info_rd_(p_c = p_c, p_e = p_e, n = n, rd0 = 0, ratio = 1, weight = "invar")$weight_per_k_per_s
  expect_equal(weight_invar, c(0.41, 0.59), tolerance = 1e-2)

  # Testing the SS weight
  weight_ss <- gs_info_rd_(p_c = p_c, p_e = p_e, n = n, rd0 = 0, ratio = 1, weight = "ss")$weight_per_k_per_s
  expect_equal(weight_ss, c(0.63, 0.37), tolerance = 1e-2)

  # Testing the MR weight following formula (10)
  x_mr <- gs_info_rd_(p_c = p_c, p_e = p_e, n = n, rd0 = 0, ratio = 1, weight = "mr")
  V1 <- x_mr$sigma2_H1_per_k_per_s[1]
  V2 <- x_mr$sigma2_H1_per_k_per_s[2]
  delta1 <- x_mr$p_e[1] - x_mr$p_c[1]
  delta2 <- x_mr$p_e[2] - x_mr$p_c[2]
  f1 <- x_mr$n[1] / sum(x_mr$n)
  f2 <- x_mr$n[2] / sum(x_mr$n)

  w1 <- (V2+(delta1-delta2)^2*f1) / (V1 + V2 + (delta1 - delta2)^2)
  w2 <- 1 - w1
  expect_equal(gs_info_rd_(p_c = p_c, p_e = p_e, n = n, rd0 = 0, ratio = 1, weight = "mr")$weight_per_k_per_s,
               c(w1, w2))

  # Note that if is risk difference is constant across strata,then alpha_per_k_per_s is zero
  expect_equal(gs_info_rd_(p_c = data.frame(stratum = c("Stratum1", "Stratum2"), rate = c(0.4, 0.8)),
                           p_e = data.frame(stratum = c("Stratum1", "Stratum2"), rate = c(0.5, 0.9)),
                           n = data.frame(stratum = c("Stratum1", "Stratum2"), n = c(50, 50), analysis = 1),
                           rd0 = 0, ratio = 1, weight = "mr")$alpha_per_k_per_s,
               c(0, 0))


})

