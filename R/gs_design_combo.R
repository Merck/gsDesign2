#  Copyright (c) 2025 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
#  All rights reserved.
#
#  This file is part of the gsDesign2 program.
#
#  gsDesign2 is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Group sequential design using MaxCombo test under non-proportional hazards
#'
#' @inheritParams gs_design_ahr
#' @inheritParams mvtnorm::pmvnorm
#' @param fh_test A data frame to summarize the test in each analysis.
#'   See examples for its data structure.
#' @param n_upper_bound A numeric value of upper limit of sample size.
#' @param ... Additional parameters passed to [mvtnorm::pmvnorm].
#'
#' @return A list with input parameters, enrollment rate, analysis, and bound.
#'
#' @export
#'
#' @examples
#' # The example is slow to run
#' library(mvtnorm)
#' library(gsDesign)
#'
#' enroll_rate <- define_enroll_rate(
#'   duration = 12,
#'   rate = 500 / 12
#' )
#'
#' fail_rate <- define_fail_rate(
#'   duration = c(4, 100),
#'   fail_rate = log(2) / 15, # median survival 15 month
#'   hr = c(1, .6),
#'   dropout_rate = 0.001
#' )
#'
#' fh_test <- rbind(
#'   data.frame(
#'     rho = 0, gamma = 0, tau = -1,
#'     test = 1, analysis = 1:3, analysis_time = c(12, 24, 36)
#'   ),
#'   data.frame(
#'     rho = c(0, 0.5), gamma = 0.5, tau = -1,
#'     test = 2:3, analysis = 3, analysis_time = 36
#'   )
#' )
#'
#' x <- gsSurv(
#'   k = 3,
#'   test.type = 4,
#'   alpha = 0.025,
#'   beta = 0.2,
#'   astar = 0,
#'   timing = 1,
#'   sfu = sfLDOF,
#'   sfupar = 0,
#'   sfl = sfLDOF,
#'   sflpar = 0,
#'   lambdaC = 0.1,
#'   hr = 0.6,
#'   hr0 = 1,
#'   eta = 0.01,
#'   gamma = 10,
#'   R = 12,
#'   S = NULL,
#'   T = 36,
#'   minfup = 24,
#'   ratio = 1
#' )
#'
#' # Example 1 ----
#' # User-defined boundary
#' \donttest{
#' gs_design_combo(
#'   enroll_rate,
#'   fail_rate,
#'   fh_test,
#'   alpha = 0.025, beta = 0.2,
#'   ratio = 1,
#'   binding = FALSE,
#'   upar = x$upper$bound,
#'   lpar = x$lower$bound
#' )
#' }
#' # Example 2 ----
#' \donttest{
#' # Boundary derived by spending function
#' gs_design_combo(
#'   enroll_rate,
#'   fail_rate,
#'   fh_test,
#'   alpha = 0.025,
#'   beta = 0.2,
#'   ratio = 1,
#'   binding = FALSE,
#'   upper = gs_spending_combo,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025), # alpha spending
#'   lower = gs_spending_combo,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2), # beta spending
#' )
#' }
gs_design_combo <- function(
    enroll_rate = define_enroll_rate(
      duration = 12,
      rate = 500 / 12
    ),
    fail_rate = define_fail_rate(
      duration = c(4, 100),
      fail_rate = log(2) / 15,
      hr = c(1, .6),
      dropout_rate = 0.001
    ),
    fh_test = rbind(
      data.frame(
        rho = 0, gamma = 0, tau = -1,
        test = 1, analysis = 1:3,
        analysis_time = c(12, 24, 36)
      ),
      data.frame(
        rho = c(0, 0.5), gamma = 0.5, tau = -1,
        test = 2:3, analysis = 3,
        analysis_time = 36
      )
    ),
    ratio = 1,
    alpha = 0.025,
    beta = 0.2,
    binding = FALSE,
    upper = gs_b,
    upar = c(3, 2, 1),
    lower = gs_b,
    lpar = c(-1, 0, 1),
    algorithm = mvtnorm::GenzBretz(maxpts = 1e5, abseps = 1e-5),
    n_upper_bound = 1e3,
    ...) {
  # get the number of analysis/test
  n_analysis <- length(unique(fh_test$analysis))
  n_test <- max(fh_test$test)

  # obtain utilities
  utility <- gs_utility_combo(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    fh_test = fh_test,
    ratio = ratio,
    algorithm = algorithm,
    ...
  )

  info <- utility$info_all
  info_fh <- utility$info
  theta_fh <- utility$theta
  corr_fh <- utility$corr

  # check design type
  if (identical(lower, gs_b) && (!is.list(lpar))) {
    two_sided <- ifelse(identical(lpar, rep(-Inf, n_analysis)), FALSE, TRUE)
  } else {
    two_sided <- TRUE
  }

  if (all(fail_rate$hr == 1)) {
    stop("gs_design_combo() hr must not be equal to 1 throughout the study as this is the null hypothesis.")
  }

  # Information Fraction
  if (n_analysis == 1) {
    min_info_frac <- 1
  } else {
    info_frac <- tapply(info$info0, info$test, function(x) x / max(x))
    min_info_frac <- apply(do.call(rbind, info_frac), 2, min)
  }

  # Find sample size and bound
  sample_size <- max(info$n)
  n0 <- 0
  prob <- NULL
  while ((abs(sample_size - n0)) > 1e-2) {
    n0 <- sample_size

    # Obtain spending function
    bound <- gs_bound(
      alpha = upper(upar, info = min_info_frac),
      beta = lower(lpar, info = min_info_frac),
      analysis = info_fh$analysis,
      theta = theta_fh * sqrt(sample_size),
      corr = corr_fh,
      binding_lower_bound = binding,
      algorithm = algorithm,
      alpha_bound = identical(upper, gs_b),
      beta_bound = identical(lower, gs_b),
      ...
    )

    # calculate the difference between the derived power and the targeted power
    # (1 - beta), based on the provided sample size, upper and lower boundaries,
    # and treatment effect
    get_combo_power <- function(n, ...) {
      # Probability Cross Boundary
      prob <<- cache_fun(
        gs_prob_combo,
        upper_bound = bound$upper, lower_bound = bound$lower,
        analysis = info_fh$analysis, theta = theta_fh * sqrt(n),
        corr = corr_fh, algorithm = algorithm, ...
      )
      max(subset(prob, bound == "upper")$probability) - (1 - beta)
    }

    sample_size_results <- uniroot(
      get_combo_power, c(1, n_upper_bound), ..., extendInt = "yes"
    )
    sample_size <- sample_size_results$root
  }

  # Probability Cross Boundary under Null
  prob_null <- gs_prob_combo(
    upper_bound = bound$upper,
    lower_bound = if (two_sided) {
      bound$lower
    } else {
      rep(-Inf, nrow(bound))
    },
    analysis = info_fh$analysis,
    theta = rep(0, nrow(info_fh)),
    corr = corr_fh,
    algorithm = algorithm, ...
  )

  prob$probability_null <- prob_null$probability

  # Prepare output
  db <- merge(
    data.frame(analysis = 1:(nrow(prob) / 2), prob, z = unlist(bound)),
    info_fh |>
      tibble::as_tibble() |>
      select(analysis, time, n, event) |>
      unique()
  ) |>
    # update sample size and events
    mutate(
      event = event * sample_size / max(n),
      n = n * sample_size / max(n)
    ) |>
    # arrange the dataset by Upper bound first and then Lower bound
    arrange(analysis, desc(bound))


  out <- db |>
    select(
      analysis, bound, time, n, event, z,
      probability, probability_null
    ) |>
    rename(probability0 = probability_null) |>
    mutate(`nominal p` = pnorm(z * (-1)))


  # get bounds to output
  bounds <- out |>
    # rbind(out_H1, out_H0) |>
    select(analysis, bound, probability, probability0, z, `nominal p`) |>
    arrange(analysis, desc(bound))

  # get analysis summary to output
  # check if rho, gamma = 0 is included in fh_test
  tmp <- fh_test |>
    filter(rho == 0 & gamma == 0 & tau == -1) |>
    select(test) |>
    unlist() |>
    as.numeric() |>
    unique()

  if (length(tmp) != 0) {
    ahr_dis <- utility$info_all |>
      filter(test == tmp) |>
      select(ahr) |>
      unlist() |>
      as.numeric()
  } else {
    ahr_dis <- gs_info_wlr(
      enroll_rate,
      fail_rate,
      ratio,
      event = unique(utility$info_all$event),
      analysis_time = unique(utility$info_all$time),
      weight = eval(parse(text = get_combo_weight(rho = 0, gamma = 0, tau = -1)))
    )$AHR
  }

  analysis <- utility$info_all |>
    select(analysis, test, time, n, event) |>
    mutate(
      theta = utility$info_all$theta,
      event_frac = event / tapply(event, test, function(x) max(x)) |>
        unlist() |>
        as.numeric()
    ) |>
    select(analysis, time, n, event, event_frac) |>
    unique() |>
    mutate(ahr = ahr_dis) |>
    mutate(
      n = n * sample_size / max(info_fh$n),
      event = event * sample_size / max(info_fh$n)
    ) |>
    arrange(analysis)

  # Output ----
  output <- structure(
    list(
      design = "combo",
      enroll_rate = enroll_rate |> mutate(rate = rate * max(analysis$n) / sum(rate * duration)),
      fail_rate = fail_rate,
      bounds = bounds,
      analysis = analysis
    ),
    class = "gs_design",
    binding = binding
  )

  return(output)
}
