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

#' Group sequential design power using MaxCombo test under non-proportional hazards
#'
#' @inheritParams gs_design_combo
#'
#' @return A list with input parameters, enrollment rate, analysis, and bound.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if lower and upper bounds have been specified.
#'    \item Extract info, info_fh, theta_fh and corr_fh from utility.
#'    \item Extract sample size via the maximum sample size of info.
#'    \item Calculate information fraction either for fixed or group sequential design.
#'    \item Compute spending function using \code{gs_bound()}.
#'    \item Compute probability of crossing bounds under the null and alternative
#'     hypotheses using \code{gs_prob_combo()}.
#'    \item Export required information for boundary and crossing probability
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @export
#'
#' @examples
#' library(mvtnorm)
#' library(gsDesign)
#' library(gsDesign2)
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
#'   data.frame(rho = 0, gamma = 0, tau = -1, test = 1, analysis = 1:3, analysis_time = c(12, 24, 36)),
#'   data.frame(rho = c(0, 0.5), gamma = 0.5, tau = -1, test = 2:3, analysis = 3, analysis_time = 36)
#' )
#'
#' # Example 1 ----
#' # Minimal Information Fraction derived bound
#' \donttest{
#' gs_power_combo(
#'   enroll_rate = enroll_rate,
#'   fail_rate = fail_rate,
#'   fh_test = fh_test,
#'   upper = gs_spending_combo,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
#'   lower = gs_spending_combo,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2)
#' )
#' }
gs_power_combo <- function(
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
        rho = 0, gamma = 0, tau = -1, test = 1,
        analysis = 1:3, analysis_time = c(12, 24, 36)
      ),
      data.frame(
        rho = c(0, 0.5), gamma = 0.5, tau = -1, test = 2:3,
        analysis = 3, analysis_time = 36
      )
    ),
    ratio = 1,
    binding = FALSE,
    upper = gs_b,
    upar = c(3, 2, 1),
    lower = gs_b,
    lpar = c(-1, 0, 1),
    algorithm = mvtnorm::GenzBretz(maxpts = 1e5, abseps = 1e-5),
    ...) {
  # Currently only support user-defined lower and upper bound
  stopifnot(identical(upper, gs_b) | identical(upper, gs_spending_combo))
  stopifnot(identical(lower, gs_b) | identical(lower, gs_spending_combo))

  # Get the number of analysis/test ----
  n_analysis <- length(unique(fh_test$analysis))
  n_test <- max(fh_test$test)

  # Obtain utilities
  utility <- gs_utility_combo(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    fh_test = fh_test,
    ratio = ratio,
    algorithm = algorithm, ...
  )

  info <- utility$info_all
  info_fh <- utility$info
  theta_fh <- utility$theta
  corr_fh <- utility$corr

  # Sample size
  sample_size <- max(info$n)

  # Information Fraction
  if (length(unique(fh_test$analysis)) == 1) {
    # Fixed design
    min_info_frac <- 1
  } else {
    info_frac <- tapply(info$info0, info$test, function(x) x / max(x))
    min_info_frac <- apply(do.call(rbind, info_frac), 2, min)
  }

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


  # Probability Cross Boundary under Alternative
  prob <- gs_prob_combo(
    upper_bound = bound$upper,
    lower_bound = bound$lower,
    analysis = info_fh$analysis,
    theta = theta_fh * sqrt(sample_size),
    corr = corr_fh,
    algorithm = algorithm, ...
  )

  # Probability Cross Boundary under Null
  prob_null <- gs_prob_combo(
    upper_bound = bound$upper,
    lower_bound = if (binding) {
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
    arrange(analysis, desc(bound))

  # Get bounds to output ----
  bound <- db |>
    mutate(`nominal p` = pnorm(z * (-1))) |>
    select(analysis, bound, probability, probability_null, z, `nominal p`) |>
    rename(probability0 = probability_null) |>
    arrange(analysis, desc(bound))

  # Get analysis summary to output ----
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
    )$ahr
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
      event = event * n / max(info_fh$n)
    ) |>
    arrange(analysis)

  # Output ----
  output <- structure(
    list(
      design = "combo",
      enroll_rate = enroll_rate |> mutate(rate = rate * max(analysis$n) / sum(rate * duration)),
      fail_rate = fail_rate,
      bound = bound,
      analysis = analysis
    ),
    class = "gs_design",
    binding = binding
  )

  return(output)
}
