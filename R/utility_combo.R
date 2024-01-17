#  Copyright (c) 2024 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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

#' MaxCombo group sequential utility
#'
#' @inheritParams ahr
#' @param ratio Experimental:Control randomization ratio (not yet implemented).
#' @param fh_test Weighting tests.
#' @param algorithm Numerical algorithms.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Define the analysis time from input fh_test.
#'    \item Compute arm0 and arm1 using \code{gs_create_arm()}.
#'    \item Set a unique test.
#'    \item Compute the information fraction using \code{gs_info_combo()}.
#'    \item Compute the correlation between tests.
#'    \item Compute the correlation between analysis.
#'    \item Compute the overall correlation.
#'    \item Extract the sample size from  info.
#'    \item Compute information restricted to actual analysis.
#'    \item Compute the effect size.
#'    \item Return a list of info_all = info, info = info_fh, theta = theta_fh, corr = corr_fh.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @importFrom mvtnorm GenzBretz
#'
#' @noRd
gs_utility_combo <- function(enroll_rate,
                             fail_rate,
                             fh_test,
                             ratio = 1,
                             algorithm = GenzBretz(maxpts = 1e5, abseps = 1e-5),
                             ...) {
  # Define analysis time
  analysis_time <- sort(unique(fh_test$analysis_time))

  # Define Arm
  gs_arm <- gs_create_arm(enroll_rate, fail_rate,
    ratio = ratio, # Randomization ratio
    total_time = max(analysis_time)
  ) # Total study duration

  arm0 <- gs_arm[["arm0"]]
  arm1 <- gs_arm[["arm1"]]

  # Unique test
  u_fh_test <- unique(fh_test[, c("test", "rho", "gamma", "tau")])

  # Information Fraction
  info <- gs_info_combo(
    enroll_rate = enroll_rate, fail_rate = fail_rate,
    ratio = ratio, analysis_time = analysis_time,
    rho = u_fh_test$rho,
    gamma = u_fh_test$gamma
  )

  # Correlation between tests
  corr_test <- with(
    u_fh_test,
    lapply(analysis_time, function(tmax) {
      cov2cor(gs_sigma2_combo(arm0, arm1,
        tmax = tmax,
        rho = rho, gamma = gamma, tau = tau
      ))
    })
  )

  # Correlation between analysis
  info_split <- split(info, info$test)
  corr_time <- lapply(info_split, function(x) {
    corr <- with(x, outer(sqrt(info), sqrt(info), function(x, y) pmin(x, y) / pmax(x, y)))
    rownames(corr) <- analysis_time
    colnames(corr) <- analysis_time
    corr
  })

  # Overall Correlation
  corr_combo <- diag(1, nrow = nrow(info))
  for (i in seq_len(nrow(info))) {
    for (j in seq_len(nrow(info))) {
      t1 <- as.numeric(info$analysis[i])
      t2 <- as.numeric(info$analysis[j])
      if (t1 <= t2) {
        test1 <- as.numeric(info$test[i])
        test2 <- as.numeric(info$test[j])
        corr_combo[i, j] <- corr_test[[t1]][test1, test2] * corr_time[[test2]][t1, t2]
        corr_combo[j, i] <- corr_combo[i, j]
      }
    }
  }

  # Sample size
  n <- max(info$n)

  # Restricted to actual analysis
  info_fh <- info %>% left_join(fh_test %>% dplyr::rename(time = analysis_time), by = c("test", "analysis", "time"))
  corr_fh <- corr_combo[!is.na(info_fh$gamma), !is.na(info_fh$gamma)]
  info_fh <- subset(info_fh, !is.na(gamma))

  # Effect size
  theta_fh <- (-info_fh$delta) / sqrt(info_fh$sigma2)

  list(info_all = info, info = info_fh, theta = theta_fh, corr = corr_fh)
}


#' Multivariate Normal Distribution for Multivariate Maximum Statistics
#'
#' Computes the distribution function of the multivariate normal distribution
#' with maximum statistics for arbitrary limits and correlation matrices.
#'
#' @details
#' Let \eqn{Z = {Z_{ij}}} be a multivariate normal distribution.
#' Here, \eqn{i} is a group indicator, \eqn{j} is a within group statistics
#' indicator. Let \eqn{G_i = \max({Z_{ij}})} for all test within one group.
#' This program calculates the probability
#' \deqn{\Pr( \text{lower} < \max(G) < \text{upper} ).}
#'
#' @inheritParams mvtnorm::pmvnorm
#'
#' @param group The vector of test statistics group.
#' @param ... Additional parameters passed to [mvtnorm::pmvnorm].
#'
#' @importFrom mvtnorm GenzBretz
#'
#' @noRd
pmvnorm_combo <- function(lower,
                          upper,
                          group,
                          mean,
                          corr,
                          algorithm = GenzBretz(maxpts = 1e5, abseps = 1e-5),
                          ...) {
  # Number of test in each group
  n_test <- as.numeric(table(group))


  # Ensure positive definitive of the correlation matrix
  if (!corpcor::is.positive.definite(corr)) {
    corr <- corpcor::make.positive.definite(corr)
    corr <- stats::cov2cor(corr)
  }

  # One dimension test
  if (length(mean) == 1) {
    p <- pnorm(mean, lower) - pnorm(mean, upper)
    return(p)
  }

  # One test for all group or lower bound is -Inf.
  if (all(n_test == 1) || all(lower == -Inf)) {
    p <- mvtnorm::pmvnorm(
      lower = rep(lower, n_test),
      upper = rep(upper, n_test),
      mean = mean,
      corr = corr,
      sigma = NULL,
      algorithm = algorithm,
      ...
    )

    return(p)

    # General Algorithm
  } else {
    # Re-arrange test based on the order for number of test
    group <- as.numeric(factor(group, order(n_test)))

    mean <- mean[order(group)]
    corr <- corr[order(group), order(group)]
    group <- group[order(group)]

    n_test <- as.numeric(table(group))



    # Split by number of test == 1
    lower1 <- lower[n_test == 1]
    upper1 <- upper[n_test == 1]

    lower2 <- lower[n_test > 1]
    upper2 <- upper[n_test > 1]

    # Combination of all possible test
    k <- length(lower2)
    test_ind <- split(matrix(c(1, -1), nrow = k, ncol = 2, byrow = TRUE), 1:k)
    test_ind <- expand.grid(test_ind)
    test <- split(test_ind, seq_len(nrow(test_ind)))

    p <- sapply(test, function(x) {
      lower_bound <- rep(c(lower1, rep(-Inf, k)), n_test)
      upper_bound <- rep(c(upper1, ifelse(x == 1, upper2, lower2)), n_test)

      p_bound <- mvtnorm::pmvnorm(
        lower = lower_bound,
        upper = upper_bound,
        mean = mean,
        corr = corr,
        sigma = NULL,
        algorithm = algorithm,
        ...
      )

      prod(x) * p_bound
    })

    return(sum(p))
  }
}


#' Create weight in MaxCombo test
#'
#' @param rho Weighting parameter.
#' @param gamma Weighting parameter.
#' @param tau Weighting parameter.
#'
#' @noRd
get_combo_weight <- function(rho, gamma, tau) {
  stopifnot(length(rho) == length(gamma))
  stopifnot(length(rho) == length(tau))

  weight <- list()
  for (i in seq_along(rho)) {
    if (tau[i] == -1) {
      tmp_tau <- NULL
    } else {
      tmp_tau <- tau[i]
    }

    text <- paste0(
      "weight <- function(x, arm0, arm1){
                            wlr_weight_fh(x, arm0, arm1
                                ,rho =", rho[i],
      ", gamma =", gamma[i],
      ", tau =", tmp_tau, ")}"
    )

    weight[[i]] <- text
  }

  weight
}

#' Compute delta in MaxCombo test
#'
#' @noRd
gs_delta_combo <- function(arm0,
                           arm1,
                           tmax = NULL,
                           rho,
                           gamma,
                           tau = rep(-1, length(rho)),
                           approx = "asymptotic",
                           normalization = FALSE) {
  stopifnot(length(tmax) == 1)

  weight <- get_combo_weight(rho, gamma, tau)
  delta <- sapply(weight, function(x) {
    x <- eval(parse(text = x))
    gs_delta_wlr(arm0, arm1,
      tmax = tmax, weight = x,
      approx = approx, normalization = normalization
    )
  })

  delta
}

#' Compute delta in MaxCombo test
#'
#' @noRd
gs_sigma2_combo <- function(arm0,
                            arm1,
                            tmax = NULL,
                            rho,
                            gamma,
                            tau = rep(-1, length(rho)),
                            approx = "asymptotic") {
  stopifnot(length(tmax) == 1)
  stopifnot(length(rho) == length(gamma))
  stopifnot(length(rho) == length(tau))

  rho1 <- outer(rho, rho, function(x, y) (x + y) / 2)
  gamma1 <- outer(gamma, gamma, function(x, y) (x + y) / 2)

  sigma2 <- rho1
  for (i in seq_along(rho)) {
    weight <- get_combo_weight(rho1[i, ], gamma1[i, ], tau)

    sigma2[i, ] <- sapply(weight, function(x) {
      x <- eval(parse(text = x))
      gs_sigma2_wlr(arm0, arm1,
        tmax = tmax, weight = x,
        approx = approx
      )
    })
  }

  sigma2
}

#' MaxCombo group sequential boundary crossing probabilities
#'
#' @inheritParams pmvnorm_combo
#' @param upper_bound A numeric vector of upper bound.
#' @param lower_bound A numeric vector of lower bound.
#' @param analysis An integer vector of the interim analysis index.
#' @param theta A numeric vector of effect size under alternative hypothesis.
#' @param corr A matrix of correlation matrix.
#'
#' @importFrom mvtnorm GenzBretz
#'
#' @noRd
gs_prob_combo <- function(upper_bound,
                          lower_bound,
                          analysis,
                          theta,
                          corr,
                          algorithm = GenzBretz(maxpts = 1e5, abseps = 1e-5),
                          ...) {
  n_analysis <- length(unique(analysis))

  p <- c()
  q <- c()
  for (k in 1:n_analysis) {
    k_ind <- analysis <= k


    # Efficacy Bound
    if (k == 1) {
      lower <- upper_bound[1]
      upper <- Inf
    } else {
      lower <- c(lower_bound[1:(k - 1)], upper_bound[k])
      upper <- c(upper_bound[1:(k - 1)], Inf)
    }


    p[k] <- pmvnorm_combo(lower,
      upper,
      group = analysis[k_ind],
      mean = theta[k_ind],
      corr = corr[k_ind, k_ind],
      ...
    )

    # Futility Bound
    if (k == 1) {
      lower <- -Inf
      upper <- lower_bound[k]
    } else {
      lower <- c(lower_bound[1:(k - 1)], -Inf)
      upper <- c(upper_bound[1:(k - 1)], lower_bound[k])
    }

    q[k] <- pmvnorm_combo(lower,
      upper,
      group = analysis[k_ind],
      mean  = theta[k_ind],
      corr  = corr[k_ind, k_ind],
      ...
    )
  }

  data.frame(
    bound = rep(c("upper", "lower"), each = n_analysis),
    probability = c(cumsum(p), cumsum(q))
  )
}


#' Lower and upper bound of group sequential design
#'
#' @param alpha A numeric vector of cumulative allocated alpha in each interim analysis.
#' @param beta A numeric vector of cumulative allocated beta in each interim analysis.
#' @param theta A numeric vector of effect size under alternative.
#' @param corr A matrix of correlation matrix.
#' @param analysis A numeric vector of interim analysis indicator. Default is `1:length(alpha)`.
#' @param theta0 A numeric vector of effect size under null hypothesis. Default is 0.
#' @param binding_lower_bound A logical value to indicate binding lower bound.
#' @param alpha_bound Logical value to indicate if alpha is Type I error or upper bound. Default is `FALSE`.
#' @param beta_bound Logical value to indicate if beta is Type II error or lower bound. Default is `FALSE`.
#' @inheritParams pmvnorm_combo
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Create a vector of allocated alpha in each interim analysis from the cumulative allocated alpha.
#'    \item Create a vector of allocated beta in each interim analysis from the cumulative allocated beta.
#'    \item Extract the number of analysis.
#'    \item Find the upper and lower bound by solving multivariate normal distribution using \code{pmvnorm_combo}
#'    \item
#'    \item Return a data frame of upper and lower boundaries of group sequential design.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @importFrom mvtnorm GenzBretz
#'
#' @examples
#' library(gsDesign)
#'
#' x <- gsDesign::gsSurv(
#'   k = 3, test.type = 4, alpha = 0.025,
#'   beta = 0.2, astar = 0, timing = c(1),
#'   sfu = sfLDOF, sfupar = c(0), sfl = sfLDOF,
#'   sflpar = c(0), lambdaC = c(0.1),
#'   hr = 0.6, hr0 = 1, eta = 0.01,
#'   gamma = c(10),
#'   R = c(12), S = NULL,
#'   T = 36, minfup = 24, ratio = 1
#' )
#'
#' cbind(x$lower$bound, x$upper$bound)
#'
#' gs_bound(
#'   alpha = sfLDOF(0.025, 1:3 / 3)$spend,
#'   beta = sfLDOF(0.2, 1:3 / 3)$spend,
#'   analysis = 1:3,
#'   theta = x$theta[2] * sqrt(x$n.I),
#'   corr = outer(1:3, 1:3, function(x, y) pmin(x, y) / pmax(x, y))
#' )
#'
#' @noRd
#'
gs_bound <- function(alpha,
                     beta,
                     theta,
                     corr,
                     analysis = seq_along(alpha),
                     theta0 = rep(0, length(analysis)),
                     binding_lower_bound = FALSE,
                     algorithm = GenzBretz(maxpts = 1e5, abseps = 1e-5),
                     alpha_bound = FALSE,
                     beta_bound = FALSE,
                     ...) {
  alpha <- c(alpha[1], diff(alpha))

  if (all(is.infinite(beta))) {
    beta <- beta
  } else {
    beta <- c(beta[1], diff(beta))
  }


  lower <- NULL
  upper <- NULL
  .lower <- -Inf

  n_analysis <- length(unique(analysis))

  for (k in 1:n_analysis) {
    k_ind <- analysis <= k

    bound_fun <- function(.lower, .upper, .prob, .theta, binding_lower_bound = FALSE) {
      if (binding_lower_bound) {
        lower_bound <- c(lower, .lower)
      } else {
        lower_bound <- c(rep(-Inf, k - 1), .lower)
      }
      upper_bound <- c(upper, .upper)

      p <- pmvnorm_combo(lower_bound,
        upper_bound,
        group = analysis[k_ind],
        mean = .theta[k_ind],
        corr = corr[k_ind, k_ind], ...
      )

      p - .prob
    }


    # change .lower for different type of test (gsDesign test.type)
    if (beta_bound) {
      .lower <- sum(beta[1:k])
    } else {
      .lower <- uniroot(bound_fun,
        .lower = -Inf, .prob = beta[k], .theta = theta,
        binding_lower_bound = TRUE,
        interval = c(-20, 20), extendInt = "yes"
      )$root
    }

    if (alpha_bound) {
      .upper <- sum(alpha[1:k])
    } else {
      .upper <- uniroot(bound_fun,
        .upper = Inf, .prob = alpha[k], .theta = theta0,
        binding_lower_bound = binding_lower_bound,
        interval = c(-20, 20), extendInt = "yes"
      )$root
    }

    lower <- c(lower, .lower)
    upper <- c(upper, .upper)
  }

  data.frame(upper = upper, lower = lower)
}
