#  Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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

#' Group sequential bound computation with non-constant effect
#'
#' Derives group sequential bounds and boundary crossing probabilities for a design.
#' It allows a non-constant treatment effect over time,
#' but also can be applied for the usual homogeneous effect size designs.
#' It requires treatment effect and statistical information at each analysis
#' as well as a method of deriving bounds, such as spending.
#' The routine enables two things not available in the gsDesign package:
#' 1) non-constant effect, 2) more flexibility in boundary selection.
#' For many applications, the non-proportional-hazards design function
#' `gs_design_nph()` will be used; it calls this function.
#' Initial bound types supported are 1) spending bounds,
#' 2) fixed bounds, and 3) Haybittle-Peto-like bounds.
#' The requirement is to have a boundary update method that can
#' each bound without knowledge of future bounds.
#' As an example, bounds based on conditional power that require
#' knowledge of all future bounds are not supported by this routine;
#' a more limited conditional power method will be demonstrated.
#' Boundary family designs Wang-Tsiatis designs including the
#' original (non-spending-function-based) O'Brien-Fleming and Pocock designs
#' are not supported by `gs_power_npe()`.
#'
#' @param theta Natural parameter for group sequential design representing
#'   expected incremental drift at all analyses; used for power calculation.
#' @param theta0 Natural parameter for null hypothesis,
#'   if needed for upper bound computation.
#' @param theta1 Natural parameter for alternate hypothesis,
#'   if needed for lower bound computation.
#' @param info Statistical information at all analyses for input `theta`.
#' @param info0 Statistical information under null hypothesis,
#'   if different than `info`;
#'   impacts null hypothesis bound calculation.
#' @param info1 Statistical information under hypothesis used for
#'   futility bound calculation if different from
#'   `info`; impacts futility hypothesis bound calculation.
#' @param info_scale The information scale for calculation,
#'   default is 2, other options are 0 or 1.
#' @param binding Indicator of whether futility bound is binding;
#'   default of `FALSE` is recommended.
#' @param upper Function to compute upper bound.
#' @param lower Function to compare lower bound.
#' @param upar Parameters passed to `upper`.
#' @param lpar parameters passed to `lower`.
#' @param test_upper Indicator of which analyses should include
#'   an upper (efficacy) bound;
#'   single value of `TRUE` (default) indicates all analyses; otherwise,
#'   a logical vector of the same length as `info` should
#'   indicate which analyses will have an efficacy bound.
#' @param test_lower Indicator of which analyses should include a lower bound;
#'   single value of `TRUE` (default) indicates all analyses;
#'   single value of `FALSE` indicated no lower bound; otherwise,
#'   a logical vector of the same length as `info` should
#'   indicate which analyses will have a lower bound.
#' @param r Integer value controlling grid for numerical integration as in
#'   Jennison and Turnbull (2000); default is 18, range is 1 to 80.
#'   Larger values provide larger number of grid points and greater accuracy.
#'   Normally, `r` will not be changed by the user.
#' @param tol Tolerance parameter for boundary convergence (on Z-scale).
#'
#' @return A tibble with columns as analysis index, bounds, z,
#'   crossing probability, theta (standardized treatment effect),
#'   theta1 (standardized treatment effect under alternative hypothesis),
#'   information fraction, and statistical information.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Extract the length of input info as the number of interim analysis.
#'    \item Validate if input info0 is NULL, so set it equal to info.
#'    \item Validate if the length of inputs info and info0 are the same.
#'    \item Validate if input theta is a scalar, so replicate
#'    the value for all k interim analysis.
#'    \item Validate if input theta1 is NULL and if it is a scalar.
#'    If it is NULL, set it equal to input theta. If it is a scalar,
#'    replicate the value for all k interim analysis.
#'    \item Validate if input test_upper is a scalar,
#'    so replicate the value for all k interim analysis.
#'    \item Validate if input test_lower is a scalar,
#'    so replicate the value for all k interim analysis.
#'    \item Define vector a to be -Inf with
#'    length equal to the number of interim analysis.
#'    \item Define vector b to be Inf with
#'    length equal to the number of interim analysis.
#'    \item Define hgm1_0 and hgm1 to be NULL.
#'    \item Define upper_prob and lower_prob to be
#'    vectors of NA with length of the number of interim analysis.
#'    \item Update lower and upper bounds using \code{gs_b()}.
#'    \item If there are no interim analysis, compute probabilities
#'    of crossing upper and lower bounds
#'    using \code{h1()}.
#'    \item Compute cross upper and lower bound probabilities
#'    using \code{hupdate()} and \code{h1()}.
#'    \item Return a tibble of analysis number, bound, z-values,
#'    probability of crossing bounds,
#'    theta, theta1, info, and info0.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @author Keaven Anderson \email{keaven_anderson@@merck.com}
#'
#' @importFrom tibble tibble
#' @importFrom stats qnorm pnorm
#'
#' @export
#'
#' @examples
#' library(gsDesign)
#' library(gsDesign2)
#' library(dplyr)
#'
#' # Default (single analysis; Type I error controlled)
#' gs_power_npe(theta = 0) %>% filter(bound == "upper")
#'
#' # Fixed bound
#' gs_power_npe(
#'   theta = c(.1, .2, .3),
#'   info = (1:3) * 40,
#'   upper = gs_b,
#'   upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
#'   lower = gs_b,
#'   lpar = c(-1, 0, 0)
#' )
#'
#' # Same fixed efficacy bounds, no futility bound (i.e., non-binding bound), null hypothesis
#' gs_power_npe(
#'   theta = rep(0, 3),
#'   info = (1:3) * 40,
#'   upar = gsDesign::gsDesign(k = 3, sfu = gsDesign::sfLDOF)$upper$bound,
#'   lpar = rep(-Inf, 3)
#' ) %>%
#'   filter(bound == "upper")
#'
#' # Fixed bound with futility only at analysis 1; efficacy only at analyses 2, 3
#' gs_power_npe(
#'   theta = c(.1, .2, .3),
#'   info = (1:3) * 40,
#'   upper = gs_b,
#'   upar = c(Inf, 3, 2),
#'   lower = gs_b,
#'   lpar = c(qnorm(.1), -Inf, -Inf)
#' )
#'
#' # Spending function bounds
#' # Lower spending based on non-zero effect
#' gs_power_npe(
#'   theta = c(.1, .2, .3),
#'   info = (1:3) * 40,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
#' )
#'
#' # Same bounds, but power under different theta
#' gs_power_npe(
#'   theta = c(.15, .25, .35),
#'   info = (1:3) * 40,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfHSD, total_spend = 0.1, param = -1, timing = NULL)
#' )
#'
#' # Two-sided symmetric spend, O'Brien-Fleming spending
#' # Typically, 2-sided bounds are binding
#' x <- gs_power_npe(
#'   theta = rep(0, 3),
#'   info = (1:3) * 40,
#'   binding = TRUE,
#'   upper = gs_spending_bound,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
#'   lower = gs_spending_bound,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL)
#' )
#'
#' # Re-use these bounds under alternate hypothesis
#' # Always use binding = TRUE for power calculations
#' gs_power_npe(
#'   theta = c(.1, .2, .3),
#'   info = (1:3) * 40,
#'   binding = TRUE,
#'   upar = (x %>% filter(bound == "upper"))$z,
#'   lpar = -(x %>% filter(bound == "upper"))$z
#' )
gs_power_npe <- function(theta = .1, theta0 = NULL, theta1 = NULL, # 3 theta
                         info = 1, info0 = NULL, info1 = NULL, # 3 info
                         info_scale = c(0, 1, 2),
                         upper = gs_b, upar = qnorm(.975),
                         lower = gs_b, lpar = -Inf,
                         test_upper = TRUE, test_lower = TRUE, binding = FALSE,
                         r = 18, tol = 1e-6) {
  # --------------------------------------------- #
  #     check & set up parameters                 #
  # --------------------------------------------- #
  n_analysis <- length(info)
  if (length(theta) == 1 && n_analysis > 1) theta <- rep(theta, n_analysis)
  if (is.null(theta0)) {
    theta0 <- rep(0, n_analysis)
  } else if (length(theta0) == 1) {
    theta0 <- rep(theta0, n_analysis)
  }
  if (is.null(theta1)) {
    theta1 <- theta
  } else if (length(theta1) == 1) {
    theta1 <- rep(theta1, n_analysis)
  }
  if (length(test_upper) == 1 && n_analysis > 1) test_upper <- rep(test_upper, n_analysis)
  if (length(test_lower) == 1 && n_analysis > 1) test_lower <- rep(test_lower, n_analysis)

  # --------------------------------------------- #
  #     set up info                               #
  # --------------------------------------------- #
  # impute info
  if (is.null(info0)) {
    info0 <- info
  }

  if (is.null(info1)) {
    info1 <- info
  }

  # set up info_scale
  info_scale <- if (methods::missingArg(info_scale)) {
    2
  } else {
    match.arg(as.character(info_scale), choices = 0:2)
  }
  if (info_scale == 0) {
    info <- info0
    info1 <- info0
  }
  if (info_scale == 1) {
    info <- info1
    info0 <- info1
  }

  # check info
  check_info(info)
  check_info(info0)
  check_info(info1)
  if (length(info0) != length(info)) stop("gs_design_npe(): length of info, info0 must be the same!")
  if (length(info1) != length(info)) stop("gs_design_npe(): length of info, info1 must be the same!")


  # --------------------------------------------- #
  #     initialization                            #
  # --------------------------------------------- #
  a <- rep(-Inf, n_analysis)
  b <- rep(Inf, n_analysis)
  hgm1_0 <- NULL
  hgm1_1 <- NULL
  hgm1 <- NULL
  upper_prob <- rep(NA, n_analysis)
  lower_prob <- rep(NA, n_analysis)

  # --------------------------------------------- #
  #     calculate crossing prob  under  H1        #
  # --------------------------------------------- #
  for (k in 1:n_analysis) {
    # compute/update lower/upper bound
    a[k] <- lower(
      k = k, par = lpar, hgm1 = hgm1_1, info = info1, r = r, tol = tol, test_bound = test_lower,
      theta = theta1, efficacy = FALSE
    )
    b[k] <- upper(k = k, par = upar, hgm1 = hgm1_0, info = info0, r = r, tol = tol, test_bound = test_upper)

    # if it is the first analysis
    if (k == 1) {
      # compute the probability to cross upper/lower bound
      upper_prob[1] <- if (b[1] < Inf) {
        pnorm(sqrt(info[1]) * (theta[1] - b[1] / sqrt(info0[1])))
      } else {
        0
      }
      lower_prob[1] <- if (a[1] > -Inf) {
        pnorm(-sqrt(info[1]) * (theta[1] - a[1] / sqrt(info0[1])))
      } else {
        0
      }
      # update the grids
      hgm1_0 <- h1(r = r, theta = theta0[1], info = info0[1], a = if (binding) {
        a[1]
      } else {
        -Inf
      }, b = b[1])
      hgm1_1 <- h1(r = r, theta = theta1[1], info = info1[1], a = a[1], b = b[1])
      hgm1 <- h1(r = r, theta = theta[1], info = info[1], a = a[1], b = b[1])
    } else {
      # compute the probability to cross upper bound
      upper_prob[k] <- if (b[k] < Inf) {
        sum(hupdate(
          theta = theta[k], thetam1 = theta[k - 1],
          info = info[k], im1 = info[k - 1],
          a = b[k], b = Inf, gm1 = hgm1, r = r
        )$h)
      } else {
        0
      }
      # compute the probability to cross lower bound
      lower_prob[k] <- if (a[k] > -Inf) {
        sum(hupdate(
          theta = theta[k], thetam1 = theta[k - 1],
          info = info[k], im1 = info[k - 1],
          a = -Inf, b = a[k], gm1 = hgm1, r = r
        )$h)
      } else {
        0
      }

      # update the grids
      if (k < n_analysis) {
        hgm1_0 <- hupdate(r = r, theta = theta0[k], info = info0[k], a = if (binding) {
          a[k]
        } else {
          -Inf
        }, b = b[k], thetam1 = 0, im1 = info0[k - 1], gm1 = hgm1_0)
        hgm1_1 <- hupdate(
          r = r, theta = theta1[k], info = info1[k],
          a = a[k], b = b[k], thetam1 = theta1[k - 1],
          im1 = info1[k - 1], gm1 = hgm1_1
        )
        hgm1 <- hupdate(
          r = r, theta = theta[k], info = info[k],
          a = a[k], b = b[k], thetam1 = theta[k - 1],
          im1 = info[k - 1], gm1 = hgm1
        )
      }
    }
  }

  ans <- tibble::tibble(
    analysis = rep(1:n_analysis, 2),
    bound = c(rep("upper", n_analysis), rep("lower", n_analysis)),
    z = c(b, a),
    probability = c(cumsum(upper_prob), cumsum(lower_prob)),
    theta = rep(theta, 2),
    theta1 = rep(theta1, 2),
    info_frac = rep(info / max(info), 2),
    info = rep(info, 2)
  ) %>%
    mutate(
      info0 = rep(info0, 2),
      info1 = rep(info1, 2)
    ) %>%
    # filter(abs(Z) < Inf) %>%
    arrange(desc(bound), analysis)

  return(ans)
}
