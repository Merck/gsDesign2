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

#' Piecewise exponential cumulative distribution function
#'
#' Computes the cumulative distribution function (CDF) or survival rate
#' for a piecewise exponential distribution, which may be stratified.
#'
#' @param x Times at which distribution is to be computed.
#' @param duration A numeric vector of time duration.
#' @param rate A numeric vector of event rate.
#' @param lower_tail Indicator of whether lower (`TRUE`) or upper tail
#'   (`FALSE`; default) of CDF is to be computed.
#' @param stratum A vector of stratum labels parallel to `duration` and `rate`,
#'   identifying the stratum each interval belongs to, e.g., the `stratum` column
#'   of a data frame created by [define_fail_rate()]. `NULL` (default), or a
#'   single distinct value, means the distribution is not stratified.
#' @param stratum_prev A numeric vector of stratum prevalences (the proportion of
#'   each stratum in the population), which must sum to 1. Either named (matched
#'   to the values of `stratum`) or unnamed (matched to `unique(stratum)` in
#'   order of appearance). Required when `stratum` has more than one distinct
#'   value, and ignored otherwise.
#'
#' @return A vector with cumulative distribution function or survival values.
#'
#' @details
#' Suppose \eqn{\lambda_i} is the failure rate in the interval
#' \eqn{(t_{i-1},t_i], i=1,2,\ldots,M} where
#' \eqn{0=t_0<t_i\ldots,t_M=\infty}.
#' The cumulative hazard function at an arbitrary time \eqn{t>0} is then:
#'
#' \deqn{\Lambda(t)=\sum_{i=1}^M \delta(t\leq t_i)(\min(t,t_i)-t_{i-1})\lambda_i.}
#' The survival at time \eqn{t} is then
#' \deqn{S(t)=\exp(-\Lambda(t)).}
#'
#' For a stratified distribution, each stratum \eqn{k} has its own intervals and
#' failure rates, hence its own survival \eqn{S_k(t)} as above. The marginal
#' (population-level) survival is the mixture of those per-stratum curves,
#' weighted by the stratum prevalences \eqn{w_k} (summing to 1):
#'
#' \deqn{S(t)=\sum_k w_k S_k(t).}
#'
#' This is the survival of a randomly selected member of the population, and is
#' generally *not* itself a piecewise exponential distribution: a mixture of
#' exponentials is not exponential. The marginal curve can therefore not be
#' obtained by pooling the per-stratum failure rates, and has to be averaged on
#' the survival scale as above.
#'
#' To obtain the marginal survival of the experimental arm of a design, multiply
#' the failure rates by the hazard ratios before calling this function.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input enrollment rate is a strictly increasing non-negative numeric vector.
#'    \item Validate if input failure rate is of type data.frame.
#'    \item Validate if input failure rate contains duration column.
#'    \item Validate if input failure rate contains rate column.
#'    \item Validate if input lower_tail is logical.
#'    \item Convert rates to step function.
#'    \item Add times where rates change to enrollment rates.
#'    \item Make a tibble of the input time points x, duration, hazard rates at points,
#'    cumulative hazard and survival.
#'    \item Extract the expected cumulative or survival of piecewise exponential distribution.
#'    \item For a stratified distribution, average the per-stratum survival values
#'    weighted by the stratum prevalences.
#'    \item If input lower_tail is true, return the CDF, else return the survival for \code{ppwe}
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @export
#'
#' @examples
#'
#' # Plot a survival function with 2 different sets of time values
#' # to demonstrate plot precision corresponding to input parameters.
#'
#' x1 <- seq(0, 10, 10 / pi)
#' duration <- c(3, 3, 1)
#' rate <- c(.2, .1, .005)
#'
#' survival <- ppwe(
#'   x = x1,
#'   duration = duration,
#'   rate = rate
#' )
#' plot(x1, survival, type = "l", ylim = c(0, 1))
#'
#' x2 <- seq(0, 10, .25)
#' survival <- ppwe(
#'   x = x2,
#'   duration = duration,
#'   rate = rate
#' )
#' lines(x2, survival, col = 2)
#'
#' # A stratified distribution: the marginal survival of the control arm of a
#' # design with a 30%/70% split between two biomarker strata.
#' fail_rate <- rbind(
#'   define_fail_rate(
#'     duration = c(6, Inf), fail_rate = log(2) / c(8, 10),
#'     hr = c(1, .7), dropout_rate = .001, stratum = "Biomarker positive"
#'   ),
#'   define_fail_rate(
#'     duration = c(6, Inf), fail_rate = log(2) / c(20, 24),
#'     hr = c(1, .5), dropout_rate = .001, stratum = "Biomarker negative"
#'   )
#' )
#' x <- seq(0, 24, 4)
#' stratum_prev <- c("Biomarker positive" = .3, "Biomarker negative" = .7)
#' with(fail_rate, ppwe(x, duration, fail_rate, stratum = stratum, stratum_prev = stratum_prev))
#'
#' # The marginal curve lies between the two per-stratum curves.
#' split(fail_rate, ~stratum) |>
#'   vapply(function(d) ppwe(x, d$duration, d$fail_rate), numeric(length(x)))
#'
#' # The marginal survival of the experimental arm.
#' with(fail_rate, ppwe(x, duration, fail_rate * hr, stratum = stratum, stratum_prev = stratum_prev))
ppwe <- function(x, duration, rate, lower_tail = FALSE, stratum = NULL,
                 stratum_prev = NULL) {
  # Check input enrollment rate assumptions
  check_non_negative(x)
  check_increasing(x, first = FALSE)

  strata <- unique(stratum)
  survival <- if (length(strata) > 1) {
    if (length(stratum) != length(duration)) stop(
      "`stratum` must be of the same length as `duration` and `rate`"
    )
    stratum_prev <- align_stratum_prev(stratum_prev, strata)
    # average the per-stratum survival curves on the survival scale
    Reduce(`+`, lapply(seq_along(strata), function(i) {
      j <- stratum == strata[i]
      stratum_prev[i] * exp(-cumulative_rate(x, duration[j], rate[j], last_(rate[j])))
    }))
  } else {
    H <- cumulative_rate(x, duration, rate, last_(rate)) # cumulative hazard
    exp(-H) # survival
  }

  # return survival or CDF
  if (lower_tail) 1 - survival else survival
}

# align `stratum_prev` with `strata` (by name when named, by position otherwise)
# and validate that the prevalences are non-negative and sum to 1
align_stratum_prev <- function(stratum_prev, strata) {
  if (is.null(stratum_prev)) stop(
    "`stratum_prev` must be provided when there is more than one stratum"
  )
  if (!is.numeric(stratum_prev)) stop("`stratum_prev` must be numeric")
  if (!is.null(names(stratum_prev))) {
    if (!setequal(names(stratum_prev), strata)) stop(
      "names(stratum_prev) must match the strata: ", paste(strata, collapse = ", ")
    )
    stratum_prev <- stratum_prev[as.character(strata)] # align with the strata order
  } else if (length(stratum_prev) != length(strata)) stop(
    "`stratum_prev` must be of length ", length(strata), " (the number of strata)"
  )
  check_non_negative(stratum_prev)
  if (!isTRUE(all.equal(sum(stratum_prev), 1))) stop(
    "`stratum_prev` must sum to 1"
  )
  stratum_prev
}

#' Approximate survival distribution with piecewise exponential distribution
#'
#' Converts a discrete set of points from an arbitrary survival distribution
#' to a piecewise exponential approximation.
#'
#' @param times Positive increasing times at which survival distribution is provided.
#' @param survival Survival (1 - cumulative distribution function) at specified `times`.
#'
#' @return A tibble containing the duration and rate.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input times is increasing positive finite numbers.
#'    \item Validate if input survival is numeric and same length as input times.
#'    \item Validate if input survival is positive, non-increasing, less than or equal to 1 and greater than 0.
#'    \item Create a tibble of inputs times and survival.
#'    \item Calculate the duration, hazard and the rate.
#'    \item Return the duration and rate by \code{s2pwe}
#'  }
#'  }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @export
#'
#' @examples
#' # Example: arbitrary numbers
#' s2pwe(1:9, (9:1) / 10)
#' # Example: lognormal
#' s2pwe(c(1:6, 9), plnorm(c(1:6, 9), meanlog = 0, sdlog = 2, lower.tail = FALSE))
s2pwe <- function(times, survival) {
  # Check input values
  check_positive(times)
  check_increasing(times)

  # Check that survival has same length as times
  if (length(survival) != length(times)) stop("`survival` must be of same length as `times`")

  # Check that survival is positive, non-increasing, less than or equal to 1 and gt 0
  check_positive(survival)
  if (any(diff(survival) > 0)) stop("`survival` must be non-increasing")
  if (survival[1] > 1) stop("`survival` must not be greater than 1")
  if (last_(survival) >= 1) stop("`survival` must have at least one value < 1")

  H <- -log(survival)
  tibble(duration = diff_one(times), rate = diff_one(H) / duration)
}
