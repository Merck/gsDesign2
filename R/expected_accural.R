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

#' Piecewise constant expected accrual
#'
#' Computes the expected cumulative enrollment (accrual)
#' given a set of piecewise constant enrollment rates and times.
#'
#' @inheritParams ahr
#' @param time Times at which enrollment is to be computed.
#'
#' @return A vector with expected cumulative enrollment for the specified `times`.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if input x is a vector of strictly increasing non-negative numeric elements.
#'    \item Validate if input enrollment rate is of type data.frame.
#'    \item Validate if input enrollment rate contains duration column.
#'    \item Validate if input enrollment rate contains rate column.
#'    \item Validate if rate in input enrollment rate is non-negative with at least one positive rate.
#'    \item Convert rates to step function.
#'    \item Add times where rates change to enrollment rates.
#'    \item Make a tibble of the input time points x, duration, enrollment rates at points, and
#'    expected accrual.
#'    \item Extract the expected cumulative or survival enrollment.
#'    \item Return \code{expected_accrual}
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @export
#'
#' @examples
#' library(tibble)
#'
#' # Example 1: default
#' expected_accrual()
#'
#' # Example 2: unstratified design
#' expected_accrual(
#'   time = c(5, 10, 20),
#'   enroll_rate = define_enroll_rate(
#'     duration = c(3, 3, 18),
#'     rate = c(5, 10, 20)
#'   )
#' )
#'
#' expected_accrual(
#'   time = c(5, 10, 20),
#'   enroll_rate = define_enroll_rate(
#'     duration = c(3, 3, 18),
#'     rate = c(5, 10, 20),
#'   )
#' )
#'
#' # Example 3: stratified design
#' expected_accrual(
#'   time = c(24, 30, 40),
#'   enroll_rate = define_enroll_rate(
#'     stratum = c("subgroup", "complement"),
#'     duration = c(33, 33),
#'     rate = c(30, 30)
#'   )
#' )
#'
#' # Example 4: expected accrual over time
#' # Scenario 4.1: for the enrollment in the first 3 months,
#' # it is exactly 3 * 5 = 15.
#' expected_accrual(
#'   time = 3,
#'   enroll_rate = define_enroll_rate(duration = c(3, 3, 18), rate = c(5, 10, 20))
#' )
#'
#' # Scenario 4.2: for the enrollment in the first 6 months,
#' # it is exactly 3 * 5 + 3 * 10 = 45.
#' expected_accrual(
#'   time = 6,
#'   enroll_rate = define_enroll_rate(duration = c(3, 3, 18), rate = c(5, 10, 20))
#' )
#'
#' # Scenario 4.3: for the enrollment in the first 24 months,
#' # it is exactly 3 * 5 + 3 * 10 + 18 * 20 = 405.
#' expected_accrual(
#'   time = 24,
#'   enroll_rate = define_enroll_rate(duration = c(3, 3, 18), rate = c(5, 10, 20))
#' )
#'
#' # Scenario 4.4: for the enrollment after 24 months,
#' # it is the same as that from the 24 months, since the enrollment is stopped.
#' expected_accrual(
#'   time = 25,
#'   enroll_rate = define_enroll_rate(duration = c(3, 3, 18), rate = c(5, 10, 20))
#' )
#'
#' # Instead of compute the enrolled subjects one time point by one time point,
#' # we can also compute it once.
#' expected_accrual(
#'   time = c(3, 6, 24, 25),
#'   enroll_rate = define_enroll_rate(duration = c(3, 3, 18), rate = c(5, 10, 20))
#' )
expected_accrual <- function(time = 0:24,
                             enroll_rate = define_enroll_rate(duration = c(3, 3, 18), rate = c(5, 10, 20))) {
  # check input value
  # check input enrollment rate assumptions
  if (!is.numeric(time)) {
    stop("gsDesign2: time in `expected_accrual()` must be a strictly increasing non-negative numeric vector!")
  }
  if (!min(time) >= 0) {
    stop("gsDesign2: time in `expected_accrual()` must be a strictly increasing non-negative numeric vector!")
  }
  if (!min(lead(time, default = max(time) + 1) - time) > 0) {
    stop("gsDesign2: t in `expected_accrual()` must be a strictly increasing non-negative numeric vector!")
  }

  # check enrollment rate assumptions
  check_enroll_rate(enroll_rate)

  # check if it is stratified design
  if ("stratum" %in% names(enroll_rate)) {
    n_strata <- length(unique(enroll_rate$stratum))
  } else {
    n_strata <- 1
  }

  # convert rates to step function
  if (n_strata == 1) {
    ratefn <- stepfun(
      x = cumsum(enroll_rate$duration),
      y = c(enroll_rate$rate, 0),
      right = TRUE
    )
  } else {
    ratefn <- lapply(unique(enroll_rate$stratum),
      FUN = function(s) {
        stepfun(
          x = cumsum((enroll_rate %>% filter(stratum == s))$duration),
          y = c((enroll_rate %>% filter(stratum == s))$rate, 0),
          right = TRUE
        )
      }
    )
  }

  # add times where rates change to enroll_rate
  if (n_strata == 1) {
    xvals <- sort(unique(c(time, cumsum(enroll_rate$duration))))
  } else {
    xvals <- lapply(unique(enroll_rate$stratum),
      FUN = function(s) {
        sort(unique(c(time, cumsum((enroll_rate %>% filter(stratum == s))$duration))))
      }
    )
  }

  # make a tibble
  if (n_strata == 1) {
    xx <- tibble(
      x = xvals,
      duration = xvals - fastlag(xvals, first = 0),
      rate = ratefn(xvals), # enrollment rates at points (right continuous)
      eAccrual = cumsum(rate * duration) # expected accrual
    )
  } else {
    xx <- lapply(1:n_strata,
      FUN = function(i) {
        tibble(
          x = xvals[[i]],
          duration = xvals[[i]] - fastlag(xvals[[i]], first = 0),
          rate = ratefn[[i]](xvals[[i]]), # enrollment rates at points (right continuous)
          eAccrual = cumsum(rate * duration) # expected accrual
        )
      }
    )
  }



  # return survival or CDF
  if (n_strata == 1) {
    ind <- !is.na(match(xx$x, time))
    ans <- as.numeric(xx$eAccrual[ind])
  } else {
    ind <- lapply(1:n_strata,
      FUN = function(i) {
        !is.na(match(xx[[i]]$x, time))
      }
    )
    ans <- lapply(1:n_strata,
      FUN = function(i) {
        as.numeric(xx[[i]]$eAccrual[ind[[i]]])
      }
    )
    ans <- Reduce(`+`, ans)
  }

  return(ans)
}
