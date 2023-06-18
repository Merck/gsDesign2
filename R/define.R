#' Define Enrollment Rate
#'
#' @param duration a numeric vector of piecewise study duration interval.
#' @param rate a numeric vector of enrollment rate in each `duration`.
#'
#' @examples
#'
#' define_enroll_rate(
#'   duration = c(2, 2, 10),
#'   rate = c(3, 6, 9)
#' )
#'
#' @export
define_enroll_rate <- function(
    duration,
    rate) {
  # length of variables
  l <- unique(c(
    length(duration),
    length(rate)
  ))

  if (length(l) > 1) {
    stop("length of duration and rate should be the same")
  }

  check_args(duration, length = l, type = c("numeric", "integer"))
  check_args(rate, length = l, type = c("numeric", "integer"))

  # the rate is positive numbers
  if (any(rate < 0)) {
    stop("The enrollment rate, `rate`, can not be negative number.")
  }
  
  df <- data.frame(
    duration = duration,
    rate = rate
  )

  class(df) <- c("enroll_rate", class(df))

  df
}
