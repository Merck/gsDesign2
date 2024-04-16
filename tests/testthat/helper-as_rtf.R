# Helper function to calculate sample size ratio
calculate_ratio <- function(ratio = 1) {
  if (!is.numeric(ratio)) {
    stop("The Ratio value provided must be numeric")
  }
  return(ratio)
}

# Helper function to calculate significance level (alpha)
calculate_alpha <- function(alpha = 0.025) {
  if (!is.numeric(alpha)) {
    stop("Alpha value provided must be numeric")
  }
  return(alpha) # Return the desired significance level
}

# Helper function to calculate beta (type II error rate)
calculate_beta <- function(beta = 0.1) {
  if (!is.numeric(beta)) {
    stop("Beta value provided must be numeric")
  }
  return(beta) # Return the desired beta
}

# Helper function to calculate study duration
calculate_study_duration <- function(duration = 36) {
  if (!is.numeric(duration)) {
    stop("Duration must be numeric")
  }
  return(duration) # Return the desired study duration
}


# Helper function to calculate the enrollment rate
enroll_rate <- function(duration = 18, rate = 20) {
  if (!is.numeric(duration) || !is.numeric(rate)) {
    stop("Duration and rate must be numeric")
  }
  return(gsDesign2::define_enroll_rate(duration, rate))
}

# Helper function to calculate the fail rate
fail_rate <- function(duration = c(4, 100), fail_rate = log(2) / 12, dropout_rate = .001, hr = c(1, .6)) {
  # Check if duration, fail_rate, dropout_rate, and hr are provided and are numeric, if not, throw an error
  if (!is.numeric(duration) || !is.numeric(fail_rate) || !is.numeric(dropout_rate) || !is.numeric(hr)) {
    stop("All parameters must be numeric values.")
  }

  # If the check passes, proceed with defining fail_rate
  return(gsDesign2::define_fail_rate(duration, fail_rate, dropout_rate, hr))
}
