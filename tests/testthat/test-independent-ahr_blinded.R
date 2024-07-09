test_that("hr vector must be a vector of positive numbers", {
    expect_error(ahr_blinded(hr = c(1, -2, 3)), "ahr_blinded: hr must be a vector of positive numbers.")
    expect_error(ahr_blinded(hr = "abc"), "ahr_blinded: hr must be a vector of positive numbers.")
})

test_that("Piecewise model hr and intervals must be aligned", {
    expect_error(ahr_blinded(hr = c(1, 2, 4), intervals = 3), "ahr_blinded: the piecewise model specified hr and intervals are not aligned.")
    expect_error(ahr_blinded(hr = c(1, 3), intervals = c(3, 4)), "ahr_blinded: the piecewise model specified hr and intervals are not aligned.")
})

test_that("Correct computation of blinded AHR and information adjustment", {
    surv <- survival::Surv(simtrial::ex2_delayed_effect$month, event = simtrial::ex2_delayed_effect$evntd)
    intervals <- c(3, Inf)
    hr <- c(1, 0.6)
    ratio <- 1
    event<- simtrial::fit_pwexp(surv, intervals)[, 3]

    expected_event <- sum(surv[, "status"])
    expected_theta <- -sum(log(hr[1:length(event)]) * event) / sum(event)
    expected_ahr <- exp(-(-sum(log(hr[1:length(event)]) * event) / sum(event)))
    expected_info0 <- sum(surv[, "status"]) * (1 - ratio / (1 + ratio)) * (ratio / (1 + ratio))

    result <- ahr_blinded(surv = surv, intervals = intervals, hr = hr, ratio = ratio)

    expect_equal(result$event, expected_event)
    expect_equal(result$ahr, expected_ahr, tolerance = 0.001)
    expect_equal(result$theta, expected_theta, tolerance = 0.001)
    expect_equal(result$info0, expected_info0)
})
