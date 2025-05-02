library(gsDesign2)
library(gt)
library(dplyr)
library(simtrial)
library(testthat)

# Function to get cut data and statistical information combined with AHR for an analysis
get_blinded_ahr <- function(observed_data, analysis_event = NULL, analysis_date, intervals, hr, ratio) {
  # Get the cut date by analysis_event if not NULL
  if (!is.null(analysis_event)) {
    cut_date <- observed_data |> simtrial::get_cut_date_by_event(analysis_event)
  } else {
    cut_date <- 0
  }
  # Cut the data by the max of analysis_date and event timing
  analysis_date <- max(analysis_date, cut_date)
  # Cut the data
  cut_data <- observed_data |> simtrial::cut_data_by_date(analysis_date)
  # Calculate the blinded estimation of AHR, theta and info0
  info <- gsDesign2::ahr_blinded(
    surv = survival::Surv(time = cut_data$tte, event = cut_data$event),
    intervals = intervals,
    hr = hr,
    ratio = ratio
  )
  # Return a row of data frame and the cut data
  return(list(info = info, cut_data = cut_data))
}

# Example 1 ----

# Define the design
x <- gs_design_ahr(enroll_rate = define_enroll_rate(18, 1, stratum = "All"),
                   fail_rate = define_fail_rate(duration = c(4, Inf), fail_rate = log(2) / 11,
                                                dropout_rate = 0.001, hr = c(1, .6), stratum = "All"),
                   alpha = 0.025, beta = 0.2,
                   analysis_time = c(12, 24, 36),
                   info_scale = "h0_info",
                   upper = gs_spending_bound,
                   lower = gs_spending_bound,
                   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
                   lpar = list(sf = gsDesign::sfLDPocock, total_spend = 0.2, param = NULL, timing = NULL)) |>
  to_integer()

set.seed(1234)
observed_data <- simtrial::sim_pw_surv(
  n = max(x$analysis$n),
  stratum = data.frame(stratum = "All", p = 1),
  block = c(rep("control", 2), rep("experimental", 2)),
  enroll_rate = x$enroll_rate,
  fail_rate = (x$fail_rate |> simtrial::to_sim_pw_surv())$fail_rate,
  dropout_rate = (x$fail_rate |> simtrial::to_sim_pw_surv())$dropout_rate
)

# Make a list of length x$analysis$time
data_list <- list()
ahr_info <- NULL
for (i in seq_along(x$analysis$time)){
  xx <- get_blinded_ahr(observed_data = observed_data,
                        analysis_event = NULL,
                        analysis_date = x$analysis$time[i],
                        intervals = cumsum(x$fail_rate$duration),
                        hr = x$fail_rate$hr,
                        ratio = x$input$ratio)
  ahr_info <- rbind(ahr_info, xx$info)
  data_list[[i]] <- xx$cut_data
}

delay_event_ia1 <- sum((observed_data %>% filter(fail_time <= 4, cte <= x$analysis$time[1]))$fail)
delay_event_ia2 <- sum((observed_data %>% filter(fail_time <= 4, cte <= x$analysis$time[2]))$fail)
delay_event_fa <- sum((observed_data %>% filter(fail_time <= 4, cte <= x$analysis$time[3]))$fail)

observed_event_ia1 <- sum(data_list[[1]]$event)
observed_event_ia2 <- sum(data_list[[2]]$event)
observed_event_fa <- sum(data_list[[3]]$event)

planned_event_ia1 <- x$analysis$event[1]
planned_event_ia2 <- x$analysis$event[2]
planned_event_fa <- x$analysis$event[3]

ustime <- c(c(observed_event_ia1, observed_event_ia2) / planned_event_fa, 1)
y <- gs_update_ahr(x, alpha = x$input$alpha,
                   event_tbl = data.frame(analysis =  c(1, 1, 2, 2, 3, 3),
                                          event = c(delay_event_ia1, observed_event_ia1 - delay_event_ia1,
                                                    delay_event_ia2, observed_event_ia2 - delay_event_ia2,
                                                    delay_event_fa, observed_event_fa - delay_event_fa)),
                   ustime = ustime,
                   lstime = ustime)

test_that("Ex1: blinded ahr computed correctly", {
  expect_equal(y$analysis$ahr, ahr_info$ahr)
})

# Double programming for futility bound
upar_update <- x$input$upar
lpar_update <- x$input$lpar
upar_update$timing <- ustime
lpar_update$timing <- ustime

yb <- gs_power_npe(theta = y$analysis$theta,
                   info0 = y$analysis$info0, info_scale = "h0_info",
                   info = y$analysis$info,
                   upper = x$input$upper, lower = x$input$lower,
                   upar = upar_update, lpar = lpar_update,
                   binding = x$input$binding, r = x$input$r
                   )
test_that("Ex1: blinded theta computed correctly", {
  expect_equal(y$analysis$theta, ahr_info$theta)
})

test_that("Ex1: blinded information computed correctly", {
  expect_equal(y$analysis$info0, ahr_info$info0)
})

lower_bound <- (yb |> filter(bound == "lower"))$z
test_that("Ex1: futility bound computed correctly", {
  expect_equal((y$bound |> filter(bound == "lower"))$z, lower_bound)
})

