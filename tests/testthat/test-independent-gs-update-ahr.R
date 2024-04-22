library(gsDesign2)
library(gt)
library(dplyr)
library(simtrial)

# Function to get cut data and statistical information combined with AHR for an analysis
get_blinded_ahr <- function(observed_data, analysis_event = 0, analysis_date, intervals, hr, ratio){
  # Cut the data by the max of analysis_date and event timing
  if (!is.null(analysis_event)){cut_date <- observed_data |> get_cut_date_by_event(analysis_event)
  }else cut_date <- 0
  analysis_date <- max(analysis_date, cut_date)
  cut_data <- observed_data |> simtrial::cut_data_by_date(analysis_date)
  info <- gsDesign2::ahr_blinded(
    surv = survival::Surv(time = cut_data$tte, event = cut_data$event),
    intervals = intervals,
    hr = hr,
    ratio = ratio
  )
  # Return a row of data frame and the cut data
  return(list(info = info, cut_data = cut_data))
}

# Example 1

# Define the design
x <- gs_design_ahr(enroll_rate = define_enroll_rate(18, 1, stratum = "All"),
                   fail_rate = define_fail_rate(duration = 4, fail_rate = log(2) / 11,
                                                dropout_rate = 0.001, hr = c(1, .6), stratum = "All"),
                   alpha = 0.025, beta = 0.2,
                   analysis_time = c(12, 24, 36),
                   upper = gs_spending_bound,
                   lower = gs_spending_bound,
                   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL, timing = NULL),
                   lpar = list(sf = gsDesign::sfLDPocock, total_spend = 0.2, param = NULL, timing = NULL),

) |> to_integer()

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
  xx <- get_blinded_ahr(observed_data = observed_data, analysis_event = 0,
                                   analysis_date = x$analysis$time[i],
                                   intervals = cumsum(x$fail_rate$duration),
                                   hr = x$fail_rate$hr, ratio = x$input$ratio)
  ahr_info <- rbind(ahr_info, xx$ahr)
  data_list[[i]] <- xx$cut_data
}
y <- gs_update_ahr(x, alpha = x$input$alpha, observed_data = data_list,
                   ia_alpha_spending = "actual_info_frac", fa_alpha_spending = "full_alpha")




test_that("check bounds for gs_update_ahr", {
  expect_equal(
    expected_accrual(
      time = 0:30,
      enroll_rate = define_enroll_rate(
        duration = c(3, 13, 18),
        rate = c(5, 20, 8)
      )
    ),
    test_eAccrual(
      x = 0:30,
      enroll_rate = define_enroll_rate(
        duration = c(3, 13, 18),
        rate = c(5, 20, 8)
      )
    )
  )
})

te