library(dplyr)
library(gsDesign2)

my_enrollRates <- tibble::tibble(
  Stratum = "All",
  duration = 12,
  rate = 500 / 12
)

my_failRates <- tibble::tibble(
  Stratum = "All",
  duration = c(4, 100),
  failRate = log(2) / 15, # median survival 15 month
  hr = c(1, .6),
  dropoutRate = 0.001
)

my_ratio <- 1
my_alpha <- 0.025
my_beta <- 0.2
my_power <- 1 - my_beta

my_analysisTimes <- c(12, 24, 36)

my_fh_test1 <- rbind(
  data.frame(
    rho = 0, gamma = 0, tau = -1,
    test = 1,
    Analysis = 1:3,
    analysisTimes = my_analysisTimes
  ),
  data.frame(
    rho = c(0, 0.5), gamma = 0.5, tau = -1,
    test = 2:3,
    Analysis = 3, analysisTimes = 36
  )
)


my_combo <- gsDesign2::gs_design_combo(
  enrollRates = my_enrollRates,
  failRates = my_failRates,
  fh_test = my_fh_test1,
  alpha = my_alpha,
  beta = my_beta,
  ratio = my_ratio,
  # test.type = 4 non-binding futility bound
  binding = TRUE,
  # alpha spending
  upper = gs_spending_combo,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  # beta spending
  lower = gs_spending_combo,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2))

my_combo %>% summary() %>% as_gt()

#------------------------------------------------------------#
#        Even if we use WLR 
#   the probability0 is small under the null hypothesis
#------------------------------------------------------------#
my_wlr <- gsDesign2::gs_design_wlr(
  enrollRates = my_enrollRates,
  failRates = my_failRates,
  weight = function(x, arm0, arm1){wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 0.5, tau = -1)},
  analysisTimes = my_analysisTimes,
  alpha = my_alpha,
  beta = my_beta,
  ratio = my_ratio,
  # test.type = 4 non-binding futility bound
  binding = TRUE,
  # alpha spending
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  # beta spending
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2))

my_wlr %>% summary() %>% as_gt()

#------------------------------------------------------------#
#        Even if we use AHR 
#   the probability0 is small under the null hypothesis
#------------------------------------------------------------#
my_ahr <- gsDesign2::gs_design_ahr(
  enrollRates = my_enrollRates,
  failRates = my_failRates,
  ratio = my_ratio,
  alpha = my_alpha,
  beta = my_beta,
  analysisTimes = my_analysisTimes,
  # test.type = 4 non-binding futility bound
  binding = FALSE,
  # alpha spending
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  # beta spending
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2))

my_ahr %>% summary() %>% as_gt()