library(dplyr)
library(gsDesign2)
library(lrstat)

my_enrollRates <- tibble::tibble(Stratum = "All",
                                 duration = 12,
                                 rate = 500 / 12)

my_failRates <- tibble::tibble(
  Stratum = "All",
  duration = c(4, 100),
  failRate = log(2) / 15,
  # median survival 15 month
  hr = c(1, .6),
  dropoutRate = 0.001
)

my_ratio <- 1
my_alpha <- 0.025
my_beta <- 0.2
my_power <- 1 - my_beta

my_analysisTimes <- c(12, 24, 36)

info_obj <-
  gs_info_wlr(
    enrollRates = my_enrollRates,
    failRates = my_failRates,
    ratio = my_ratio,
    events = NULL,
    analysisTimes = my_analysisTimes,
    weight = function(x, arm0, arm1){wlr_weight_fh(x, arm0, arm1, rho = 0, gamma = 0.5, tau = -1)},
    approx = "asymptotic"
  )

lrsamplesize(
  beta = my_beta,
  kMax = length(my_analysisTimes),
  informationRates =  info_obj$info0/info_obj$info0[nrow(info_obj)],
  alpha = my_alpha,
  typeAlphaSpending = "sfOF",
  typeBetaSpending = "sfOF",
  #enrollment
  allocationRatioPlanned = my_ratio,
  accrualDuration = max(my_enrollRates$duration),
  accrualTime = c(0, my_enrollRates$duration),
  accrualIntensity = c(my_enrollRates$rate,0),
  #hazard rates
  piecewiseSurvivalTime = c(0, my_failRates$duration[-nrow(my_failRates)]),
  lambda1 = with(my_failRates, failRate * hr),
  lambda2 = my_failRates$failRate,
  #dropout; the two packages are flexible in different ways in this department
  gamma1 = my_failRates$dropoutRate[1],
  gamma2 = my_failRates$dropoutRate[1],
  #WLR test
  rho1 = 0,
  rho2 = 0.5,
  #
  followupTime = 36-12
)


my_wlr <- gsDesign2::gs_design_wlr(
  enrollRates = my_enrollRates,
  failRates = my_failRates,
  weight = function(x, arm0, arm1) {
    wlr_weight_fh(x,
                  arm0,
                  arm1,
                  rho = 0,
                  gamma = 0.5,
                  tau = -1)
  },
  analysisTimes = my_analysisTimes,
  alpha = my_alpha,
  beta = my_beta,
  ratio = my_ratio,
  # test.type = 4 non-binding futility bound
  binding = FALSE,
  # alpha spending
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  # beta spending
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2)
)

my_wlr %>% summary() %>% as_gt()

