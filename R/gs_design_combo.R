#  Copyright (c) 2021 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co., Inc., Kenilworth, NJ, USA.
#
#  This file is part of the gsdmvn program.
#
#  gsdmvn is free software: you can redistribute it and/or modify
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

#' Group sequential design using MaxCombo test under non-proportional hazards
#' @importFrom tibble tibble
#' 
#' @inheritParams gs_design_ahr
#' @inheritParams pmvnorm_combo
#' @param fh_test a data frame to summarize the test in each analysis.
#'                Refer examples for its data structure.
#' @param n_upper_bound a numeric value of upper limit of sample size
#' @importFrom mvtnorm GenzBretz
#' 
#' @export
#' 
#' @examples
#' # The example is slow to run
#' library(dplyr)
#' library(mvtnorm)
#' library(gsDesign)
#' library(tibble)
#' 
#' enrollRates <- tibble(
#'   Stratum = "All", 
#'   duration = 12, 
#'   rate = 500/12)
#'   
#' failRates <- tibble(
#'   Stratum = "All",
#'   duration = c(4, 100),
#'   failRate = log(2) / 15,  # median survival 15 month
#'   hr = c(1, .6),
#'   dropoutRate = 0.001)
#'   
#' fh_test <- rbind( 
#'   data.frame(rho = 0, gamma = 0, tau = -1,
#'              test = 1, Analysis = 1:3, analysisTimes = c(12, 24, 36)),
#'   data.frame(rho = c(0, 0.5), gamma = 0.5, tau = -1,
#'              test = 2:3, Analysis = 3, analysisTimes = 36))
#' 
#' x <- gsSurv( 
#'   k = 3 , 
#'   test.type = 4 , 
#'   alpha = 0.025 ,
#'   beta = 0.2 , 
#'   astar = 0 , 
#'   timing = 1,
#'   sfu = sfLDOF , 
#'   sfupar = 0, 
#'   sfl = sfLDOF ,
#'   sflpar = 0, 
#'   lambdaC = 0.1,
#'   hr = 0.6, 
#'   hr0 = 1, 
#'   eta = 0.01,
#'   gamma = 10,
#'   R = 12, 
#'   S = NULL,
#'   T = 36, 
#'   minfup = 24, 
#'   ratio = 1)
#' 
#' # -------------------------#
#' #       example 1          #
#' # ------------------------ #
#' \dontrun{
#' # User defined boundary
#' gs_design_combo(
#'   enrollRates,
#'   failRates,
#'   fh_test,
#'   alpha = 0.025, beta = 0.2,
#'   ratio = 1,
#'   binding = FALSE,       
#'   upar = x$upper$bound,
#'   lpar = x$lower$bound)
#' }
#' 
#' # -------------------------#
#' #       example 2          #
#' # ------------------------ #
#' # Boundary derived by spending function
#' gs_design_combo(
#'   enrollRates,
#'   failRates,
#'   fh_test,
#'   alpha = 0.025, 
#'   beta = 0.2,
#'   ratio = 1,
#'   binding = FALSE,                 
#'   upper = gs_spending_combo,
#'   upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),   # alpha spending
#'   lower = gs_spending_combo,
#'   lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.2),     # beta spending
#' )

gs_design_combo <- function(enrollRates = tibble(Stratum = "All", 
                                                 duration = 12, 
                                                 rate = 500/12),
                            failRates = tibble(Stratum = "All", 
                                               duration = c(4, 100), 
                                               failRate = log(2) / 15,  
                                               hr = c(1, .6), 
                                               dropoutRate = 0.001),
                            fh_test = rbind(data.frame(rho = 0, gamma = 0, tau = -1, test = 1, Analysis = 1:3, analysisTimes = c(12, 24, 36)),
                                            data.frame(rho = c(0, 0.5), gamma = 0.5, tau = -1, test = 2:3, Analysis = 3, analysisTimes = 36)),
                            ratio = 1,
                            alpha = 0.025,
                            beta = 0.2,
                            binding = FALSE,
                            upper = gs_b,
                            upar = c(3, 2, 1),
                            lower = gs_b,
                            lpar = c(-1, 0, 1),
                            algorithm = mvtnorm::GenzBretz(maxpts = 1e5, abseps = 1e-5),
                            n_upper_bound = 1e3,
                            ...){
  
  # --------------------------------------------- #
  #     check input values                        #
  # --------------------------------------------- #
  # Currently only support user defined lower and upper bound
  #stopifnot( identical(upper, gs_b) | identical(upper, gs_spending_combo) )
  #stopifnot( identical(lower, gs_b) | identical(lower, gs_spending_combo) )
  
  # --------------------------------------------- #
  #     get the number of analysis/test           #
  # --------------------------------------------- #
  n_analysis <- length(unique(fh_test$Analysis))
  n_test <- max(fh_test$test)
  
  # --------------------------------------------- #
  #     obtain utilities                          #
  # --------------------------------------------- #
  utility <- gs_utility_combo(enrollRates = enrollRates,
                              failRates = failRates,
                              fh_test = fh_test,
                              ratio = ratio,
                              algorithm = algorithm, 
                              ...)
  
  info     <- utility$info_all
  info_fh  <- utility$info
  theta_fh <- utility$theta
  corr_fh  <- utility$corr
  
  # Information Fraction
  if(n_analysis == 1){
    min_info_frac <- 1
  }else{
    info_frac <- tapply(info$info0, info$test, function(x) x / max(x))
    min_info_frac <- apply(do.call(rbind, info_frac), 2, min)
  }
  
  # Function to calculate power
  foo <- function(n, beta, ...){
    
    # Probability Cross Boundary
    prob <- gs_prob_combo(upper_bound = bound$upper,
                          lower_bound = bound$lower,
                          analysis = info_fh$Analysis,
                          theta = theta_fh * sqrt(n),
                          corr = corr_fh,
                          algorithm = algorithm, ...)
    
    max(subset(prob, Bound == "Upper")$Probability) - (1 - beta)
  }
  
  # Find sample size and bound
  n <- max(info$N)
  n0 <- 0
  while( (abs(n - n0)) > 1e-2){
    # print(n)
    n0 <- n
    
    # Obtain spending function
    bound <- gs_bound(alpha = upper(upar, min_info_frac),
                      beta = lower(lpar, min_info_frac),
                      analysis = info_fh$Analysis,
                      theta = theta_fh * sqrt(n),
                      corr = corr_fh,
                      binding_lower_bound = binding,
                      algorithm = algorithm,
                      alpha_bound = identical(upper, gs_b),
                      beta_bound = identical(lower, gs_b),
                      ...)
    
    
    n <- uniroot(foo, c(1, n_upper_bound), extendInt = "yes", beta = beta, ...)$root
    
  }
  
  # Probability Cross Boundary
  prob <- gs_prob_combo(upper_bound = bound$upper,
                        lower_bound = bound$lower,
                        analysis = info_fh$Analysis,
                        theta = theta_fh * sqrt(n),
                        corr = corr_fh,
                        algorithm = algorithm, ...)
  
  # Probability Cross Boundary under Null
  prob_null <- gs_prob_combo(upper_bound = bound$upper,
                             lower_bound = if(binding){bound$lower}else{rep(-Inf, nrow(bound))},
                             analysis = info_fh$Analysis,
                             theta = rep(0, nrow(info_fh)),
                             corr = corr_fh,
                             algorithm = algorithm, ...)
  
  # if(binding == FALSE){
  #   prob_null$Probability[prob_null$Bound == "Lower"] <- NA
  # }
  
  prob$Probability_Null <- prob_null$Probability
  
  # Prepare output
  db <- merge(
    data.frame(Analysis = 1:(nrow(prob)/2), prob, Z = unlist(bound)),
    info_fh %>% 
      tibble::as_tibble() %>% 
      select(Analysis, Time, N, Events) %>% 
      unique()) %>% 
    # update sample size and events
    mutate(
      Events = Events * n / max(N),
      N = N * n / max(N)) %>% 
    # arrange the dataset by Upper bound first and then Lower bound
    arrange(Analysis, desc(Bound))
  
  
  # out <- db[order(db$Bound, decreasing = TRUE), c("Analysis", "Bound", "Time", "N", "Events", "Z", "Probability", "Probability_Null")]
  out <- db %>% 
    dplyr::select(Analysis, Bound, Time, N, Events, Z, Probability, Probability_Null) %>% 
    dplyr::rename(Probability0 = Probability_Null) %>% 
    dplyr::mutate(`Nominal p` = pnorm(Z * (-1)))
  
  
  # --------------------------------------------- #
  #     get bounds to output                      #
  # --------------------------------------------- #
  bounds <- out %>% 
    #rbind(out_H1, out_H0) %>% 
    select(Analysis, Bound, Probability, Probability0, Z, `Nominal p`) %>%  
    arrange(Analysis,desc(Bound))  
  
  # --------------------------------------------- #
  #     get analysis summary to output            #
  # --------------------------------------------- #
  # check if rho, gamma = 0 is included in fh_test
  tmp <- fh_test %>% 
    filter(rho == 0 & gamma == 0 & tau == -1) %>% 
    select(test) %>% 
    unlist() %>% 
    as.numeric() %>% 
    unique()
  if(length(tmp) != 0){
    AHR_dis <- utility$info_all %>% 
      filter(test == tmp) %>% 
      select(AHR) %>% 
      unlist() %>% 
      as.numeric()
  }else{
    AHR_dis <- gs_info_wlr(
      enrollRates, 
      failRates, 
      ratio, 
      events = unique(utility$info_all$Events), 
      analysisTimes = unique(utility$info_all$Time), 
      weight = eval(parse(text = get_combo_weight(rho = 0, gamma = 0, tau = -1))))$AHR
  }
  
  analysis <- utility$info_all %>% 
    select(Analysis, test, Time, N, Events)%>% 
    mutate(theta = utility$info_all$theta,
           EF = Events/tapply(Events, test, function(x) max(x)) %>% unlist() %>% as.numeric()) %>% 
    select(Analysis, Time, N, Events, EF) %>% 
    unique() %>% 
    mutate(AHR = AHR_dis) %>% 
    mutate(N = N *n / max(info_fh$N),
           Events = Events * n / max(info_fh$N)) %>%  
    arrange(Analysis)
  
  # --------------------------------------------- #
  #     output                                    #
  # --------------------------------------------- #
  message("The AHR reported in the `analysis` table is under the log-rank test.")
  output <- list(
    enrollRates = enrollRates %>% mutate(rate = rate * max(analysis$N) / sum(rate * duration) ),
    failRates = failRates,
    bounds = bounds, 
    analysis = analysis)
  class(output) <- c("combo", "gs_design", class(output))
  return(output)
}
