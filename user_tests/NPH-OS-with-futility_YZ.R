#################################################################
##########OS Design under NPH                      ##############
#################################################################
library(ggplot2)
library(gsDesign)
# PLEASE RE-INSTALL GSDESIGN2 BY
# remotes::install_github("Merck/gsDesign2")
library(gsDesign2) 
#install.packages('mkgsdesign2', repos = "https://rspm.merck.com/bardsdev/latest")
#library(mkgsdesign2)
#library(gsdmvn)
library(dplyr)
#library(knitr)
#library(kableExtra)
library(mvtnorm)
library(gt)

pwexp2=function(m1,m2,cp,t)
{
  if (t<=cp)
  {
    return (exp(-log(2)/m1*t))
  } else {
    return (exp(-log(2)/m1*cp-log(2)/m2*(t-cp)))
  }
}
getm_pwexp2=function(m1,m2,cp,t){return(pwexp2(m1,m2,cp,t)-0.5)}
pwexp3=function(m1,m2,m3,cp1,cp2,t)
{
  if (t<=cp1)
  {
    return (exp(-log(2)/m1*t))
  } else if (t<=cp2) {
    return (exp(-log(2)/m1*cp1-log(2)/m2*(t-cp1)))
  } else {
    return (exp(-log(2)/m1*cp1-log(2)/m2*(cp2-cp1)-log(2)/m3*(t-cp2)))
  }
}
getm_pwexp3=function(m1,m2,m3,cp1,cp2,t){return(pwexp3(m1,m2,m3,cp1,cp2,t)-0.5)}
#######OS PH##########

#gamma=c(13,31,40,32)
#R=c(2,2,4.3,5)
#R=c(2,2,5.05,5)
gamma=c(6,15,27,35,41,42,42,42,38,37,34,31,27,33)
R=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)

#piecewise exponential control arm distribution
#m gives median of each piece
m<-c(40.65719,8.872054)
#s is the change point
s<-5
a=0.025
hr<-0.7
eta<- -log(0.98)/12
power<-1-0.0865
timing<-0.75

#####Proportional hazards: original design
os_f <- gsSurv ( k = 2 , test.type = 4 , alpha = a , beta = 1-power , astar = 0 , timing = timing ,
                 sfu=sfLDOF,sfupar = c(0), sfl = sfHSD , sflpar = c( -15 ) ,
                 lambdaC = log(2)/m , hr = hr , hr0 = 1 , eta = eta ,
                 gamma = gamma , R = R , S = s , ratio = 1
)
gsBoundSummary(os_f)



############################################
######OS: Non-proportional hazards#########
############################################
#option 1: design under NPH with OS as only primary endpoint, with futility of OS at IA 
nphpt<-4 #first 4 mon HR =1
hr0<-1
nprd<-length(gamma)
design<-os_f

gethr1<-function(hr1,  hr0,hrtarget){
  enrollRates <- tibble::tibble(Stratum='All',
                                duration=rep(design$R,1),
                                rate=design$gamma   )
  
  
  failRates<- tibble::tibble(Stratum='All',
                             period=1:3,
                             duration=c(nphpt,s-nphpt,100),
                             failRate=c(log(2)/m[1],log(2)/m),
                             hr=c(hr0,hr1,hr1),
                             dropoutRate=eta)
  
  res<-AHR(enrollRates=enrollRates,
           failRates=failRates,
           totalDuration= max(design$T)
  )$AHR-hrtarget
  return(res)
}

#find out the 2nd piece HR if AHR at intended FA timing is ~0.7
hr1<-uniroot(gethr1,c(0.00001,1),hr0=1,hrtarget=0.7)$root
hr1#the 2nd piece of the piecewise HR after the initial delay

######################################
#calculate median from control 
uniroot(getm_pwexp2,c(0,20),m1=m[1],m2=m[2],cp=s)$root
#calculate median from treatment
uniroot(getm_pwexp3,c(0,20),m1=m[1]/hr0,m2=m[1]/hr1,m3=m[2]/hr1,cp1=nphpt,cp2=s)$root

enrollRates <- tibble::tibble(Stratum='All',
                              duration=rep(design$R,1),
                              rate=design$gamma   )


failRates<- tibble::tibble(Stratum='All',
                           period=1:3,
                           duration=c(nphpt,s-nphpt,100),
                           failRate=c(log(2)/m[1],log(2)/m),
                           hr=c(hr0,hr1,hr1),
                           dropoutRate=eta)

ahr<-AHR(enrollRates=enrollRates,
         failRates=failRates,
         totalDuration=design$T)  #fix duration and find averhr and Events
ahr


design_nph_t=gsDesign2::gs_power_ahr(
  enrollRates = enrollRates,
  failRates = failRates,
  ratio = 1,
  events = NULL,
  analysisTimes = design$T,
  binding = TRUE,
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = design$alpha, param = NULL, timing = NULL, theta=0),
  #upar = list(par = list(sf = gsDesign::sfLDOF, total_spend = design$alpha, param = NULL, timing = NULL, theta=0)),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfHSD,total_spend=design$beta, param=-15,timing=NULL,theta=0,h1_spending = TRUE),
  #lpar = list(par = list(sf = gsDesign::sfHSD,total_spend=design$beta, param=-15,timing=NULL,theta=0,h1_spending = TRUE)),
  #lpar = list(par = list(sf = gsDesign::sfHSD,total_spend=design$beta, param=-15,timing=NULL,theta=0,h1_spending = FALSE)),
  test_upper = TRUE,
  test_lower = TRUE,
  r = 18,
  tol = 1e-06
)

design_nph_t %>% summary() %>% as_gt()
####################################################
# Below please find Yujie's justifications         #
# Keaven, please correct me if I am anywhere wrong #
####################################################
# The statistical informaion of the above design is 
# From the output, you will find the theta value 
# (measuring the treatment effect)is not relatively large
x <- gs_info_ahr(enrollRates = enrollRates, failRates = failRates,
                 ratio = 1, events = NULL, analysisTimes = design$T)
x %>% gt()

cat("The theta (treatment effect) at IA and FA is", x$theta, ",\n")

# Under the H0, 
# the probability is calculated under 0 treatment effect, i.e., theta = 0
y_H0 <- gsDesign2::gs_power_npe(theta = 0, 
                                info = x$info0, info0 = x$info0, info_scale = 2,
                                upper = gs_spending_bound, 
                                lower = gs_spending_bound, 
                                upar = list(sf = gsDesign::sfLDOF, total_spend = design$alpha, param = NULL, timing = NULL, theta=0), 
                                lpar = list(sf = gsDesign::sfHSD,total_spend=design$beta, param=-15,timing=NULL,theta=0,h1_spending = TRUE),
                                test_upper = TRUE,
                                test_lower = TRUE,
                                binding = TRUE, r = 18, tol = 1e-6)
# H0: efficacy bound
cat("You will find the efficacy probability at IA, FA is", 
    (y_H0 %>% filter(Bound == "Upper"))$Probability,
    "which match with your efficacy spending function, i.e.",
    gsDesign::sfLDOF(alpha = design$alpha, param = NULL, t = x$info0 / max(x$info0))$spend, ".\n")
# H0: futility bound
cat("You will find the futility probability at IA, FA is", 
    (y_H0 %>% filter(Bound == "Lower"))$Probability, 
    "I know this is lower than expected. And after a careful check, I can't find a problem with the code.\n")


# Under the H1,
# the probability is calculated under the treatment effect x$theta
y_H1 <- gsDesign2::gs_power_npe(theta = x$theta, 
                                info = x$info, info0 = x$info0, info_scale = 2,
                                upper = gs_spending_bound, 
                                lower = gs_spending_bound, 
                                upar = list(sf = gsDesign::sfLDOF, total_spend = design$alpha, param = NULL, timing = NULL, theta=0), 
                                lpar = list(sf = gsDesign::sfHSD,total_spend=design$beta, param=-15,timing=NULL,theta=0,h1_spending = TRUE),
                                test_upper = TRUE,
                                test_lower = TRUE,
                                binding = TRUE, r = 18, tol = 1e-6) 
# H1: futility bound
cat("You will find the futility probability at IA, FA is", 
    (y_H1 %>% filter(Bound == "Lower"))$Probability,
    "which almost match with your futility spending function, i.e.",
    gsDesign::sfHSD(alpha = design$beta, param = -15, t = x$info0 / max(x$info0))$spend, ".\n")
# H0: efficacy bound
cat("You will find the futility probability at IA, FA is", 
    (y_H1 %>% filter(Bound == "Upper"))$Probability, ".\n")







