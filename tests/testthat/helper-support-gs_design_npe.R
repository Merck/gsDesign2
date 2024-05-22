params_gs_design_npe <- function() {
  list(
    K = 3,
    timing = c(.45, .8, 1),
    sfu = gsDesign::sfPower,
    sfupar = 4,
    sfl = gsDesign::sfHSD,
    sflpar = 2,
    delta = .2,
    alpha = .02,
    beta = .15
  )
}
