assert("gs_b() returns values as expected", {
  IF <- c(.6, .8, 1)
  par <- gsDesign::gsDesign(
    alpha = .02,
    k = length(IF),
    test.type = 1,
    sfu = gsDesign::sfLDOF,
    timing = IF
  )$upper$bound
  (all_equal(par, gs_b(par)))

  par <- 1:10
  k <- 5
  (all_equal(par[5], gs_b(par, k = k)))
})

assert("gs_b() returns NA if the number of interim analysis is larger than the length of par", {
  IF <- c(.8, 1)
  par <- gsDesign::gsDesign(
    alpha = .025,
    k = length(IF),
    test.type = 1,
    sfu = gsDesign::sfLDOF,
    timing = IF
  )$upper$bound
  (is.na(gs_b(par, k = 3)))
})
