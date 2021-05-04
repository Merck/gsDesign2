testthat::test_that("s2pwe fails to come up with the correct answer",{
  time=c(1,5,6,8,10)
  survival=c(0.5,0.4,0.3,0.2,0.1)
  expect_equivalent(round(s2pwe(time=time,survival=survival),3),
                    round(tibble(duration=c(1,4,1,2,2),rate=c(0.693,0.0558,0.288,0.203,0.347)),3))
})

