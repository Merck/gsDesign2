count_footnotes <- function(x) {
  length(x$footnotes)
}

assert("lt::lt: footnote=FALSE removes footnote", {

  # fixed design
  x <- fixed_design_ahr(
    enroll_rate = define_enroll_rate(duration = 18, rate = 1),
    fail_rate = define_fail_rate(duration = 18, fail_rate = 0.1, dropout_rate = 0.001)
  )
  y <- summary(x)
  z1 <- lt::lt(y)
  (count_footnotes(z1) %==% 1L)
  z2 <- lt::lt(y, footnote = FALSE)
  (count_footnotes(z2) %==% 0L)

  # gs design
  x <- gs_design_ahr()
  y <- summary(x)
  z1 <- lt::lt(y)
  (count_footnotes(z1) %==% 2L)
  z2 <- lt::lt(y, footnote = FALSE)
  (count_footnotes(z2) %==% 0L)

})
