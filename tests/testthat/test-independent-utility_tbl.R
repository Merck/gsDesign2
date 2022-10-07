test_that("test rounddf", {
  x2 <- rnorm(3)
  x3 <- rnorm(3)
  tbl <- tibble::tibble(x1 = c("a", "b", "c"), x2 = x2, x3 = x3)
  
  tbl_new <- gsDesign2:::rounddf(tbl, digits = 2)
  expect_equal(tbl_new$x1, tbl$x1)
  expect_equal(tbl_new$x2, round(x2, 2))
  expect_equal(tbl_new$x3, round(x3, 2))
  
  tbl_new <- gsDesign2:::rounddf(tbl, digits = c(1, 1, 2))
  expect_equal(tbl_new$x1, tbl$x1)
  expect_equal(tbl_new$x2, round(x2, 1))
  expect_equal(tbl_new$x3, round(x3, 2))
})

test_that("test table_ab", {
  a <- data.frame(Index = 1:2, a1 = c(1.1234, 5.6789), a2 = c("text 1", "text 2"))
  b <- data.frame(Index = 1:2,
                  b1 = c("apple", "penny"),
                  b2 = 1:2,
                  b3 = 3:4)
  tbl <- gsDesign2:::table_ab(a, b, byvar = "Index", decimals = c(0, 2, 0), aname = "Index")
  
  expect_equal(tbl$Index, c(paste0("Index: 1 a1: ", round(1.1234, 2), " a2: text 1"),
                            paste0("Index: 2 a1: ", round(5.6789, 2), " a2: text 2")))
  expect_equal(tbl$b1, b$b1)
  expect_equal(tbl$b2, b$b2)
  expect_equal(tbl$b3, b$b3)
})