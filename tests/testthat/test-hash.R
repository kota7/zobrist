library(zobrist)
library(testthat)


context("check behavior of hash function")

test_that("correct hashing", {
  z <- zobristht(5, 3)

  rr <- z$randomint
  key <- c(1, 2)
  expect_equal(z$hashfunc(key),
               bitwXor(rr[key][1], rr[key][2]))

  h1 <- z$hashfunc(2)
  h2 <- z$hashfunc(4)
  h3 <- z$hashfunc(c(2, 4))
  expect_equal(bitwXor(h1, h2), h3)
  expect_equal(bitwXor(h1, h1), 0L)
  expect_equal(bitwXor(h2, h3), h1)

  h4 <- z$hashfunc(5)
  h5 <- z$hashfunc(c(2, 4, 5))
  expect_equal(bitwXor(bitwXor(h1, h2), h4), h5)
  expect_equal(bitwXor(h5, h1), bitwXor(h2, h4))

})



test_that("invalid cases", {
  z <- zobristht(5, 3)
  expect_warning(z$hashfunc(0))

})
