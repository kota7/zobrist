library(zobrist)
library(testthat)
library(combiter)


context("Hash function")

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



test_that("using incr", {
  z <- zobristht(7, 3)
  key <- c(5, 2, 3, 7)
  value <- 5
  z$update(key, value)

  iter <- isubset(4)
  while (hasNext(iter))
  {
    i <- nextElem(iter)
    x <- key[i]
    y <- setdiff(key, x)
    expect_equal(z$get(x, y), value)
  }

})




test_that("vectorized version", {
  z <- zobristht(7, 4)

  key <- c(5, 2, 3, 7)

  iter <- isubset(4)
  h1 <- integer(0)
  keys <- list()
  while (hasNext(iter))
  {
    i <- nextElem(iter)
    x <- key[i]
    h1 <- c(h1, z$hashfunc(x))
    keys <- c(keys, list(x))
  }
  h2 <- z$hashfunc_vec(keys)
  expect_true(all(h1 == h2))

})





