library(zobrist)
library(testthat)
library(combiter)


context("Rehash")

test_that("Rehash", {
  z <- zht(5, hashsize = 1, rehashable = TRUE, threslf = 0.1)
  ## this will accept upto 2^3 / 2 = 4 keys stored
  ## when we have 4 or more items, rehash is invoked
  iter <- isubset(5)
  while (hasNext(iter))
  {
    i <- nextElem(iter)
    z[i] <- sum(i)
  }

  ## check if the values are still correctly stored
  iter <- isubset(5)
  while (hasNext(iter))
  {
    i <- nextElem(iter)
    expect_equal(z[i], sum(i))
  }

})
