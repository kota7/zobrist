library(zobrist)
library(testthat)
library(combiter)


context("Rehash")

test_that("Rehash", {
  z <- zobristht(5, 3, rehashable = TRUE, threslf = 0.5)
  ## this will accept upto 2^3 / 2 = 4 keys stored
  ## when we have 4 or more items, rehash is invoked
  iter <- isubset(5)
  while (hasNext(iter))
  {
    i <- nextElem(iter)
    z$update(i, sum(i))
  }

  ## check if the values are still correctly stored
  iter <- isubset(5)
  while (hasNext(iter))
  {
    i <- nextElem(iter)
    expect_equal(z$get(i), sum(i))
  }

})
