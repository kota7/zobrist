library(zobrist)
library(testthat)
library(combiter)


context("Hash table methods (update/find/delete/get)")


test_that("update and get", {
  z <- zobristht(5, 4)
  expect_null(z$get(3))

  z$update(3, 2)
  expect_equal(z$get(3), 2)
  expect_null(z$get(2))

  z$update(3, 5)
  expect_equal(z$get(3), 5)
  expect_null(z$get(1))


  iter <- isubset(5)
  while (hasNext(iter))
  {
    i <- nextElem(iter)
    z$update(i, sum(i))
    expect_equal(z$get(i), sum(i))
  }

})



test_that("delete and find", {
  z <- zobristht(5, 3)

  iter <- isubset(5)
  while (hasNext(iter))
  {
    i <- nextElem(iter)
    expect_false(z$find(i))
    z$update(i, sum(i))
    expect_true(z$find(i))
    z$delete(i)
    expect_false(z$find(i))
  }

})


