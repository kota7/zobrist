library(zobrist)
library(testthat)
library(combiter)


context("Hash table methods (update/find/delete/get)")


test_that("update and get", {
  z <- zht(5, 4)
  expect_null(z[3])

  z[3] <- 2
  expect_equal(z[3], 2)
  expect_null(z[2])

  z[c(3, 1)] <- "xyz"
  expect_equal(z[c(1, 3)], "xyz")


  z[5] <- list(x = 5, y = "abc")
  expect_equal(z[5], list(x = 5, y = "abc"))

  z <- zht(5, 4)
  iter <- isubset(5)
  ct <- 0L
  while (hasNext(iter))
  {
    i <- nextElem(iter)
    z[i] <- sum(i)
    expect_equal(z[i], sum(i))
    ct <- ct + 1
    expect_equal(z$size(), ct)
  }

})



test_that("delete and find", {
  z <- zht(5, 3)

  iter <- isubset(5)
  while (hasNext(iter))
  {
    i <- nextElem(iter)
    expect_false(haskey(z, i))
    z[i] <- sum(i)
    expect_true(haskey(z, i))
    z[i] <- NULL
    expect_false(haskey(z, i))

    expect_equal(z$size(), 0L)
  }

})


