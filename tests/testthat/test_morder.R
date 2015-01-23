library("bigmemory")
context("morder/mpermute")

m = matrix(as.double(as.matrix(iris[,1:4])), nrow=nrow(iris[,1:4]))
n = m

test_that("morder equivalent to order",{
    expect_identical(morder(m, 1), as.numeric(order(m[,1])))
})

test_that("mpermute changes elements order",{
    expect_false(all(m == mpermute(m, cols=1)))
    expect_true(all(n == m[morder(m,1),]))
})
