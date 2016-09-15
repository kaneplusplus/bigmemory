library("bigmemory")
context("morder/mpermute")
data(iris,package='datasets')


m = matrix(as.double(as.matrix(iris[,1:4])), nrow=nrow(iris[,1:4]))
n = m

mm <- m[1:4,1:4]
colnames(mm) <- letters[1:4]
bm <- as.big.matrix(mm)


test_that("morder equivalent to order",{
    expect_identical(morder(m, 1), as.numeric(order(m[,1])))
})

test_that("mpermute changes elements order",{
    expect_false(all(m == mpermute(m, cols=1)))
    expect_true(all(n == m[morder(m,1),]))
})

test_that("column reording works", {
  mpermuteCols(bm, order = c(3,4,1,2))
  expect_equivalent(bm[], mm[,c('c','d','a','b')])
  expect_equivalent(colnames(bm), c('c','d','a','b'))
  mpermuteCols(mm, order = c(3,4,1,2))
  expect_equivalent(colnames(mm), c('c','d','a','b'))
  expect_equivalent(bm[], mm)
  mpermuteCols(bm, rows = 1)
  mpermuteCols(mm, rows = 1)
  expect_equivalent(bm[], mm)
})

test_that("morderCols works",{
  expect_true(all(order(mm[1,]) == morderCols(bm, rows = 1)))
  expect_true(all(order(mm[2,]) == morderCols(mm, rows = 2)))
})

rm(bm)
gc()

