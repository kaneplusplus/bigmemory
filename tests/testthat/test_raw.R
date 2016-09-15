library("bigmemory")
context("raw data")

m<-big.matrix(10,1,type='raw')
f<-big.matrix(10,1,type='raw',backingpath = tempfile(fileext = '.bin'))

test_that("Reading and writing byte<128 on memory-backed file",{
  m[3,1]<-as.raw(10)
  expect_equal(m[3,1],as.raw(10))
})

test_that("Reading and writing byte>=128 on memory-backed file",{
  m[4,1]<-as.raw(130)
  expect_equal(m[4,1],as.raw(130))
})

test_that("Reading and writing byte<128 on file-backed file",{
  f[5,1]<-as.raw(10)
  expect_equal(f[5,1],as.raw(10))
})

test_that("Reading and writing byte>=128 on memory-backed file",{
  f[6,1]<-as.raw(130)
  expect_equal(f[6,1],as.raw(130))
})

test_that("Making sure the 'char' matrix behaves like signed byte", {
  mchar<-big.matrix(3,1,type='char')
  mchar[1,1]<-   40
  mchar[2,1]<- -123
  mchar[3,1]<-  190
  expect_equal(mchar[1:3,1],c(40,-123,NA))
  
})

test_that("Testing for as.big.matrix for raw", {
  x <- matrix(as.raw(seq(0,255, length.out = 16)), 4, 4)
  expect_equal(typeof(x),'raw')
  expect_equal(class(x),'matrix')
  m<-as.big.matrix(x, type = "raw")
  expect_equal(x[1,], as.raw(c(0,68,136,204)))
})