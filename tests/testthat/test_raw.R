library("bigmemory")
context("raw data")

m<-big.matrix(10,1,type='char')
f<-big.matrix(10,1,type='char',backingpath = tempfile(fileext = '.bin'))

test_that("Reading and writing byte<128 on memory-backed file",{
	m[3,1]<-as.raw(10)
	expect_equal(m[3,1],10)
})

test_that("Reading and writing byte>=128 on memory-backed file",{
	m[4,1]<-as.raw(130)
	expect_equal(m[4,1],130)
})

test_that("Reading and writing byte<128 on file-backed file",{
	f[5,1]<-as.raw(10)
	expect_equal(f[5,1],10)
})

test_that("Reading and writing byte>=128 on memory-backed file",{
	f[6,1]<-as.raw(130)
	expect_equal(f[6,1],130)
})
