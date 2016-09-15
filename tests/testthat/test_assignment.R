library("bigmemory")
context("big.matrix assignment")

set.seed(123)
z <- filebacked.big.matrix(3, 3, type='double', init=123.0,
                           backingfile="example.bin",
                           descriptorfile="example.desc",
                           dimnames=list(c('a','b','c'), c('d', 'e', 'f')))

mat <- matrix(1:9, ncol = 3, nrow = 3, dimnames = list(letters[1:3], 
                                                       LETTERS[1:3]))
bm <- as.big.matrix(mat, type = "double")
z[] <- mat

test_that("Able to assign individual elements", {
  
  tmp <- bm[]
  
  # element assign
  bm[1,3] <- 15.123
  z[1,3] <- 15.123
  mat[1,3] <- 15.123
  
  expect_equivalent(bm[1,3], 15.123)
  expect_equivalent(z[1,3], 15.123)
  
  bm[] <- tmp
  z[] <- tmp
})

test_that("Able to assign non-contiguous individual elements", {
  
  tmp <- bm[]
  
  # element assign
  bm[c(1,3,5)] <- 15.123
  z[c(1,3,5)] <- 15.123
  mat[c(1,3,5)] <- 15.123
  
  expect_equivalent(bm[], mat)
  expect_equivalent(z[], mat)
  
  bm[] <- tmp
  z[] <- tmp
})

test_that("Able to assign rowwise elements", {
  
  row <- rnorm(3)
  tmp <- bm[]
  
  # row assign
  bm[2,] <- row
  z[2,] <- row
  mat[2,] <- row
  
  expect_equivalent(bm[], mat)
  expect_equivalent(z[], mat)
  
  bm[] <- tmp
  z[] <- tmp
})

test_that("Able to assign columnwise elements", {
  
  col <- rnorm(3)
  tmp <- bm[]
  
  # column assign
  bm[,3] <- col
  z[,3] <- col
  mat[,3] <- col
  
  expect_equivalent(bm[], mat)
  expect_equivalent(z[], mat)
  
  bm[] <- tmp
  z[] <- tmp
})


test_that("Able to assign non-contiguous columns", {
  
  tmp <- bm[]
  
  # column assign
  bm[,c(1,3)] <- mat[,c(1,3)]
  z[,c(1,3)] <- mat[,c(1,3)]
  mat[,c(1,3)] <- mat[,c(1,3)]
  
  expect_equivalent(bm[], mat)
  expect_equivalent(z[], mat)
  
  bm[] <- tmp
  z[] <- tmp
})

test_that("Able to assign matrix to rows", {
  tmp <- bm[]
  
  newmat <- matrix(1:6, 2, 3)
  
  bm[1:2,] <- newmat[1:2,]
  z[1:2,] <- newmat[1:2,]
  mat[1:2,] <- newmat[1:2,]
  
  expect_equivalent(bm[], mat)
  expect_equivalent(z[], mat)
  
  bm[] <- tmp
  z[] <- tmp
  mat <- tmp
  
})

rm(z)
gc()
file.remove('example.bin')
file.remove('example.desc')

