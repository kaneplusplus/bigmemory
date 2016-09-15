library("bigmemory")
context("mwhich")

# global objects
x <- as.big.matrix(matrix(1:30, 10, 3))
options(bigmemory.allow.dimnames=TRUE)
colnames(x) <- c("A", "B", "C")

x1 <- x[mwhich(x, 1:2, list(c(2,3), c(11,17)),
               list(c('ge','le'), c('gt', 'lt')), 'OR'),]
x2 <- x[mwhich(x, c("A","B"), list(c(2,3), c(11,17)), 
               list(c('ge','le'), c('gt', 'lt')), 'AND'),]

y <- matrix(1:30, 10, 3)
colnames(y) <- c("A", "B", "C")
y1 <- y[mwhich(y, 1:2, list(c(2,3), c(11,17)),
               list(c('ge','le'), c('gt', 'lt')), 'OR'),]

y2 <- y[mwhich(y, -3, list(c(2,3), c(11,17)),
               list(c('ge','le'), c('gt', 'lt')), 'AND'),]

test_that("mwhich indices correct",{
    expect_identical(x[2:6,], x1)
    expect_identical(x[2:3,], x2)
})

test_that("mwhich works for regular 'matrix'",{
    expect_identical(y1, x1)
    expect_identical(y2, x2)
})

test_that("mwhich recognizes NA", {
    x[1,1] <- NA
    expect_identical(mwhich(x, 1:2, NA, 'eq', 'OR'), 1)
    expect_identical(mwhich(x, 1:2, NA, 'neq', 'AND'), as.numeric(2:10))
})

test_that("mwhich recognizes Inf",{
    x <- big.matrix(4, 2, init=1, type="double")
    x[1,1] <- Inf
    mwhich(x, 1, Inf, 'eq')
    mwhich(x, 1, 1, 'gt')
    mwhich(x, 1, 1, 'le')
})