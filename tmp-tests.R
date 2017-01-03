Rcpp::sourceCpp('tmp-tests.cpp')

require(microbenchmark)
require(bigmemory)

X <- big.matrix(1e4, 1e4, shared = TRUE)
X[] <- rnorm(length(X))
y <- rnorm(ncol(X))

print(microbenchmark(
  test1 <- produ1(X@address, y),
  test2 <- produ2(X@address, y),
  times = 10
))

print(all.equal(test1, test2))

X2 <- sub.big.matrix(X, firstRow = 10, lastRow = 9e3,
                     firstCol = 10, lastCol = 9e3)
y2 <- rnorm(ncol(X2))

print(microbenchmark(
  test3 <- produ1(X2@address, y2),
  test4 <- produ2(X2@address, y2),
  times = 10
))
print(all.equal(test3, test4))
