################################################################################

context("local sub.big.matrix")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)

# Simulating some data
N <- 200
M <- 100
x <- matrix(rnorm(N * M, 0, 5), N, M, dimnames = list(sample(letters, N, TRUE),
                                                      sample(LETTERS, M, TRUE)))

################################################################################

ALL.TYPES <- c("char", "short", "integer", "float", "double", "raw")
seq2 <- function(lims) lims[1]:lims[2]

test_that("Equality with local sub.big.matrix", {
  for (t in ALL.TYPES) {
    if (t == "raw") {
      X <- abs(round(x))
      storage.mode(X) <- "raw"
      X <- as.big.matrix(X)
    } else {
      X <- as.big.matrix(x, type = t)
    }
    
    # one-level sub.big.matrix
    limsRow = sort(sample(N, 2))
    limsCol = sort(sample(M, 2))
    test <- sub.big.matrix(X, firstRow = limsRow[1], lastRow = limsRow[2],
                           firstCol = limsCol[1], lastCol = limsCol[2])
    expect_equal(test[,], X[seq2(limsRow), seq2(limsCol)])
    
    # two-level sub.big.matrix
    N2 <- nrow(test)
    M2 <- ncol(test)
    limsRow2 <- `if`(N2 > 2, sort(sample(N2, 2)), NULL)
    limsCol2 <- `if`(M2 > 2, sort(sample(M2, 2)), NULL)
    test2 <- sub.big.matrix(test, firstRow = limsRow2[1], lastRow = limsRow2[2],
                            firstCol = limsCol2[1], lastCol = limsCol2[2])
    expect_equal(test2[,], test[seq2(limsRow2), seq2(limsCol2)])
  }
})

################################################################################

options(opt.save)

################################################################################
