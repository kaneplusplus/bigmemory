################################################################################

context("LOCAL SUB")

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
    
    limsRow = sort(sample(N, 2))
    limsCol = sort(sample(M, 2))
    test <- sub.big.matrix(X, firstRow = limsRow[1], lastRow = limsRow[2],
                           firstCol = limsCol[1], lastCol = limsCol[2])
    expect_identical(test[,], X[seq2(limsRow), seq2(limsCol)])
  }
})

################################################################################

options(opt.save)

################################################################################
