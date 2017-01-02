################################################################################

context("TRANSPOSE")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)

# Simulating some data
N <- 200
M <- 100
x <- matrix(rnorm(N * M, 0, 5), N, M, dimnames = list(sample(letters, N, TRUE),
                                                      sample(LETTERS, M, TRUE)))

################################################################################

ALL.TYPES <- c("char", "short", "integer", "float", "double", "raw")

test_that("Equality with t()", {
  for (t in ALL.TYPES) {
    if (t == "raw") {
      X <- abs(round(x[,]))
      storage.mode(X) <- "raw"
      X <- as.big.matrix(X)
    } else {
      X <- as.big.matrix(x, type = t)
    }
    
    test <- transpose(X)
    expect_identical(test[,], t(X[,]))
  }
})

################################################################################

options(opt.save)

################################################################################
