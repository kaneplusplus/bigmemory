library("bigmemory")
context("Matrix Manipulations")

z <- filebacked.big.matrix(3, 3, type='integer', init=123,
                           backingfile="example.bin",
                           descriptorfile="example.desc",
                           dimnames=list(c('a','b','c'), c('d', 'e', 'f')))

mat <- matrix(1:9, ncol = 3, nrow = 3, dimnames = list(letters[1:3], 
                                                       LETTERS[1:3]))
bm <- as.big.matrix(mat)
df <- as.data.frame(mat)
l <- as.list(seq(3))
vec <- seq(3)

test_that("warnings returned", {
    expect_warning(
        as.big.matrix(vec),
        regexp="*Coercing vector to a single-column matrix.")
    expect_warning(
        as.big.matrix(df), 
        regexp="*Coercing data.frame to matrix via factor level numberings.")
})

test_that("is.big.matrix recognizes objects correctly", {
    expect_true(is.big.matrix(bm))
    expect_false(is.big.matrix(mat), 
                info="matrix interpreted as big.matrix")
    expect_false(is.big.matrix("hello"), 
                 info="character interpreted as big.matrix")
    expect_false(is.big.matrix(l),
                 info="list interpreted as list")
})

test_that("as.big.matrix converts types correctly",{
    expect_is(as.big.matrix(mat), "big.matrix")
    expect_is(suppressWarnings(as.big.matrix(vec)), "big.matrix")
    expect_is(suppressWarnings(as.big.matrix(df)), "big.matrix")
    expect_equivalent(as.big.matrix(mat)[,], mat)
    expect_equivalent(suppressWarnings(as.big.matrix(vec))[,], vec)
    expect_true(all(suppressWarnings(as.big.matrix(df))[,] == df))
})

test_that("flush works correctly",{
    expect_true(flush(z))
    if (Sys.info()['sysname'] != "Darwin")
      expect_warning(flush(bm), info="You cannot call flush on a non-filebacked 
                     big.matrix")
})

rm(z)
gc()
file.remove('example.bin')
file.remove('example.desc')
