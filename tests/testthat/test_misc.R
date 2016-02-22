library("bigmemory")
context("miscellaneous")

z <- filebacked.big.matrix(3, 3, type='integer', init=123,
                           backingfile="example.bin",
                           descriptorfile="example.desc",
                           dimnames=list(c('a','b','c'), c('A', 'B', 'C')))
mat <- matrix(1:9, ncol = 3, nrow = 3, dimnames = list(letters[1:3], 
                                                       LETTERS[1:3]))
bm <- as.big.matrix(mat)

test_that("is.nil works appropriately",{
    expect_error(is.nil("hello"), info="address is not an externalptr")
    expect_false(is.nil(bm@address))
})

test_that("file.name works appropriately",{
    if (Sys.info()['sysname'] != "Darwin")
      expect_error(file.name(bm), 
                   info="The argument is not a file backed big.matrix.")
    expect_identical(file.name(z), 'example.bin')
})

test_that("basic matrix metrics work",{
    expect_equivalent(ncol(bm), 3L)
    expect_equivalent(nrow(bm), 3L)
    expect_equivalent(dim(bm), c(3L, 3L))
    expect_equivalent(ncol(z), 3L)
    expect_equivalent(nrow(z), 3L)
    expect_equivalent(dim(z), c(3L, 3L))
})

test_that("dimnames returned are correct", {
    expect_is(dimnames(mat), "list")
    expect_identical(dimnames(mat), dimnames(bm), 
                     info = "dimnames don't match between big.matrix 
                     and matrix")
    expect_identical(dimnames(mat), dimnames(z), 
                     info = "dimnames don't match between 
                     filebacked.big.matrix and matrix")
})

rm(z)
gc()
file.remove('example.bin')
file.remove('example.desc')
