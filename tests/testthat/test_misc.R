library("bigmemory")
context("miscellaneous")

z <- filebacked.big.matrix(3, 3, type='integer', init=123,
                           backingfile="example.bin",
                           descriptorfile="example.desc",
                           dimnames=list(c('a','b','c'), c('d', 'e', 'f')))
mat <- matrix(1:9, ncol = 3, nrow = 3, dimnames = list(letters[1:3], 
                                                       LETTERS[1:3]))
bm <- as.big.matrix(mat)

test_that("is.nil works appropriately",{
    expect_error(is.nil("hello"), info="address is not an externalptr")
    expect_false(is.nil(bm@address))
})

test_that("file.name works appropriately",{
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