library("bigmemory")
context("big.matrix creation")

z <- filebacked.big.matrix(3, 3, type='integer', init=123,
                           backingfile="example.bin",
                           descriptorfile="example.desc",
                           dimnames=list(c('a','b','c'), c('d', 'e', 'f')))

mat <- matrix(1:9, ncol = 3, nrow = 3, dimnames = list(letters[1:3], 
                                                       LETTERS[1:3]))

bm <- as.big.matrix(mat)


test_that("filebacked matrix created successfully",{
    expect_true(file.exists("example.bin"))
    expect_true(file.exists("example.desc"))
    expect_true(all(z[,] == 123))
})

test_that("describe returns correct data", {
    desc <- describe(bm)
    expect_is(desc, "big.matrix.descriptor")
    expect_true(length(desc@description$rowOffset) == 2)
    expect_true(length(desc@description$colOffset) == 2)
    expect_true(typeof(bm) == desc@description$type)
})

test_that("attach methods successful",{
    zdescription <- describe(z)
    bmdescription <- describe(bm)
    expect_is(zdescription, "big.matrix.descriptor")
    expect_is(bmdescription, "big.matrix.descriptor")
    
    y <- attach.big.matrix(zdescription)
    expect_false(identical(z@address, y@address))
    expect_identical(z[,], y[,])
    
    x <- attach.big.matrix(bmdescription)
    expect_false(identical(z@address, y@address))
    expect_identical(bm[,], x[,])
})

rm(z)
gc()
file.remove('example.bin')
file.remove('example.desc')
