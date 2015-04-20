library("bigmemory")
context("big.matrix Float Type")

z <- filebacked.big.matrix(3, 3, type='float', init=123.0,
                           backingfile="example.bin",
                           descriptorfile="example.desc",
                           dimnames=list(c('a','b','c'), c('d', 'e', 'f')))

mat <- matrix(1:9, ncol = 3, nrow = 3, dimnames = list(letters[1:3], 
                                                       LETTERS[1:3]))

dmat <- matrix(rnorm(9), ncol = 3, nrow = 3, dimnames = list(letters[1:3], 
                                                       LETTERS[1:3]))

bm <- as.big.matrix(mat, type="float")

test_that("filebacked matrix created successfully",{
    expect_true(file.exists("example.bin"))
    expect_true(file.exists("example.desc"))
    expect_true(all(z[,] == 123.0))
    expect_true(typeof(z) == "float")
})

test_that("RAM matrix created successfully",{
    expect_true(all(bm[,] == mat))
    expect_true(typeof(bm) == "float")
})

# Float data types are not typical in R
# The default warning is a sanity check to realize that
# any double (i.e. numeric) values passed are down cast to float
test_that("Proper warning returned", {
    expect_warning(as.big.matrix(dmat, type="float"), info="Not warning about
                   float type downcast")
})
