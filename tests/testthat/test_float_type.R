context("big.matrix float type")

options(bigmemory.typecast.warning=FALSE)

set.seed(123)
z <- filebacked.big.matrix(3, 3, type='float', init=123.0,
                           backingfile="example.bin",
                           descriptorfile="example.desc",
                           dimnames=list(c('a','b','c'), c('d', 'e', 'f')))

mat <- matrix(1:9, ncol = 3, nrow = 3, dimnames = list(letters[1:3], 
                                                       LETTERS[1:3]))

dmat <- matrix(rnorm(9), ncol = 3, nrow = 3, dimnames = list(letters[1:3], 
                                                       LETTERS[1:3]))

fmat <- big.matrix(3,3, type="float", init = 13.123)

bm <- as.big.matrix(dmat, type="float")


test_that("filebacked matrix created successfully",{
    expect_true(file.exists("example.bin"))
    expect_true(file.exists("example.desc"))
    expect_true(all(z[,] == 123.0))
    expect_true(typeof(z) == "float")
})

test_that("RAM matrix created successfully",{
    expect_equal(bm[,], dmat, tolerance = 1e-7)
    expect_true(typeof(bm) == "float")
})

test_that("Able to access and assign elements", {
    fmat[1,3] <- 15.123
    expect_equivalent(fmat[1,3], 15.123)
    
    newRow <- rnorm(3)
    fmat[1,] <- newRow    
    expect_equal(fmat[1,], newRow, tolerance = 1e-07)

    newCol <- rnorm(3)
    fmat[,1] <- newCol
    expect_equal(fmat[,1], newCol, tolerance = 1e-07)
})

# Float data types are not typical in R
# The default warning is a sanity check to realize that
# any double (i.e. numeric) values passed are down cast to float
options(bigmemory.typecast.warning=TRUE)
test_that("Proper warning returned", {
    expect_warning(as.big.matrix(dmat, type="float"), info="Not warning about
                   float type downcast")
})

rm(z)
gc()
file.remove('example.bin')
file.remove('example.desc')

