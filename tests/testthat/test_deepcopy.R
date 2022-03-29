context("deepcopy")

mat <- matrix(1:9, ncol = 3, nrow = 3, dimnames = list(letters[1:3], 
                                                      LETTERS[1:3]))
bm <- as.big.matrix(mat)
dm <- deepcopy(bm)
dbbm <- deepcopy(bm, type="double")

test_that("addresses are not the same", {
    expect_false(identical(dm@address ,bm@address))
})

test_that("contents equivalent",{
    expect_equivalent(bm[,], dm[,])
})

test_that("type is correctly set",{
    expect_true(typeof(bm) == typeof(dm))
    expect_true(typeof(bm) != typeof(dbbm))
    expect_true(typeof(dbbm) == "double")
})

test_that("sharing type is correct",{
    expect_true(is.shared(bm) && is.shared(dm))
})
