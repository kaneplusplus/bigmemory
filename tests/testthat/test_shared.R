context("shared option")

X1 <- big.matrix(10, 10)

opt.save <- options(bigmemory.default.shared=FALSE)
X2 <- big.matrix(10, 10)
options(opt.save)

test_that("Default shared parameter is read from options",{
  expect_true(is.shared(X1))
  expect_false(is.shared(X2))
})
