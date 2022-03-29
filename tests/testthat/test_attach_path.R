context("backingpath option when attaching")

tmp <- tempfile()
tmp.dir <- dirname(tmp)
tmp.file <- basename(tmp)
desc.path <- paste0(tmp.file, ".desc")
X <- big.matrix(10, 10, backingfile = tmp.file, backingpath = tmp.dir,
                descriptorfile = desc.path, init = 0)
X.desc <- describe(X)

test_that("Format_path puts an '/' at the end if there isn't", {
  expect_equal(format_path("test"), "test/")
  expect_equal(format_path("test/"), "test/")
  expect_equal(format_path("test/test/"), "test/test/")
})

test_that("New element 'dirname' in description", {
  expect_equal(X.desc@description$dirname, format_path(tmp.dir))
})

test_that("you can attach from a full path", {
  X2 <- attach.big.matrix(file.path(tmp.dir, desc.path))
  expect_false(is.nil(X2@address), info = "the matrix exists")
  X2[] <- 1
  expect_equal(X[,], matrix(1, 10, 10), 
               info = "modifying X2 modifies also X")
})

test_that("you can attach from a composed path", {
  X3 <- attach.big.matrix(desc.path, backingpath = tmp.dir)
  expect_false(is.nil(X3@address), info = "the matrix exists")
  X3[] <- 2
  expect_equal(X[,], matrix(2, 10, 10), 
               info = "modifying X3 modifies also X")
})

test_that("you can attach with or without backingpath", {
  X4 <- attach.big.matrix(X.desc)
  expect_false(is.nil(X4@address), info = "the matrix exists")
  X4[] <- 3
  expect_equal(X[,], matrix(3, 10, 10), 
               info = "modifying X4 modifies also X")
  X5 <- attach.big.matrix(X.desc, backingpath = tmp.dir)
  expect_false(is.nil(X5@address), info = "the matrix exists")
  X5[] <- 4
  expect_equal(X[,], matrix(4, 10, 10), 
               info = "modifying X5 modifies also X")
})

test_that("you can sub with or without backingpath", {
  X6 <- sub.big.matrix(X.desc, lastCol = 5)
  expect_false(is.nil(X6@address), info = "the matrix exists")
  X6[] <- 1
  expect_equal(X[,], cbind(matrix(1, 10, 5), matrix(4, 10, 5)), 
               info = "modifying X6 modifies also X")
  X7 <- sub.big.matrix(X.desc, firstCol = 6, backingpath = tmp.dir)
  expect_false(is.nil(X7@address), info = "the matrix exists")
  X7[] <- 2
  expect_equal(X[,], cbind(matrix(1, 10, 5), matrix(2, 10, 5)), 
               info = "modifying X7 modifies also X")
})
