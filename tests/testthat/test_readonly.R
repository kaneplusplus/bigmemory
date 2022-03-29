context("read_only")

rownames = letters[1:3]
colnames = LETTERS[1:3]

back.dir = tempdir()
fbm.file = "fbm"
fbm.desc.file = "fbm.desc"
fbm.desc.path = file.path(back.dir,fbm.desc.file)
fbm.data.path = file.path(back.dir,fbm.file)
# fbm = filebacked.big.matrix(3,3,dimnames=list(rownames,colnames),backingpath=back.dir, backingfile=fbm.file, descriptorfile=paste(fbm.file,".desc",sep=""))
# fbm[,] = 1:9

bm = big.matrix(3,3,dimnames=list(rownames,colnames))

test_that("test_readonly", {
  mat = matrix(1:9, ncol = 3, dimnames = list(rownames, colnames))
  bm = big.matrix(3, 3, dimnames = list(rownames, colnames))
  bm[, ] = mat
  bm2 = attach.big.matrix(describe(bm), readonly = TRUE, shared=FALSE)
  bm3 = attach.big.matrix(describe(bm), readonly = FALSE, shared=FALSE)
  expect_false(is.readonly(bm), info = "bm should not be readonly")
  expect_true(is.readonly(bm2), info = "bm2 should be readonly")
  expect_false(is.readonly(bm3), info = "bm3 should be readonly")
  expect_equal(mat[2, 2], bm2[2, 2], info = "Read big.matrix attached as read-only is OK")
  expect_error({
      bm2[1, 1] = 100
  }, info = "Writing to a big.matrix made read-only by FS before attached gives error")
  expect_error({
      bm2[1, ] = 100
  }, info = "Writing row to a big.matrix made read-only by FS before attached gives error")
  expect_error({
      bm2[, 1] = 100
  }, info = "Writing column to a big.matrix made read-only by FS before attached gives error")
  expect_error({
      bm2[, ] = 100
  }, info = "Writing to full matrix a big.matrix made read-only by FS before attached gives error")
  expect_error({
      bm2[matrix(c(1, 2, 2, 2), ncol = 2), ] = 100
  }, info = "Writing subset by matrix to a big.matrix made read-only by FS before attached gives error")
    # in order to reuse, must remove prior objects
#     rm(fbm)
#     gc()
#     file.remove(file.path(back.dir, fbm.file))
#     file.remove(file.path(back.dir, fbm.desc.file))
   
    if (file.exists(file.path(back.dir, fbm.file))) {
      unlink(c(file.path(back.dir, fbm.file), 
               file.path(back.dir, fbm.desc.file)))
    } 
    fbm = filebacked.big.matrix(3, 3, dimnames=list(rownames, colnames), 
                                backingpath=back.dir, backingfile=fbm.file, 
                                descriptorfile=fbm.desc.file)
    fbm[, ] = mat[, ] = 1:9
    fbm2 = attach.big.matrix(describe(fbm), path = back.dir, 
                             readonly = TRUE)
    fbm3 = attach.big.matrix(describe(fbm), path = back.dir, 
                             readonly = FALSE)
    expect_false(is.readonly(fbm), info = "fbm should not be readonly")
    expect_true(is.readonly(fbm2), info = "fbm2 should be readonly")
    expect_false(is.readonly(fbm3), info = "fbm3 should not be readonly")
    Sys.chmod(fbm.data.path, "0444")
    expect_warning(attach.big.matrix(fbm.desc.path, 
                                     readonly = FALSE),
                   info = "big.matrix object could only be opened read-only.")
    expect_true(suppressWarnings(
        is.readonly(attach.big.matrix(fbm.desc.path, 
                                      readonly = FALSE))), 
        info = "FBM should be readonly if readonly on FS, even if requested read/write")
    Sys.chmod(fbm.data.path, "0644")
    expect_true(is.readonly(attach.big.matrix(fbm.desc.path, 
                                              readonly = TRUE)), info = "FBM should not be readonly if you ask for that.")
    expect_equal(mat[2, 2], fbm2[2, 2], info = "Read big.matrix attached as read-only is OK")
    expect_error({
        fbm2[1, 1] = 100
    }, info = "Writing to a big.matrix made read-only by FS before attached gives error")
    expect_error({
        fbm2[1, ] = 100
    }, info = "Writing row to a big.matrix made read-only by FS before attached gives error")
    expect_error({
        fbm2[, 1] = 100
    }, info = "Writing column to a big.matrix made read-only by FS before attached gives error")
    expect_error({
        fbm2[, ] = 100
    }, info = "Writing to full matrix a big.matrix made read-only by FS before attached gives error")
    expect_error({
        fbm2[matrix(c(1, 2, 2, 2), ncol = 2), ] = 100
    }, info = "Writing subset by matrix to a big.matrix made read-only by FS before attached gives error")
    expect_equal(mat[, ], fbm2[, ], info = "Writing to a big.matrix made read-only by FS before attached does nothing")
    expect_error({
        fbm3 = attach.big.matrix(fbm.desc.path, readonly = TRUE)
        fbm3[1, 1] = 100
    }, info = "Should give error if you ask for a readonly matrix and try to write to it.")
    return(TRUE)

    rm(fbm, fbm2, fbm3)
    gc()
    file.remove(file.path(back.dir, fbm.file))
    file.remove(file.path(back.dir, fbm.desc.file))
})


