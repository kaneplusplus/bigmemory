library("bigmemory")
context("read")

test_that("test_read", {
    mat = matrix(1:9, ncol = 3, nrow = 3, dimnames = list(letters[1:3], 
                                                          LETTERS[1:3]))
    retList = list(mat, rownames(mat), colnames(mat))
    matnull = matrix(1:9, ncol = 3, nrow = 3)
    retListNull = list(mat, NULL, NULL)
    bm = as.big.matrix(mat)
    bmnull = as.big.matrix(matnull)
    expect_identical(bm[2, 2], mat[2, 2], info = "scalar with names")
    expect_identical(bmnull[2, 2], matnull[2, 2], info = "scalar without names")
    expect_identical(bm[2, ], mat[2, ], info = "row with names")
    expect_identical(bmnull[2, ], matnull[2, ], info = "row without names")
    expect_identical(bm[, 2], mat[, 2], info = "column with names")
    expect_identical(bmnull[, 2], matnull[, 2], info = "column without names")
    expect_identical(bm[2, 2, drop = FALSE], mat[2, 2, drop = FALSE], 
                     info = "drop is FALSE scalar with names")
    expect_identical(bmnull[2, 2, drop = FALSE], matnull[2, 2, 
                                                         drop = FALSE], info = "drop is FALSE scalar without names")
    expect_identical(bm[2, , drop = FALSE], mat[2, , drop = FALSE], 
                     info = "drop is FALSE row with names")
    expect_identical(bmnull[2, , drop = FALSE], matnull[2, , 
                                                        drop = FALSE], info = "drop is FALSE row without names")
    expect_identical(bm[, 2, drop = FALSE], mat[, 2, drop = FALSE], 
                     info = "drop is FALSE column with names")
    expect_identical(bmnull[, 2, drop = FALSE], matnull[, 2, 
                                                        drop = FALSE], info = "drop is FALSE column without names")
    expect_identical(bm[1:2, 2:3], mat[1:2, 2:3], info = "partial matrix with names")
    expect_identical(bmnull[1:2, 2:3], matnull[1:2, 2:3], info = "partial matrix without names")
    expect_identical(bm[, ], mat[, ], info = "full matrix with names")
    expect_identical(bmnull[, ], matnull[, ], info = "full matrix without names")
    mat.list = list(mat[2, , drop = FALSE], mat[, 2, drop = FALSE])
    for (mat in mat.list) {
        matnull = unname(mat)
        bm = as.big.matrix(mat)
        bmnull = as.big.matrix(matnull)
        expect_identical(bm[1, 1], mat[1, 1], info = "scalar with names")
        expect_identical(bmnull[1, 1], matnull[1, 1], info = "scalar without names")
        expect_identical(bm[1, ], mat[1, ], info = "row with names")
        expect_identical(bmnull[1, ], matnull[1, ], info = "row without names")
        expect_identical(bm[, 1], mat[, 1], info = "column with names")
        expect_identical(bmnull[, 1], matnull[, 1], info = "column without names")
        expect_identical(bm[1, 1, drop = FALSE], mat[1, 1, drop = FALSE], 
                         info = "drop is FALSE scalar with names")
        expect_identical(bmnull[1, 1, drop = FALSE], matnull[1, 
                                                             1, drop = FALSE], info = "drop is FALSE scalar without names")
        expect_identical(bm[1, , drop = FALSE], mat[1, , drop = FALSE], 
                         info = "drop is FALSE row with names")
        expect_identical(bmnull[1, , drop = FALSE], matnull[1, 
                                                            , drop = FALSE], info = "drop is FALSE row without names")
        expect_identical(bm[, 1, drop = FALSE], mat[, 1, drop = FALSE], 
                         info = "drop is FALSE column with names")
        expect_identical(bmnull[, 1, drop = FALSE], matnull[, 
                                                            1, drop = FALSE], info = "drop is FALSE column without names")
        expect_identical(bm[, ], mat[, ], info = "full matrix with names")
        expect_identical(bmnull[, ], matnull[, ], info = "full matrix without names")
    }
    return(TRUE)
})
