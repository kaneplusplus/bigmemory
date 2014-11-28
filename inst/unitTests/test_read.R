test_read <- function() {
  mat = matrix(1:9, ncol=3, nrow=3, dimnames=list(letters[1:3], LETTERS[1:3]))
  retList = list( mat, rownames(mat), colnames(mat))
  matnull = matrix(1:9, ncol=3, nrow=3)
  retListNull = list( mat, NULL, NULL )
  bm = as.big.matrix(mat)
  bmnull = as.big.matrix(matnull)
  
  checkIdentical(mat[2, 2], bm[2, 2], "scalar with names")
  checkIdentical(matnull[2, 2], bmnull[2, 2], "scalar without names")
  checkIdentical(mat[2,], bm[2,], "row with names")
  checkIdentical(matnull[2,], bmnull[2,], "row without names")
  checkIdentical(mat[, 2], bm[, 2], "column with names")
  checkIdentical(matnull[, 2], bmnull[, 2], "column without names")
  
  checkIdentical(mat[2, 2, drop=FALSE], bm[2, 2, drop=FALSE], "drop is FALSE scalar with names")
  checkIdentical(matnull[2, 2, drop=FALSE], bmnull[2, 2, drop=FALSE], "drop is FALSE scalar without names")
  checkIdentical(mat[2,, drop=FALSE], bm[2,, drop=FALSE], "drop is FALSE row with names")
  checkIdentical(matnull[2,, drop=FALSE], bmnull[2,, drop=FALSE], "drop is FALSE row without names")
  checkIdentical(mat[, 2, drop=FALSE], bm[, 2, drop=FALSE], "drop is FALSE column with names")
  checkIdentical(matnull[, 2, drop=FALSE], bmnull[, 2, drop=FALSE], "drop is FALSE column without names")
  
  checkIdentical(mat[1:2, 2:3], bm[1:2, 2:3], "partial matrix with names")
  checkIdentical(matnull[1:2, 2:3], bmnull[1:2, 2:3], "partial matrix without names")
  checkIdentical(mat[, ], bm[, ], "full matrix with names")
  checkIdentical(matnull[, ], bmnull[, ], "full matrix without names")

  # With wierd single col or row bigmats
  mat.list = list( mat[2, , drop=FALSE], mat[, 2, drop=FALSE] )
  for (mat in mat.list) {
    matnull = unname(mat)
    bm = as.big.matrix(mat)
    bmnull = as.big.matrix(matnull)

    checkIdentical(mat[1, 1], bm[1, 1], "scalar with names")
    checkIdentical(matnull[1, 1], bmnull[1, 1], "scalar without names")
    checkIdentical(mat[1,], bm[1,], "row with names")
    checkIdentical(matnull[1,], bmnull[1,], "row without names")
    checkIdentical(mat[, 1], bm[, 1], "column with names")
    checkIdentical(matnull[, 1], bmnull[, 1], "column without names")
    
    checkIdentical(mat[1, 1, drop=FALSE], bm[1, 1, drop=FALSE], "drop is FALSE scalar with names")
    checkIdentical(matnull[1, 1, drop=FALSE], bmnull[1, 1, drop=FALSE], "drop is FALSE scalar without names")
    checkIdentical(mat[1,, drop=FALSE], bm[1,, drop=FALSE], "drop is FALSE row with names")
    checkIdentical(matnull[1,, drop=FALSE], bmnull[1,, drop=FALSE], "drop is FALSE row without names")
    checkIdentical(mat[, 1, drop=FALSE], bm[, 1, drop=FALSE], "drop is FALSE column with names")
    checkIdentical(matnull[, 1, drop=FALSE], bmnull[, 1, drop=FALSE], "drop is FALSE column without names")
    
    checkIdentical(mat[, ], bm[, ], "full matrix with names")
    checkIdentical(matnull[, ], bmnull[, ], "full matrix without names")
  }
  return(TRUE)
}
