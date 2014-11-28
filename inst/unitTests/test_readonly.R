library(bigmemory)
library(RUnit)

rownames = letters[1:3]
colnames = LETTERS[1:3]


back.dir = tempdir()
fbm.file = "fbm"
fbm.desc.file = "fbm.desc"
fbm.desc.path = file.path(back.dir,fbm.desc.file)
fbm.data.path = file.path(back.dir,fbm.file)
fbm = filebacked.big.matrix(3,3,dimnames=list(rownames,colnames),backingpath=back.dir, backingfile=fbm.file, descriptorfile=paste(fbm.file,".desc",sep=""))
fbm[,] = 1:9

bm = big.matrix(3,3,dimnames=list(rownames,colnames))

test_readonly <- function() {

  # In-memory bm
  mat = matrix(1:9,ncol=3,dimnames=list(rownames,colnames))
  bm = big.matrix(3,3,dimnames=list(rownames,colnames))
  bm[,] = mat
  bm2 = attach.big.matrix(describe(bm),readonly=TRUE)
  bm3 = attach.big.matrix(describe(bm),readonly=FALSE)

  checkTrue(!is.readonly(bm), "bm should not be readonly")
  checkTrue(is.readonly(bm2), "bm2 should be readonly")
  checkTrue(!is.readonly(bm3), "bm3 should be readonly")
  
  checkEquals(bm2[2,2],mat[2,2],"Read big.matrix attached as read-only is OK")
  checkException({bm2[1,1] = 100},"Writing to a big.matrix made read-only by FS before attached gives error", silent=TRUE)
  checkException({bm2[1,] = 100},"Writing row to a big.matrix made read-only by FS before attached gives error", silent=TRUE)
  checkException({bm2[,1] = 100},"Writing column to a big.matrix made read-only by FS before attached gives error", silent=TRUE)
  checkException({bm2[,] = 100},"Writing to full matrix a big.matrix made read-only by FS before attached gives error", silent=TRUE)
  checkException({bm2[ matrix(c(1,2,2,2),ncol=2),] = 100},"Writing subset by matrix to a big.matrix made read-only by FS before attached gives error", silent=TRUE)

  # Filebacked bm, attaching
  fbm = filebacked.big.matrix(3,3,dimnames=list(rownames,colnames),backingpath=back.dir, backingfile=fbm.file, descriptorfile=fbm.desc.file)
  fbm[,] = mat[,] = 1:9
  fbm2 = attach.big.matrix(describe(fbm),path=back.dir,readonly=TRUE)
  fbm3 = attach.big.matrix(describe(fbm),path=back.dir,readonly=FALSE)
  checkTrue(!is.readonly(fbm), "fbm should not be readonly")
  checkTrue(is.readonly(fbm2), "fbm2 should be readonly")
  checkTrue(!is.readonly(fbm3), "fbm3 should not be readonly")
  Sys.chmod(fbm.data.path,"0444")
#  checkTrue(is.readonly(fbm), "fbm should be readonly if changed by FS")
  checkTrue(is.readonly(attach.big.matrix(fbm.desc.path,readonly=FALSE)), "FBM should be readonly if readonly on FS, even if requested read/write")
  Sys.chmod(fbm.data.path,"0644")
  checkTrue(is.readonly(attach.big.matrix(fbm.desc.path,readonly=TRUE)), "FBM should not be readonly if you ask for that.")

  # Filebacked bm, reading and writing
  checkEquals(fbm2[2,2],mat[2,2],"Read big.matrix attached as read-only is OK")
  checkException({fbm2[1,1] = 100},"Writing to a big.matrix made read-only by FS before attached gives error")
  checkException({fbm2[1,] = 100},"Writing row to a big.matrix made read-only by FS before attached gives error")
  checkException({fbm2[,1] = 100},"Writing column to a big.matrix made read-only by FS before attached gives error")
  checkException({fbm2[,] = 100},"Writing to full matrix a big.matrix made read-only by FS before attached gives error")
  checkException({fbm2[ matrix(c(1,2,2,2),ncol=2),] = 100},"Writing subset by matrix to a big.matrix made read-only by FS before attached gives error")
  checkEquals( fbm2[,], mat[,],"Writing to a big.matrix made read-only by FS before attached does nothing")
  checkException( {fbm3 = attach.big.matrix(fbm.desc.path, readonly=TRUE); fbm3[1, 1] = 100}, "Should give error if you ask for a readonly matrix and try to write to it.")
  return(TRUE)
}
