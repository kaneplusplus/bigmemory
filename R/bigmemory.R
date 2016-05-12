#' @useDynLib bigmemory
#' @import methods bigmemory.sri Rcpp
#' @importFrom utils head tail

#############################################################################
# This function is used to match up a vector of column names to the
# entire set of column names, providing the proper column indices.
# The name choice was based on the phrase "multiple map" though
# perhaps we should have made a different choice.

mmap = function(x, y) {
  if (is.null(x)) return(NULL)
  ans <- match(x, y)
  if (any(is.na(ans))) stop("Couldn't find a match to one of the arguments.")
  return(ans)
}

checkReadOnly <- function(x)
{
  if (is.readonly(x)) {
    stop("you may not modify a read-only big.matrix object")
  }
}

#############################################################################

#' @template big.matrix_class_template
#' @export
setClass('big.matrix', representation(address='externalptr'))

#' @rdname big.matrix.descriptor-class
setClass('descriptor', representation(description='list'))

#' @template big.matrix.descriptor_class_template
#' @rdname big.matrix.descriptor-class
#' @export
setClass('big.matrix.descriptor', contains='descriptor')

# Here, x is a big.matrix, and the result is a descriptor.

# Here, x is a descriptor, and the result is the description which is
# the relevant data needed for the attach.
setGeneric('description', function(x) standardGeneric('description'))

#' @rdname attach.big.matrix
#' @export
setMethod('describe', signature(x='big.matrix'),
  function(x)
  {
    return(new('big.matrix.descriptor', description=DescribeBigMatrix(x)))
  })


#' @template core_template
#' @export
big.matrix <- function(nrow, ncol, type=options()$bigmemory.default.type,
                       init=NULL, dimnames=NULL, separated=FALSE,
                       backingfile=NULL, backingpath=NULL, descriptorfile=NULL,
                       binarydescriptor=FALSE, shared=TRUE)
{
  if (!is.null(backingfile))
  {
    if (!shared) warning("All filebacked objects are shared.")
    return(filebacked.big.matrix(nrow=nrow, ncol=ncol, type=type, init=init,
                               dimnames=dimnames, separated=separated,
                               backingfile=backingfile, backingpath=backingpath,
                               descriptorfile=descriptorfile,
                               binarydescriptor=binarydescriptor))
  }
  if (nrow < 1 | ncol < 1)
    stop('A big.matrix must have at least one row and one column')

  typeVal <- NULL
  if (type == 'integer') typeVal <- 4
  if (type == 'float') typeVal <- 6
  if (type == 'double') typeVal <- 8
  if (type == 'short') typeVal <- 2
  if (type == 'char') typeVal <- 1
  if (is.null(typeVal)) stop('invalid type')
  if (!is.null(dimnames)) {
    rownames <- dimnames[[1]]
    colnames <- dimnames[[2]]
  } else {
    rownames <- NULL
    colnames <- NULL
  }
  if (is.null(init)) init <- NA
  if (shared) {
      address <- CreateSharedMatrix(as.double(nrow),
                  as.double(ncol),as.character(colnames),as.character(rownames),
                  as.integer(typeVal), as.double(init), as.logical(separated))
  } else {
    address <- CreateLocalMatrix(as.double(nrow),
                as.double(ncol), as.character(colnames), as.character(rownames),
                as.integer(typeVal), as.double(init), as.logical(separated))
  }
  if (is.null(address)) {
    stop(paste("Error: memory could not be allocated for instance",
               "of type big.matrix"))
  }
  x <- new("big.matrix", address=address)
  if (is.null(x)) {
    stop("Error encountered when creating instance of type big.matrix")
  }
  return(x)
}

#' @rdname big.matrix
#' @export
filebacked.big.matrix <- function(nrow, ncol,
                                  type=options()$bigmemory.default.type,
                                  init=NULL, dimnames=NULL, separated=FALSE,
                                  backingfile=NULL, backingpath=NULL, 
                                  descriptorfile=NULL, binarydescriptor=FALSE)
{    
    if (nrow < 1 | ncol < 1)
        stop('A big.matrix must have at least one row and one column')
    
    typeVal=NULL
    if (type == 'integer') typeVal <- 4
    if (type == 'float') typeVal <- 6
    if (type == 'double') typeVal <- 8
    if (type == 'short') typeVal <- 2
    if (type == 'char') typeVal <- 1
    if (is.null(typeVal)) stop('invalid type')
    if (!is.null(dimnames)) {
        rownames <- dimnames[[1]]
        colnames <- dimnames[[2]]
    } else {
        rownames <- NULL
        colnames <- NULL
    }
    if (is.null(backingfile))
    {
        stop('You must specify a backing file')
    }
    anon.backing <- ifelse( backingfile == '', TRUE, FALSE )
    if (anon.backing)
    {
        backingfile <- tempfile()
        backingpath = ""
#        backingpath <- dirname(backingfile)
#        backingfile <- basename(backingfile)
    }
    if (is.null(descriptorfile) && !anon.backing) 
    {
        warning(paste("No descriptor file given, it will be named",
                      paste(backingfile, '.desc', sep='')))
        descriptorfile <- paste(backingfile, '.desc', sep='')
    }
    if ( !anon.backing && ((basename(backingfile) != backingfile) ||
                               (basename(descriptorfile) != descriptorfile)) )
    {
        stop(paste("The path to the descriptor and backing file are",
                   "specified with the backingpath option"))
    }
    if (is.null(backingpath)) backingpath <- ''
    backingpath <- path.expand(backingpath)
    if (backingpath != "") {
      backingpath <- paste(backingpath, '', sep=.Platform$file.sep)
    }
    
    if(file.exists(paste(backingpath, backingfile, sep=.Platform$file.sep))){
        stop("Backing file already exists! Either remove or specify
             different backing file")
    }
    if (backingpath == "" && dirname(backingfile) == ".") 
      backingpath = paste(getwd(), "", sep=.Platform$file.sep)

    address <- CreateFileBackedBigMatrix(as.character(backingfile), 
                     as.character(backingpath), as.double(nrow), 
                     as.double(ncol), as.character(colnames), 
                     as.character(rownames), as.integer(typeVal), 
                     as.double(init), as.logical(separated))
    if (is.null(address))
    {
        stop("Error encountered when creating instance of type big.matrix")
    }
    x <- new("big.matrix", address=address)
    if (is.null(x))
    {
        stop("Error encountered when creating instance of type big.matrix")
    }
    if (is.null(descriptorfile) && !anon.backing)
    {
        warning(paste("A descriptor file has not been specified.  ",
                      "A descriptor named ", backingfile, 
                      ".desc will be created.", sep=''))
        descriptorfile <- paste(backingfile, ".desc", sep='' )
    }
    if (!anon.backing)
    {
        descriptorfilepath <- paste(backingpath, descriptorfile, 
                                    sep=.Platform$file.sep)
        if(binarydescriptor)
        {
            saveRDS(describe(x), file=descriptorfilepath)
        } else {
            dput(describe(x), descriptorfilepath)
        }
    }
    return(x)
}


#' @rdname big.matrix
#' @export
setGeneric('as.big.matrix', 
           function(x, type=NULL, separated=FALSE,
                    backingfile=NULL, backingpath=NULL,
                    descriptorfile=NULL, binarydescriptor=FALSE, shared=TRUE) standardGeneric('as.big.matrix'))


#' @title Convert to base R matrix
#' @description Extract values from a \code{big.matrix} object
#' and convert to a base R matrix object
#' @param x A big.matrix object
#' @export
setMethod('as.matrix', signature(x='big.matrix'),
          function(x) return(x[,]))


#' @template as.big.matrix_methods_template
NULL


setMethod('as.big.matrix', signature(x='matrix'),
          function(x, type, separated, backingfile, backingpath, descriptorfile,
                   binarydescriptor, shared)
          {
              if (!is.numeric(x)) {
                  warning("Casting to numeric type")
                  x <- matrix(as.numeric(x), nrow=nrow(x), dimnames=dimnames(x))
              }
              if (is.null(type)) type <- typeof(x)
              
#               if (type=="integer" | type=="double" | type=="short" | type=="char") 
              if (type %in% c("integer","double", "short", "char", "float"))
              {
                  y <- big.matrix(nrow=nrow(x), ncol=ncol(x), type=type, 
                                  init=NULL, dimnames=dimnames(x), separated=separated,
                                  backingfile=backingfile, backingpath=backingpath,
                                  descriptorfile=descriptorfile, binarydescriptor=binarydescriptor,
                                  shared=shared)
                  y[1:nrow(x),1:ncol(x)] <- x
                  junk <- gc() 
              } else stop('bigmemory: that type is not implemented.')
              return(y)
          })



setMethod('as.big.matrix', signature(x='data.frame'),
          function(x, type, separated, backingfile, backingpath, descriptorfile,
                   binarydescriptor, shared)
          {
              warning("Coercing data.frame to matrix via factor level numberings.")
              if (is.null(type)) type <- options()$bigmemory.default.type
#               if (type=="integer" | type=="double" | type=="short" | type=="char") 
              if (type %in% c("integer","double", "short", "char", "float"))
              {
                  y <- big.matrix(nrow=nrow(x), ncol=ncol(x), type=type, 
                                  init=NULL, dimnames=dimnames(x), separated=separated,
                                  backingfile=backingfile, backingpath=backingpath,
                                  descriptorfile=descriptorfile, binarydescriptor=binarydescriptor,
                                  shared=shared)
                  oldbtw <- options()$bigmemory.typecast.warning
                  options(bigmemory.typecast.warning=FALSE)
                  for (i in 1:ncol(x)) {
                      if (is.character(x[,i])) x[,i] <- factor(x[,i])
                      if (is.factor(x[,i])) x[,i] <- as.numeric(x[,i])
                      y[,i] <- x[,i]
                  }
                  options(bigmemory.typecast.warning=oldbtw)
                  junk <- gc() 
              } else stop('bigmemory: that type is not implemented.')
              return(y)
              
          })



setMethod('as.big.matrix', signature(x='vector'),
          function(x, type, separated, backingfile, backingpath, descriptorfile,
                   binarydescriptor, shared)
          {
              if (!is.numeric(x)) {
                  warning("Casting to numeric type")
                  x <- as.numeric(x)
              }
              x <- matrix(x, length(x), 1)
              warning("Coercing vector to a single-column matrix.")
              return(as.big.matrix(x, type, separated, backingfile, 
                                   backingpath, descriptorfile, binarydescriptor, shared))
          })


#' @rdname big.matrix
#' @export
setGeneric('is.big.matrix', function(x) standardGeneric('is.big.matrix'))

#' @rdname big.matrix
setMethod('is.big.matrix', signature(x='big.matrix'),
  function(x) return(TRUE))

#' @rdname big.matrix
setMethod('is.big.matrix', definition=function(x) return(FALSE))

  
colnames.bm <- function(x)
{
  ret <- GetColumnNamesBM(x@address)
  if (length(ret)==0) return(NULL)
  return(ret)
}

rownames.bm <- function(x)
{
  ret <- GetRowNamesBM(x@address)
  if (length(ret)==0) return(NULL)
  return(ret)
}

assign('colnames.bm<-', 
  function(x, value) {
      checkReadOnly(x)
      if (is.character(value)) {
        if (any(value=="")) {
          stop("empty strings prohibited in column names")
        }
      } else {
        if (!is.null(value)) {
          value <- as.character(value)
          warning("column names coerced to character")
        }
      }
      if (!is.null(value) & length(value) != ncol(x))
        stop("length of 'colnames' not equal to array extent.")
      SetColumnNames(x@address, value)
      return(x)
  })

assign('rownames.bm<-',
  function(x,value) {
      checkReadOnly(x)
      if (is.character(value)) {
        if (any(value=="")) {
          stop("empty strings prohibited in row names")
        }
      } else {
        if (!is.null(value)) {
          value <- as.character(value)
          warning("row names coerced to character")
        }
      }
      if (length(value) != nrow(x) & !is.null(value)) 
        stop("length of 'rownames' not equal to array extent.")
      SetRowNames(x@address, value)
      return(x)
  })

#' @title The Number of Rows/Columns of a big.matrix
#' @description \code{nrow} and \code{ncol} return the number of
#' rows or columns present in a \code{big.matrix} object.
#' @param x A big.matrix object
#' @return An integer of length 1
#' @docType methods
#' @rdname ncol-methods
#' @export
setMethod('ncol', signature(x="big.matrix"),
  function(x) return(CGetNcol(x@address)))

#' @rdname ncol-methods
#' @export
setMethod('nrow', signature(x="big.matrix"), 
  function(x) return(CGetNrow(x@address)))

#' @title Dimensions of a big.matrix object
#' @description Retrieve the dimensions of a \code{big.matrix} object
#' @param x A \code{big.matrix} object
#' @export
setMethod('dim', signature(x="big.matrix"),
  function(x) return(c(nrow(x), ncol(x))))

#' @title Length of a big.matrix object
#' @description Get the length of a \code{big.matrix} object
#' @param x A \code{big.matrix} object
#' @export
setMethod('length', signature(x="big.matrix"),
  function(x) return(prod(dim(x))))

GetElements.bm <- function(x, i, j, drop=TRUE)
{
  if (!is.numeric(i) & !is.character(i) & !is.logical(i))
    stop("row indices must be numeric, logical, or character vectors.")
  if (!is.numeric(j) & !is.character(j) & !is.logical(j))
    stop("column indices must be numeric, logical, or character vectors.")
  if (is.character(i))
    if (is.null(rownames(x))) stop("row names do not exist.")
    else i <- mmap(i, rownames(x))
  if (is.character(j))
    if (is.null(colnames(x))) stop("column names do not exist.")
    else j <- mmap(j, colnames(x))
  if (is.logical(i)) {
    if (length(i) != nrow(x))
      stop("row vector length must match the number of rows of the matrix.")
    i <- which(i)
  }
  if (is.logical(j)) {
    if (length(j) != ncol(x))
      stop(paste("column vector length must match the number of",
                 "columns of the matrix."))
    j <- which(j)
  }

  tempi <- CCleanIndices(as.double(i), as.double(nrow(x)))
  if (is.null(tempi[[1]])) stop("Illegal row index usage in extraction.\n")
  if (tempi[[1]]) i <- tempi[[2]]
  tempj <- CCleanIndices(as.double(j), as.double(ncol(x)))
  if (is.null(tempj[[1]])) stop("Illegal column index usage in extraction.\n")
  if (tempj[[1]]) j <- tempj[[2]]

  retList <- GetMatrixElements(x@address, as.double(j), as.double(i))
  mat = .addDimnames(retList, length(i), length(j), drop)
  return(mat)
}

# Function contributed by Peter Haverty at Genentech.
GetIndivElements.bm <- function(x,i) {
  # Check i
  if (is.logical(i)) {
    stop("Logical indices not allowed when subsetting by a matrix.")
  }
  if (ncol(i) != 2) {
    stop("When subsetting with a matrix, it must have two columns.")
  }
  if (is.character(i)) {
    if (is.null(rownames(x))) stop("row names do not exist.")
    if (is.null(colnames(x))) stop("column names do not exist.")
    i <- matrix(c(mmap(i[,1], rownames(x)), mmap(i[,2], colnames(x))), ncol=2)
  }
  tempi <- CCleanIndices(as.double(i[,1]), as.double(nrow(x)))
  if (is.null(tempi[[1]])) stop("Illegal row index usage in assignment.\n")
  if (tempi[[1]]) i[,1] <- tempi[[2]]
  tempj <- CCleanIndices(as.double(i[,2]), as.double(ncol(x)))
  if (is.null(tempj[[1]])) stop("Illegal column index usage in assignment.\n")
  if (tempj[[1]]) i[,2] <- tempj[[2]]

  return(GetIndivMatrixElements(x@address, as.double(i[,2]),
                                as.double(i[,1])))
}


GetCols.bm <- function(x, j, drop=TRUE)
{
  if (!is.numeric(j) & !is.character(j) & !is.logical(j))
    stop("column indices must be numeric, logical, or character vectors.")
  if (is.character(j))
    if (is.null(colnames(x))) stop("column names do not exist.")
    else j <- mmap(j, colnames(x))
  if (is.logical(j)) {
    if (length(j) != ncol(x))
      stop(paste("column vector length must match the number of",
                 "columns of the matrix."))
    j <- which(j)
  }
  
  tempj <- CCleanIndices(as.double(j), as.double(ncol(x)))
  if (is.null(tempj[[1]])) stop("Illegal column index usage in extraction.\n")
  if (tempj[[1]]) j <- tempj[[2]]
  
  retList <- GetMatrixCols(x@address, as.double(j))
  mat = .addDimnames(retList, nrow(x), length(j), drop)
  return(mat)
}

GetRows.bm <- function(x, i, drop=TRUE)
{
  if (!is.numeric(i) & !is.character(i) & !is.logical(i))
    stop("row indices must be numeric, logical, or character vectors.")
  if (is.character(i))
    if (is.null(rownames(x))) stop("row names do not exist.")
    else i <- mmap(i, rownames(x))
  if (is.logical(i)) {
    if (length(i) != nrow(x))
      stop("row vector length must match the number of rows of the matrix.")
    i <- which(i)
  }
  tempi <- CCleanIndices(as.double(i), as.double(nrow(x)))
  if (is.null(tempi[[1]])) stop("Illegal row index usage in extraction.\n")
  if (tempi[[1]]) i <- tempi[[2]]

  retList <- GetMatrixRows(x@address, as.double(i))
  mat = .addDimnames(retList, length(i), ncol(x), drop)
  return(mat)
}

GetAll.bm <- function(x, drop=TRUE)
{
  retList <- GetMatrixAll(x@address)
  mat = .addDimnames(retList, nrow(x), ncol(x), drop)
  return(mat)
}

#' @title Extract or Replace 
#' @description Extract or replace big.matrix elements
#' @name Extract,big.matrix
#' @param x A \code{big.matrix object}
#' @param i Indices specifying the rows
#' @param j Indices specifying the columns
#' @param drop Logical indication if reduce to minimum dimensions
#' @param value typically an array-like R object of similar class
#' @docType methods
#' @rdname extract-methods
#' @aliases [,big.matrix,ANY,ANY,missing-method
#' @export
setMethod("[",
  signature(x = "big.matrix", drop = "missing"),
  function(x, i, j, drop) return(GetElements.bm(x, i, j)))


#' @rdname extract-methods
#' @export
setMethod("[",
  signature(x = "big.matrix", drop = "logical"),
  function(x, i, j, drop) return(GetElements.bm(x, i, j, drop)))


#' @rdname extract-methods
#' @export
setMethod("[",
  signature(x = "big.matrix", i="missing", drop = "missing"),
  function(x, i, j, drop) return(GetCols.bm(x, j)))


#' @rdname extract-methods
#' @export
setMethod("[",
  signature(x = "big.matrix", i="missing", drop = "logical"),
  function(x, i, j, drop) return(GetCols.bm(x, j, drop)))


#' @rdname extract-methods
#' @export
setMethod("[",
  signature(x = "big.matrix", j="missing", drop = "missing"),
  function(x, i, j, drop) return(GetRows.bm(x, i)))


#' @rdname extract-methods
#' @export
setMethod("[",
  signature(x = "big.matrix", j="missing", drop = "logical"),
  function(x, i, j, drop) return(GetRows.bm(x, i, drop)))


#' @rdname extract-methods
#' @export
setMethod("[",
  signature(x = "big.matrix", i="missing", j="missing", drop = "missing"),
  function(x, i, j, drop) return(GetAll.bm(x)))


#' @rdname extract-methods
#' @export
setMethod("[",
  signature(x = "big.matrix", i="missing", j="missing", drop = "logical"),
  function(x, i, j, drop) return(GetAll.bm(x, drop)))

# Function contributed by Peter Haverty at Genentech.
#' @rdname extract-methods
#' @export
setMethod('[',
  signature(x = "big.matrix",i="matrix",j="missing",drop="missing"),
  function(x, i, j, drop) return(GetIndivElements.bm(x, i)))


#' @importFrom stats na.omit
SetElements.bm <- function(x, i, j, value)
{
  checkReadOnly(x)
  if (!is.numeric(i) & !is.character(i) & !is.logical(i))
    stop("row indices must be numeric, logical, or character vectors.")
  if (!is.numeric(j) & !is.character(j) & !is.logical(j))
    stop("column indices must be numeric, logical, or character vectors.")
  if (is.character(i))
    if (is.null(rownames(x))) stop("row names do not exist.")
    else i <- mmap(i, rownames(x))
  if (is.character(j))
    if (is.null(colnames(x))) stop("column names do not exist.")
    else j <- mmap(j, colnames(x))
  if (is.logical(i)) {
    if (length(i) != nrow(x))
      stop("row vector length must match the number of rows of the matrix.")
    i <- which(i)
  }
  if (is.logical(j)) {
    if (length(j) != ncol(x))
      stop(paste("column vector length must match the number of",
                 "columns of the matrix."))
    j <- which(j)
  }

  tempi <- CCleanIndices(as.double(i), as.double(nrow(x)))
  if (is.null(tempi[[1]])) stop("Illegal row index usage in assignment.\n")
  if (tempi[[1]]) i <- tempi[[2]]
  tempj <- CCleanIndices(as.double(j), as.double(ncol(x)))
  if (is.null(tempj[[1]])) stop("Illegal column index usage in assignment.\n")
  if (tempj[[1]]) j <- tempj[[2]]

  if ( options()$bigmemory.typecast.warning &&
       ((typeof(value) == "double") && (typeof(x) != "double") ||
       (typeof(value) == "integer" && (typeof(x) != "double" && 
                                           typeof(x) != "float" &&
                                           typeof(x) != "integer")) || 
       (typeof(value) == "double" && (typeof(x) == "float")) 
       )) 
  {
    warning(cat("Assignment will down cast from ", typeof(value), " to ",
                typeof(x), "\nHint: To remove this warning type:  ",
                "options(bigmemory.typecast.warning=FALSE)\n", sep=''))
  }

  totalts <- as.double(length(i)) * as.double(length(j))
  # If we are assigning from a matrix, make sure the dimensions agree.
  if (is.matrix(value))
  {
    if (ncol(value) != length(j) | nrow(value) != length(i)) 
    {
      stop("Matrix dimensions do not agree with big.matrix instance set size.")
    }
  } else if (length(value) != totalts) {
    # Otherwise, make sure we are assigning the correct number of things
    # (rep if necessary)
    numReps <- totalts / length(value)
    if (numReps != round(numReps)) 
    {
      stop(paste("number of items to replace is not a multiple of",
                 "replacement length"))
    }
  }
  if (typeof(x) != 'double') {
    integerVals = na.omit(as.integer(value))
    if ( sum(integerVals == na.omit(as.integer(value))) !=
         length(integerVals) | is.factor(value)) 
    {
      warning("non-integer (possibly Inf or -Inf) typecast to integer")
    }
  }
  # Note: we pass doubles as doubles, but anything else as integers.
#   if (typeof(x) == 'double') {
#     SetMatrixElements(x@address, as.double(j), as.double(i), 
#           as.double(value))
#   } else {
#     SetMatrixElements(x@address, as.double(j), as.double(i), 
#           as.integer(value))
#   }
  
  switch(typeof(x),
         'double' = {SetMatrixElements(x@address, as.double(j), as.double(i), 
                                      as.double(value))},
         'float' = {SetMatrixElements(x@address, as.double(j), as.double(i), 
                                     as.double(value))},
         SetMatrixElements(x@address, as.double(j), as.double(i), 
                           as.integer(value))
         )
  
  return(x)
}

SetIndivElements.bm <- function(x, i, value) {
  # Check i
  checkReadOnly(x)
  if (is.logical(i)) {
    stop("Logical indices not allowed when subsetting by a matrix.")
  }
  if (ncol(i) != 2) {
    stop("When subsetting with a matrix, it must have two columns.")
  }
  if (is.character(i)) {
    if (is.null(rownames(x))) stop("row names do not exist.")
    if (is.null(colnames(x))) stop("column names do not exist.")
    i <- matrix(c(mmap(i[,1], rownames(x)), mmap(i[,2], colnames(x))), ncol=2)
  }
  tempi <- CCleanIndices(as.double(i[,1]), as.double(nrow(x)))
  if (is.null(tempi[[1]])) stop("Illegal row index usage in assignment.\n")
  if (tempi[[1]]) i[,1] <- tempi[[2]]
  tempj <- CCleanIndices(as.double(i[,2]), as.double(ncol(x)))
  if (is.null(tempj[[1]])) stop("Illegal column index usage in assignment.\n")
  if (tempj[[1]]) i[,2] <- tempj[[2]]

  # Check value length, rep as necessary
  if (length(value) > nrow(i) || nrow(i) %% length(value) != 0) {
    stop("number of items to replace is not a multiple of replacement length")
  }
  if (length(value) < nrow(i)) {
    value = rep(value, nrow(i) %/% length(value))
  }

  # Give typecast warning if necessary
  if ( options()$bigmemory.typecast.warning &&
       ((typeof(value) == "double") && (typeof(x) != "double") ||
       (typeof(value) == "integer" &&
        (typeof(x) != "double" && typeof(x) != "integer"))) ||
       (typeof(value) == "double" && (typeof(x) == "float"))
       )
  {
    warning(cat("Assignment will down cast from ", typeof(value), " to ",
                typeof(x), "\nHint: To remove this warning type:  ",
                "options(bigmemory.typecast.warning=FALSE)\n", sep=''))
  }

  # Call appropriate .Call C++
#   if (typeof(x) == 'double') {
#     SetIndivMatrixElements(x@address, as.double(i[,2]),
#       as.double(i[,1]), as.double(value))
#   } else {
#     SetIndivMatrixElements(x@address, as.double(i[,2]),
#       as.double(i[,1]), as.integer(value))
#   }
  
  switch(typeof(x),
         'double' = {SetIndivMatrixElements(x@address, as.double(i[,2]),
                                            as.double(i[,1]), as.double(value))},
         'float' = {SetIndivMatrixElements(x@address, as.double(i[,2]),
                                           as.double(i[,1]), as.single(value))},
         SetIndivMatrixElements(x@address, as.double(i[,2]),
                                as.double(i[,1]), as.integer(value))
  )
  
  return(x)
}

#' @importFrom stats na.omit
SetCols.bm <- function(x, j, value)
{
  checkReadOnly(x)
  if (!is.numeric(j) & !is.character(j) & !is.logical(j))
    stop("column indices must be numeric, logical, or character vectors.")
  if (is.character(j))
    if (is.null(colnames(x))) stop("column names do not exist.")
    else j <- mmap(j, colnames(x))
  if (is.logical(j)) {
    if (length(j) != ncol(x))
      stop(paste("column vector length must match the number of",
                 "columns of the matrix."))
    j <- which(j)
  }

  tempj <- CCleanIndices(as.double(j), as.double(ncol(x)))
  if (is.null(tempj[[1]])) stop("Illegal column index usage in extraction.\n")
  if (tempj[[1]]) j <- tempj[[2]]

  if ( options()$bigmemory.typecast.warning &&
       ((typeof(value) == "double") && (typeof(x) != "double") ||
       (typeof(value) == "integer" &&
        (typeof(x) != "double" && typeof(x) != "integer")) || 
       (typeof(value) == "double" && (typeof(x) == "float"))) 
       )
  {
    warning(cat("Assignment will down cast from ", typeof(value), " to ",
                typeof(x), "\nHint: To remove this warning type:  ",
                "options(bigmemory.typecast.warning=FALSE)\n", sep=''))
  }

  totalts <- as.double(nrow(x)) * as.double(length(j))
  # If we are assigning from a matrix, make sure the dimensions agree.
  if (is.matrix(value)){
    if (ncol(value) != length(j) | nrow(value) != nrow(x)) 
    {
      stop("Matrix dimensions do not agree with big.matrix instance set size.")
    }
  } 
  else if (length(value) != totalts) 
  {
    # Otherwise, make sure we are assigning the correct number of things
    # (rep if necessary)
    numReps <- totalts / length(value)
    if (numReps != round(numReps)) {
      stop(paste("number of items to replace is not a multiple of",
                 "replacement length"))
    }
  }
  if (typeof(x) != 'double') {
    integerVals = na.omit(as.integer(value))
    if ( sum(integerVals == na.omit(as.integer(value))) !=
         length(integerVals) | is.factor(value)) {
      warning("non-integer (possibly Inf or -Inf) typecast to integer")
    }
  }
  # Note: we pass doubles as doubles, but anything else as integers.
#   if (typeof(x) == 'double') 
#   {
#     SetMatrixCols(x@address, as.double(j), as.double(value))
#   } 
#   else 
#   {
#     SetMatrixCols(x@address, as.double(j), as.integer(value))
#   }
  
  switch(typeof(x),
         'double' = {SetMatrixCols(x@address, as.double(j), as.double(value))},
         'float' = {SetMatrixCols(x@address, as.double(j), as.single(value))},
         SetMatrixCols(x@address, as.double(j), as.integer(value))
  )
  
  return(x)
}

#' @importFrom stats na.omit
SetRows.bm <- function(x, i, value) 
{
  checkReadOnly(x)
  if (!is.numeric(i) & !is.character(i) & !is.logical(i))
    stop("row indices must be numeric, logical, or character vectors.")
  if (is.character(i))
    if (is.null(rownames(x))) stop("row names do not exist.")
    else i <- mmap(i, rownames(x))
  if (is.logical(i)) {
    if (length(i) != nrow(x))
      stop("row vector length must match the number of rows of the matrix.")
    i <- which(i)
  }

  tempi <- CCleanIndices(as.double(i), as.double(nrow(x)))
  if (is.null(tempi[[1]])) stop("Illegal row index usage in extraction.\n")
  if (tempi[[1]]) i <- tempi[[2]]
  
  if ( options()$bigmemory.typecast.warning &&
       ((typeof(value) == "double") && (typeof(x) != "double") ||
       (typeof(value) == "integer" &&
        (typeof(x) != "double" && typeof(x) != "integer"))  || 
       (typeof(value) == "double" && (typeof(x) == "float")))
  )
  {
    warning(cat("Assignment will down cast from ", typeof(value), " to ",
                typeof(x), "\nHint: To remove this warning type:  ",
                "options(bigmemory.typecast.warning=FALSE)\n", sep=''))
  }

  # Note: i may be a mwhich statement in which case we _must_ ensure
  # that we disable read locking before it is evaluated or we will
  # have a race condition.  - Jay and Mike.

  totalts <- as.double(length(i)) * as.double(ncol(x))
  # If we are assigning from a matrix, make sure the dimensions agree.
  if (is.matrix(value))
  {
    if (ncol(value) != ncol(x) | nrow(value) != length(i)) 
    {
      stop("Matrix dimensions do not agree with big.matrix instance set size.")
    }
  } 
  else if (length(value) != totalts) 
  {
    # Otherwise, make sure we are assigning the correct number of things
    # (rep if necessary)
    numReps <- totalts / length(value)
    if (numReps != round(numReps)) 
    {
      stop(paste("number of items to replace is not a multiple of",
                 "replacement length"))
    }
  }
  if (typeof(x) != 'double') 
  {
    integerVals <- na.omit(as.integer(value))
    if ( sum(integerVals == na.omit(as.integer(value))) !=
         length(integerVals) | is.factor(value)) {
      warning("non-integer (possibly Inf or -Inf) typecast to integer")
    }
  }
  # Note: we pass doubles as doubles, but anything else as integers.
#   if (typeof(x) == 'double') {
#     SetMatrixRows(x@address, as.double(i), as.double(value))
#   } 
#   else 
#   {
#     SetMatrixRows(x@address, as.double(i), as.integer(value))
#   }
  
  switch(typeof(x),
         'double' = {SetMatrixRows(x@address, as.double(i), as.double(value))},
         'float' = {SetMatrixRows(x@address, as.double(i), as.single(value))},
         SetMatrixRows(x@address, as.double(i), as.integer(value))
  )
  
  return(x)
}

#' @importFrom stats na.omit
SetAll.bm <- function(x, value) 
{
  checkReadOnly(x)
  if ( options()$bigmemory.typecast.warning &&
       ((typeof(value) == "double") && (typeof(x) != "double") ||
       (typeof(value) == "integer" &&
        (typeof(x) != "double" && typeof(x) != "integer"))  || 
       (typeof(value) == "double" && (typeof(x) == "float")))
  )
  {
    warning(cat("Assignment will down cast from ", typeof(value), " to ",
                typeof(x), "\nHint: To remove this warning type:  ",
                "options(bigmemory.typecast.warning=FALSE)\n", sep=''))
  }

  totalts <- as.double(nrow(x)) * as.double(ncol(x))
  # If we are assigning from a matrix, make sure the dimensions agree.
  if (is.matrix(value))
  {
    if (ncol(value) != ncol(x) | nrow(value) != nrow(x)) 
    {
      stop("Matrix dimensions do not agree with big.matrix instance set size.")
    }
  } 
  else if (length(value) != totalts) 
  {
    # Otherwise, make sure we are assigning the correct number of things
    # (rep if necessary)
    numReps <- totalts / length(value)
    if (numReps != round(numReps)) {
      stop(paste("number of items to replace is not a multiple of", 
                 "replacement length"))
    }
  }
  if (typeof(x) != 'double') 
  {
    integerVals = na.omit(as.integer(value))
    if ( sum(integerVals == na.omit(as.integer(value))) !=
         length(integerVals) | is.factor(value)) 
    {
      warning("non-integer (possibly Inf or -Inf) typecast to integer")
    }
  }
  # Note: we pass doubles as doubles, but anything else as integers.
#   if (typeof(x) == 'double') 
#   {
#     SetMatrixAll(x@address, as.double(value))
#   } 
#   else 
#   {
#     SetMatrixAll(x@address, as.integer(value))
#   }
  
  switch(typeof(x),
         'double' = {SetMatrixAll(x@address, as.double(value))},
         'float' = {SetMatrixAll(x@address, as.single(value))},
         SetMatrixAll(x@address, as.integer(value))
  )
  
  return(x)
}

#' @rdname extract-methods
#' @export
setMethod('[<-',
  signature(x = "big.matrix"),
  function(x, i, j, value) return(SetElements.bm(x, i, j, value)))

#' @rdname extract-methods
#' @export
setMethod('[<-',
  signature(x = "big.matrix", i="missing"),
  function(x, i, j, value) return(SetCols.bm(x, j, value)))

#' @rdname extract-methods
#' @export
setMethod('[<-',
  signature(x = "big.matrix", j="missing"),
  function(x, i, j, value) return(SetRows.bm(x, i, value)))

#' @rdname extract-methods
#' @export
setMethod('[<-',
  signature(x = "big.matrix", i="missing", j="missing"),
  function(x, i, j, value) return(SetAll.bm(x, value)))

# Function contributed by Peter Haverty at Genentech.
#' @rdname extract-methods
#' @export
setMethod('[<-',
  signature(x = "big.matrix",i="matrix",j="missing"),
  function(x, i, j, value) return(SetIndivElements.bm(x, i, value)))

#' @title The Type of a big.matrix Object
#' @description \code{typeof} returns the storage type of a 
#' \code{big.matrix} object
#' @param x A \code{big.matrix} object
#' @export
setMethod('typeof', signature(x="big.matrix"),
  function(x) {
      return(GetTypeString(x@address))
      }
  )


#' @title Check if Float
#' @description Check to see if the elements of a big.matrix object are floats.
#' @param x An object to be evaluated if float
#' @export
setGeneric('is.float', function(x){
    standardGeneric('is.float')
})

#' @title Is Float?
#' @description Check if R numeric value has float flag
#' @param x A numeric value
setMethod('is.float', signature(x='numeric'),
          function(x){
              if(is.null(attr(x, 'Csingle'))){
                  return(FALSE)
              }else{
                  bool <- attr(x, 'Csingle')
                  return(bool)
              }
          })

#' @title Return First or Last Part of a big.matrix Object
#' @description Returns the first or last parts of a \code{big.matrix}
#' object.
#' @param x A big.matrix object
#' @param n A single integer for the number of rows to return
#' @docType methods
#' @rdname head-methods
#' @export
setMethod('head', signature(x="big.matrix"),
  function(x, n = 6) {
    n <- min(as.integer(n), nrow(x))
    if (n<1 | n>nrow(x)) stop("n must be between 1 and nrow(x)")
    return(x[1:n,])
  })


#' @rdname head-methods
#' @export
setMethod('tail', signature(x="big.matrix"),
  function(x, n = 6) {
    n <- min(as.integer(n), nrow(x))
    if (n<1 | n>nrow(x)) stop("n must be between 1 and nrow(x)")
    return(x[(nrow(x)-n+1):nrow(x),])
  })

#' @title Print Values
#' @description \code{print} will print out the elements within
#' a \code{big.matrix} object.
#' @note By default, this will only return the \code{head} of a big.matrix
#' to prevent console overflow.  If you trun off the bigmemory.print.warning
#' option then it will convert to a base R matrix and print all elements.
#' @param x A \code{big.matrix} object
#' @export
setMethod('print', signature(x='big.matrix'), 
  function(x) {
    if (options()$bigmemory.print.warning==TRUE)
    {
      cat("Warning: This is not advised.  Here is the head of the matrix:\n")
      print(head(x))
    }
    else
    {
      # Should change this to a C print function, unfortunately, for proper
      # formatting, this means we would also have to pass the terminal
      # width.
      print(x[,])
    }
  })

###################################################################
# mwhich()
#
# x big.matrix  
# cols  is.numeric or is.character
# vals  list of scalar or 2-vectors otherwise
# comps could be missing, in which case we'll fill in 'eq' in signature,
#       a list of comparisons matching dim of associated vals component

#' @template mwhich_template
#' @export
setGeneric('mwhich', function(x, cols, vals, comps, op = 'AND')
  standardGeneric('mwhich'))

# add mwhich-methods roxygen
# setting to NULL avoids the redundant usage statements
#' @template mwhich_methods_template
NULL

setMethod('mwhich',
  signature(x='big.matrix', op='character'),
  function(x, cols, vals, comps, op) {
    return(mwhich.internal(x, cols, vals, comps, op, MWhichBigMatrix))
  })

# @rdname mwhich-methods
setMethod('mwhich',
  signature(x='matrix', op='character'),
  function(x, cols, vals, comps, op)
  {
    if (is.integer(x))
      return(mwhich.internal(x, cols, vals, comps, op, MWhichRIntMatrix))
    if (is.numeric(x))
      return(mwhich.internal(x, cols, vals, comps, op, MWhichRNumericMatrix))
    stop("Unsupported matrix type given to mwhich")
  })

# @rdname mwhich-methods
setMethod('mwhich',
  signature(x='big.matrix', op='missing'),
  function(x, cols, vals, comps)
    return(mwhich.internal(x, cols, vals, comps, op='AND', 
                           whichFuncName=MWhichBigMatrix)))

# @rdname mwhich-methods
setMethod('mwhich',
  signature(x='matrix', op='missing'),
  function(x, cols, vals, comps)
  {
    if (is.integer(x))
      return(mwhich.internal(x, cols, vals, comps, op='AND', 
                             whichFuncName=MWhichRIntMatrix))
    if (is.numeric(x))
      return(mwhich.internal(x, cols, vals, comps, op='AND', 
                             whichFuncName=MWhichRNumericMatrix))
    stop("Unsupported matrix type given to mwhich")
  })

mwhich.internal <- function(x, cols, vals, comps, op, whichFuncName) 
{
  cols <- cleanupcols(cols, ncol(x), colnames(x))
  if (length(setdiff(cols, 1:ncol(x))) > 0)
    stop('Invalid column(s) in which()')

  # if vals or comps are not lists but are length 1 or 2, make them
  # trivial lists.
  if ( !is.list(vals) & 
       (length(vals)==1 || length(vals)==2) ) {
    vals <- list(vals)
  } else {
    if (!is.list(vals)) stop('vals should be a list')
  }
  if ( !is.list(comps) &
       (length(comps)==1 || length(comps)==2)) {
    comps <- list(comps)
  } else {
    if (!is.list(comps)) stop('comps should be a list')
  }

  # Replicate vals or comps if appropriate.
  if (length(cols)!=length(vals)) {
    if (length(vals)==1) {
      vals <- data.frame(matrix(unlist(vals), length(vals), length(cols)))
    } else stop('length(vals) must be 1 or length(cols)')
  }
  if (length(cols)!=length(comps)) {
    if (length(comps)==1) {
      comps <- data.frame(matrix(unlist(comps), length(comps), length(cols)),
                          stringsAsFactors=FALSE)
    } else stop('length(comps) must be 1 or length(cols)')
  }
  if (length(comps)!=length(vals))
    stop('length of comps must equal length of vals')
  if (any(!unlist(lapply(comps, is.character))) ||
      any(!(unlist(comps) %in% c('eq', 'neq', 'le', 'lt', 'ge', 'gt')))) {
    stop('comps must contain eq, neq, le, lt, ge, or gt')
  }

  testCol <- cols
  opVal <- 0
  if (op == 'OR') opVal <- 1
  minVal <- rep(NA, length(cols))
  maxVal <- rep(NA, length(cols))
  chkmin <- rep(0, length(cols))
  chkmax <- rep(0, length(cols))

  for (i in 1:length(cols)) {

    if (length(vals[[i]])==1) {
      # Here, we have the easy comparisons.
      if (is.na(vals[[i]]) && (comps[[i]]!='eq' && comps[[i]]!='neq'))
        stop('NA comparison limited to eq and neq, not le, lt, gt, or ge')
      if (length(comps[[i]])==1) {
        if (comps[[i]]=='eq' || comps[[i]]=='neq') {
          minVal[i] <- vals[[i]]
          maxVal[i] <- vals[[i]]
        }
        if (comps[[i]]=='neq') {
          chkmin[i] <- -1
          chkmax[i] <- -1            # Not used, but....
        }
        if (comps[[i]]=='ge' || comps[[i]]=='gt') {
          minVal[i] <- vals[[i]]
          maxVal[i] <- Inf
          if (comps[[i]]=='gt') chkmin[i] <- 1
        }
        if (comps[[i]]=='le' || comps[[i]]=='lt') {
          minVal[i] <- -Inf
          maxVal[i] <- vals[[i]]
          if (comps[[i]]=='lt') chkmax[i] <- 1
        }
      } else stop('vals/comps must be componentwise of same dimension')
    } else {
      # Here, we have two vals and two comps
      if (any(is.na(vals[[i]]))) stop('NAs not allowed in interval comparison')
      minVal[i] <- vals[[i]][1]
      maxVal[i] <- vals[[i]][2]
      if (comps[[i]][1]=='gt') chkmin[i] <- 1
      if (comps[[i]][2]=='lt') chkmax[i] <- 1
      if (comps[[i]][1]!='gt' && comps[[i]][1]!='ge')
        stop('invalid comparison of lower bound')
      if (comps[[i]][2]!='lt' && comps[[i]][2]!='le')
        stop('invalid comparison of upper bound')
    }

  } # End of the for loop

  ##### The new C function has new vectors chkmin and chkmax;
  ##### the value 0 indicates comparison with equality,
  ##### the value 1 indicates a strict inequality,
  ##### the value -1 indicates a 'neq' check;
  ##### if is.na checking is required, only the minVal needs to be
  ##### used, with chkmin = 0 being is.na and chkmin = 1 being !is.na.

  ret = NULL
  #if (whichFuncName == 'MWhichBigMatrix')
  if (is.big.matrix(x))
    ret = whichFuncName(x@address, as.double(testCol), 
                as.double(minVal), as.double(maxVal), 
                as.integer(chkmin), as.integer(chkmax), as.integer(opVal))
  else
    ret = whichFuncName(x, nrow(x),
                as.double(testCol), 
                as.double(minVal), as.double(maxVal), 
                as.integer(chkmin), as.integer(chkmax), as.integer(opVal))

  return(ret)
}


#' @title Dimnames of a big.matrix Object
#' @description Retrieve or set the dimnames of an object
#' @param x A big.matrix object
#' @param value A possible value for \code{dimnames(x)}
#' @docType methods
#' @rdname dimnames-methods
#' @export
setMethod('dimnames', signature(x = "big.matrix"),
  function(x) return(list(rownames.bm(x), colnames.bm(x))))


#' @rdname dimnames-methods
#' @export
setMethod('dimnames<-', signature(x = "big.matrix", value='list'),
  function(x, value) {
    if (options()$bigmemory.allow.dimnames) {
      rownames.bm(x) <- value[[1]]
      colnames.bm(x) <- value[[2]]
    } else {
      stop(paste("Changing dimnames is not allowed; to override, please set",
                 "options(bigmemory.allow.dimnames=TRUE)."))
    }
    return(x)
  })


#' @template write.big.matrix_template
#' @export
setGeneric('write.big.matrix', 
           function(x, filename, row.names=FALSE, col.names=FALSE, sep=",") 
               standardGeneric('write.big.matrix'))

#' @rdname write.big.matrix
setMethod('write.big.matrix', signature(x='big.matrix',filename='character'),
          function(x, filename, row.names, col.names, sep)
          {
              if (is.character(row.names))
                  stop("You must set the row names before writing.\n")
              if (is.character(col.names))
                  stop("You must set the column names before writing.\n")
              if (row.names & !HasRowColNames(x@address)[1]) {
                  row.names <- FALSE
                  warning("No row names exist, overriding your row.names option.\n")
              }
              if (col.names & !HasRowColNames(x@address)[2]) {
                  col.names <- FALSE
                  warning("No column names exist, overriding your col.names option.\n")
              }
              WriteMatrix(x@address, filename, as.logical(row.names), 
                    as.logical(col.names), sep)
              invisible(NULL)
          })


#' @rdname write.big.matrix
#' @export
setGeneric('read.big.matrix', 
  function(filename, sep=',', header=FALSE, col.names=NULL, row.names=NULL, 
           has.row.names=FALSE, ignore.row.names=FALSE, type=NA, skip=0, 
           separated=FALSE, backingfile=NULL, backingpath=NULL, 
           descriptorfile=NULL, binarydescriptor=FALSE, extraCols=NULL,
           shared=TRUE) 
  standardGeneric('read.big.matrix'))

#' @@importFrom stats na.omit
#' @rdname write.big.matrix
setMethod('read.big.matrix', signature(filename='character'),
  function(filename, sep, header, col.names, row.names, has.row.names, 
           ignore.row.names, type, skip, separated, backingfile, backingpath, 
           descriptorfile, binarydescriptor, extraCols, shared=TRUE)
  {
    if (!is.logical(header))
      stop("header argument must be logical")
    if (is.logical(col.names) || is.logical(row.names))
      stop("row.names and col.names, if used, must only be vectors of names (not logicals).")
    if ( (header || is.character(col.names)) && is.numeric(extraCols) )
    {
      stop(paste("When column names are specified, extraCols must be the names",
                 "of the extra columns."))
    }
    if (!header && is.null(col.names) && is.character(extraCols))
      stop(paste("No header and no column names were specified, so extraCols",
           "must be an integer."))
    if (!file.exists(filename))
      stop(paste("The file", filename, "could not be found"))
    headerOffset <- as.numeric(header)
    colNames <- NULL
    if (header) {
      colNames <- unlist(strsplit(
        scan(filename, what='character', skip=skip, nlines=1, sep="\n", 
             quiet=TRUE), split=sep))
      colNames <- gsub("\"", "", colNames, perl=TRUE)
      colNames <- gsub("\'", "", colNames, perl=TRUE)
      if (is.na(colNames[1])) colNames <- colNames[-1]
      if (is.character(col.names)) {
        warning("Using supplied column names and skipping the header row.\n")
        colNames <- col.names
      } else {
        if (!is.null(col.names))
          stop("Invalid header/col.names usage (col.names must be a vector of names if used).\n")
      }
    } else {
      if (is.character(col.names)) colNames <- col.names
    }

    # Get the first line of data
    firstLine <- scan(filename, what='character', skip=(skip+headerOffset),
      nlines=1, sep="\n", quiet=TRUE)
    firstLineVals <- unlist(strsplit(firstLine, split=sep))
    numFields <- length(firstLineVals)
    firstLineVals[firstLineVals=="NA"] <- NA
    if (length(firstLineVals) < numFields) {
      firstLineVals <- c(firstLineVals, NA)
    }

    # At this point, we assume there are length(colNames) columns of data if
    # available, otherwise, figure it out.
    if (!is.null(colNames)) numCols <- length(colNames)
    else {
      numCols <- length(firstLineVals) - has.row.names 
    }

    if (length(firstLineVals) - has.row.names != numCols)
      stop("Dimension mismatch between header row and first data row.\n")

    rowNames <- NULL
    if (!is.null(row.names)) {
      if (is.character(row.names)) {
        rowNames <- row.names
        ignore.row.names <- TRUE
      } else { stop("Invalid row.names (must be a vector of names if used).\n") }
    }

    if (is.na(type)) {
      type <- 'double'
      if (has.row.names) firstLineVals <- firstLineVals[-1]
      if (sum(na.omit(as.integer(firstLineVals)) ==
              na.omit(as.double(firstLineVals))) ==
          numCols ) 
        type <- 'integer'
      warning(paste("Because type was not specified, we chose", type,
                    "based on the first line of data."))
    }

    lineCount <- CCountLines(filename) - skip - headerOffset
    numRows <- lineCount
    createCols <- numCols
    if (is.numeric(extraCols)) createCols <- createCols + extraCols
    if (is.character(extraCols)) {
      createCols <- createCols + length(extraCols)
      colNames <- c(colNames, extraCols)
    }

    bigMat <- big.matrix(nrow=numRows, ncol=createCols, type=type,
                         dimnames=list(rowNames, colNames), init=NULL, 
                         separated=separated, backingfile=backingfile,
                         backingpath=backingpath,
                         descriptorfile=descriptorfile,
                         binarydescriptor=binarydescriptor, shared=TRUE)

    # has.row.names indicates whether or not there are row names;
    # we take ignore.row.names from the user, but pass (essentially)
    # use.row.names (which is !ignore.row.names) to C:
    ReadMatrix(
          as.character(filename), 
          bigMat@address, 
          as.double(skip+headerOffset), 
          as.double(numRows), 
          as.double(numCols), 
          as.character(sep), 
          as.logical(has.row.names),
          as.logical(!ignore.row.names))

    return(bigMat)
  })


#' @rdname big.matrix
#' @export
setGeneric('is.separated', function(x) standardGeneric('is.separated'))

#' @rdname big.matrix
setMethod('is.separated', signature(x='big.matrix'),
  function(x) return(IsSeparated(x@address)))

cleanupcols <- function(cols=NULL, nc=NULL, colnames=NULL) {
  if (is.null(cols)) cols <- 1:nc
  else {
    if (!is.numeric(cols) & !is.character(cols) & !is.logical(cols))
      stop("column indices must be numeric, logical, or character vectors.")
    if (is.character(cols))
      if (is.null(colnames)) stop("column names do not exist.")
      else cols <- mmap(cols, colnames)
    if (is.logical(cols)) {
      if (length(cols) != nc)
        stop(paste("column vector length must match the number of",
                   "columns of the matrix."))
      cols <- which(cols)
    }
    tempj <- CCleanIndices(as.double(cols), as.double(nc))
    if (is.null(tempj[[1]])) stop("Illegal column index usage in extraction.\n")
    if (tempj[[1]]) cols <- tempj[[2]]
  }
  return(cols)
}

cleanuprows <- function(rows=NULL, nr=NULL, rownames=NULL) {
  if (is.null(rows)) rows <- 1:nr
  else {
    if (!is.numeric(rows) & !is.character(rows) & !is.logical(rows))
      stop("column indices must be numeric, logical, or character vectors.")
    if (is.character(rows))
      if (is.null(rownames)) stop("row names do not exist.")
      else rows <- mmap(rows, rownames)
    if (is.logical(rows)) {
      if (length(rows) != nr)
        stop(paste("row vector length must match the number of",
                   "rows of the matrix."))
      rows <- which(rows)
    }
    tempj <- CCleanIndices(as.double(rows), as.double(nr))
    if (is.null(tempj[[1]])) stop("Illegal row index usage in extraction.\n")
    if (tempj[[1]]) rows <- tempj[[2]]
  }
  return(rows)
}


#' @template deepcopy_template
#' @export
deepcopy <- function(x, cols=NULL, rows=NULL, 
                     y=NULL, type=NULL, separated=NULL,
                     backingfile=NULL, backingpath=NULL,
                     descriptorfile=NULL, binarydescriptor=FALSE,
                     shared=TRUE)
{
  cols <- cleanupcols(cols, ncol(x), colnames(x))
  rows <- cleanuprows(rows, nrow(x), rownames(x))
  if (nrow(x) > 2^31-1)
    stop(paste("Too many rows to copy at this point in time;",
               "this may be fixed in the future."))
  if (is.null(type)) type <- typeof(x)
  if (is.big.matrix(x)) {
    if (is.null(separated)) separated <- is.separated(x)
  } else {
    separated <- FALSE
  }
  if (is.null(y)) {
    y <- big.matrix(nrow=length(rows), ncol=length(cols), type=type, init=NULL,
                  dimnames=dimnames(x), separated=separated,
                  backingfile=backingfile, backingpath=backingpath,
                  descriptorfile=descriptorfile,
                  binarydescriptor=binarydescriptor, shared)
  }
  if (is.big.matrix(x) && is.big.matrix(y))
#     .Call("CDeepCopy", x@address, y@address, as.double(rows), as.double(cols), 
#       getOption("bigmemory.typecast.warning"))
  CDeepCopy(x@address, y@address, as.double(rows), as.double(cols), 
        getOption("bigmemory.typecast.warning"))
  else
    for (i in 1:length(cols)) y[,i] <- x[rows,cols[i]]

  return(y)
}

# Following the R convention we are going to assume Unix directory 
# separators '/' as opposed to the Windows convention '\'.

#' @rdname sub.big.matrix
#' @export
setGeneric('is.sub.big.matrix', function(x)
	standardGeneric('is.sub.big.matrix'))

#' @rdname sub.big.matrix
setMethod('is.sub.big.matrix', signature(x='big.matrix'),
  function(x) return(CIsSubMatrix(x@address)) )

# For now a submatrix only goes over a range of columns and a range
# of row.  This could be made more sophiticated but it would probably
# take a lot of work.

#' @template sub.big.matrix_template
#' @export
setGeneric('sub.big.matrix', function(x, firstRow=1, lastRow=NULL,
  firstCol=1, lastCol=NULL, backingpath=NULL) standardGeneric('sub.big.matrix'))


#' @rdname sub.big.matrix
setMethod('sub.big.matrix', signature(x='big.matrix'),
  function(x, firstRow, lastRow, firstCol, lastCol, backingpath)
  {
    return(sub.big.matrix(describe(x), firstRow, lastRow, firstCol, lastCol, 
           backingpath))
  })

#' @rdname big.matrix.descriptor-class
#' @param x A descriptor object
#' @param firstRow the first row of the submatrix
#' @param lastRow the last row of the submatrix if not NULL
#' @param firstCol the first column of the submatrix
#' @param lastCol of the submatrix if not NULL
#' @param backingpath required path to the filebacked object, if applicable
setMethod('sub.big.matrix', signature(x='big.matrix.descriptor'),
  function( x, firstRow, lastRow, firstCol, lastCol, backingpath)
  {
    rowOffset <- firstRow-1
    colOffset <- firstCol-1
    rbm <- attach.resource(x, path=backingpath)
    if (is.null(lastRow)) lastRow <- nrow(rbm)
    if (is.null(lastCol)) lastCol <- ncol(rbm)
    numCols <- lastCol-firstCol+1
    numRows <- lastRow-firstRow+1
    if (colOffset < 0 || rowOffset < 0 || numCols < 1 || numRows < 1 ||
        colOffset+numCols > ncol(rbm) || rowOffset+numRows > nrow(rbm))
    {
      rm(rbm)
      stop(paste("A sub.big.matrix object could not be created",
                 "with the specified parameters"))
    }
    SetRowOffsetInfo(rbm@address, 
          as.double(rowOffset + GetRowOffset(rbm@address)), 
          as.double(numRows) )
    SetColumnOffsetInfo(rbm@address, 
          as.double(colOffset + GetColOffset(rbm@address)),
          as.double(numCols))
    return(rbm)
  })


setMethod('description', signature(x='big.matrix.descriptor'),
  function(x) return(x@description))

DescribeBigMatrix = function(x)
{
  if (!is.filebacked(x))
  {
    if (is.shared(x)) {
      ret <- list(sharedType = 'SharedMemory',
                  sharedName = shared.name(x), 
                  totalRows = GetTotalRows(x@address),
                  totalCols = GetTotalColumns(x@address),
                  rowOffset = GetRowOffset(x@address),
                  colOffset = GetColOffset(x@address),
                  nrow=nrow(x), ncol=ncol(x),
                  rowNames=rownames(x), colNames=colnames(x), type=typeof(x), 
                  separated=is.separated(x))
    } else {
      stop("you can't describe a non-shared big.matrix.")
    }
  }
  else
  {
    ret = list(sharedType='FileBacked',
               filename=file.name(x),
               totalRows = GetTotalRows(x@address),
               totalCols = GetTotalColumns(x@address),
               rowOffset = GetRowOffset(x@address),
               colOffset = GetColOffset(x@address),
               nrow=nrow(x), ncol=ncol(x),
               rowNames=rownames(x), colNames=colnames(x), type=typeof(x), 
               separated=is.separated(x))
  }
}


#' @template attach.big.matrix_template
# @rdname attach.big.matrix
#' @export
attach.big.matrix = function(obj, ...)
{
  if (!is.null( list(...)[['backingpath']]))
    return(attach.resource(obj, path=list(...)[['backingpath']], ...))
  return(attach.resource(obj, ...))
}

#' @rdname big.matrix.descriptor-class
#' @param obj The filename of the descriptor for a filebacked matrix,
#' assumed ot be in the directory specified
#' @param ... possibly \code{path} which gives the path where the descriptor
#' and/or filebacking can be found.
#' @export
setMethod('attach.resource', signature(obj='character'),
  function(obj, ...)
  {
    path <- list(...)[['path']]
    if (!is.null(path) && path != "") 
      path = paste(path.expand(path), "", sep=.Platform$file.sep)
    if (basename(obj) != obj)
    {
      if (!is.null(path) != "")
        warning(paste("Two paths were specified in attach.resource.",
          "The one associated with the file will be used.", sep="  "))
      fileWithPath = obj
    } else {
      if (!is.null(path) && path == "")
        fileWithPath <- obj
      else if (!is.null(path))
        fileWithPath = paste(path, obj, sep=.Platform$file.sep)
      else
        fileWithPath = obj
    }
    fi = file.info(fileWithPath)
    if (is.na(fi$isdir))
      stop( paste("The file", fileWithPath, "could not be found") )
    if (fi$isdir)
      stop( fileWithPath, "is a directory" )
    info <- tryCatch(readRDS(file=fileWithPath), error=function(er){return(dget(fileWithPath))})
    
    if (dirname(obj) != ".") {
      new_path = dirname(obj)
    }
    else if (!is.null(path) && path != "") new_path = path
    else new_path = path
    return(attach.resource(info, path=new_path, ...))
  })

#' @rdname big.matrix.descriptor-class
#' @export
setMethod('attach.resource', signature(obj='big.matrix.descriptor'),
  function(obj, ...)
  {
    path <- list(...)[['path']]
    info <- description(obj)
    typeLength <- NULL
    if (info$type == 'char') typeLength <- 1
    if (info$type == 'short') typeLength <- 2
    if (info$type == 'integer') typeLength <- 4
    if (info$type == 'float') typeLength <- 6
    if (info$type == 'double') typeLength <- 8
    if (is.null(typeLength)) 
      stop('invalid type')

    readOnly <- ifelse( is.null(list(...)$readonly), FALSE, list(...)$readonly)
    if (!is.logical(readOnly)) {
      stop("The readOnly argument must be of type logical")
    }
    
    if (info$sharedType == 'SharedMemory')
    {
      address <- CAttachSharedBigMatrix(as.character(info$sharedName), 
        as.double(info$totalRows), 
        as.double(info$totalCols), 
        as.character(info$rowNames), 
        as.character(info$colNames), as.integer(typeLength), 
        as.logical(info$separated),
        as.logical(readOnly))
    }
    else
    {
      if (is.null(path) && dirname(info$filename) == ".") {
        path <- getwd()  
        path <- path.expand(path)
        path <- paste(path, '', sep=.Platform$file.sep)
      } else if (is.null(path) && dirname(info$filename) != ".") {
        path = paste(dirname(info$filename), "", sep=.Platform$file.sep)
        info$filename = basename(info$filename)
      } else if(nchar(path) > 0) {
        path = paste(path, "", sep=.Platform$file.sep)
      }
      if (!info$separated) {
        if (!file.exists(paste(path, info$filename, sep=.Platform$file.sep)))
        {
          stop(paste("The backing file", paste(path, info$filename, sep=''),
            "could not be found"))
        }
      } else { 
        # It's separated and we need to check for each column.
        for (i in 1:info$ncol) {
          fn <- paste(info$filename, "_column_", (i-1), sep='')
          if (!file.exists(paste(path, fn, sep=.Platform$file.sep)))
          {
            stop(paste("The backing file", 
                       paste(path, fn, sep=.Platform$file.sep), 
              "could not be found"))
          }
        }
      }
      address <- CAttachFileBackedBigMatrix(
        as.character(info$filename), 
        as.character(path), 
        as.double(info$totalRows), 
        as.double(info$totalCols), 
        as.character(info$rowNames), 
        as.character(info$colNames), 
        as.integer(typeLength), 
        as.logical(info$separated), 
        as.logical(readOnly))
    }
    if (!is.null(address)) 
    {
      SetRowOffsetInfo(address, info$rowOffset, info$nrow)
      SetColumnOffsetInfo(address, info$colOffset, info$ncol)
      ret <- new('big.matrix', address=address)
      # If the user did not specify read-only but the big matrix could 
      # only be opened read-only then issue a warning.
      if (readOnly != is.readonly(ret)) {
        warning("big.matrix object could only be opened read-only.")
      }
    }
    else 
    {
      stop("Fatal error in attach: big.matrix could not be attached.")
    }
    return(ret)  
  })


#' @rdname big.matrix
#' @export
setGeneric('is.filebacked', function(x) standardGeneric('is.filebacked'))

#' @rdname big.matrix
setMethod('is.filebacked', signature(x='big.matrix'),
  function(x) return(IsFileBackedBigMatrix(x@address)))

#' @rdname big.matrix
#' @export
setGeneric('shared.name', function(x) standardGeneric('shared.name'))

#' @rdname big.matrix
setMethod('shared.name', signature(x='big.matrix'),
  function(x) return(SharedName(x@address)))

#' @rdname big.matrix
#' @export
setGeneric('file.name', function(x) standardGeneric('file.name'))

#' @rdname big.matrix
setMethod('file.name', signature(x='big.matrix'),
  function(x)
  {
    if (!is.filebacked(x))
    {
      stop("The argument is not a file backed big.matrix.")
    }
    return(FileName(x@address))
  })


t.big.matrix <- function(x, backingfile=NULL,
                     backingpath=NULL, descriptorfile=NULL,
                     binarydescriptor=FALSE, shared=TRUE) {
  temp <- big.matrix(nrow=ncol(x), ncol=nrow(x), type=typeof(x),
    dimnames=dimnames(x)[[2:1]], separated=is.separated(x),
    backingfile=backingfile, backingpath=backingpath, 
    descriptorfile=descriptorfile, binarydescriptor=binarydescriptor,
    shared=TRUE)

  for (i in 1:nrow(x)) {
    temp[,i] <- x[i,]
  }

  return(temp)
}

#' @template flush_template
#' @export
setGeneric('flush', function(con) standardGeneric('flush'))

#' @rdname flush-methods
setMethod('flush', signature(con='big.matrix'),
  function(con) 
  {
    if (!is.filebacked(con))
    {
      warning("You cannot call flush on a non-filebacked big.matrix")
      return(invisible(TRUE))
    }
    return(invisible(Flush(con@address)))
  })


#' @rdname big.matrix
#' @export
setGeneric('is.shared', function(x) standardGeneric('is.shared'))

#' @rdname big.matrix
setMethod('is.shared', signature(x='big.matrix'),
  function(x) return(IsShared(x@address)))

#' @template morder_template
#' @export
morder <- function(x, cols, na.last=TRUE, decreasing = FALSE)
{
  if (is.character(cols)) cols <- mmap( cols, colnames(x) )
  if (sum(cols > ncol(x)) > 0 | sum(cols < 1) > 0 | sum(is.na(cols) > 0))
  {
    stop("Bad column indices.")
  }
  
  switch(class(x),
         "big.matrix" = OrderBigMatrix(x@address, as.double(cols), 
                                       as.integer(na.last), as.logical(decreasing) ),
         "matrix" = switch(typeof(x),
                           'integer' = OrderRIntMatrix(x, nrow(x), as.double(cols), 
                                                       as.integer(na.last), as.logical(decreasing) ),
                           'double' = OrderRNumericMatrix(x, nrow(x), as.double(cols), 
                                                          as.integer(na.last), as.logical(decreasing) ),
                           stop("Unsupported matrix value type.")),
         stop("unsupported matrix type")
  )
}

#' @rdname morder
#' @export
morderCols <- function(x, rows, na.last=TRUE, decreasing = FALSE)
{
  if (is.character(rows)) rows <- mmap( rows, rownames(x) )
  if (sum(rows > nrow(x)) > 0 | sum(rows < 1) > 0 | sum(is.na(rows) > 0))
  {
    stop("Bad row indices.")
  }
  
  switch(class(x),
         "big.matrix" = OrderBigMatrixCols(x@address, as.double(rows), 
                                           as.integer(na.last), as.logical(decreasing) ),
         "matrix" = switch(typeof(x),
                           'integer' = OrderRIntMatrixCols(x, nrow(x), ncol(x), as.double(rows), 
                                                           as.integer(na.last), as.logical(decreasing) ),
                           'double' = OrderRNumericMatrixCols(x, nrow(x), ncol(x), as.double(rows), 
                                                              as.integer(na.last), as.logical(decreasing) ),
                           stop("Unsupported matrix value type.")),
         stop("unsupported matrix type")
  )
}

#' @rdname morder
#' @export
mpermute <- function(x, order=NULL, cols=NULL, allow.duplicates=FALSE, ...)
{
  if (is.null(order) && is.null(cols))
    stop("You must specify either order or cols.")

  if (!is.null(order) && !is.null(cols))
    stop("You must specify either order or cols.")

  if (!is.null(order))
  {
    if (length(order) != nrow(x))
      stop("order parameter must have the same length as nrow(x)")

    if (!allow.duplicates && sum(duplicated(order)) > 0)
      stop("order parameter contains duplicated entries.")

    r = range(order)
    if (is.na(r[1]))
      stop("order parameter contains NAs")
    if (r[1] < 1 || r[2] > nrow(x))
      stop("order parameter contains values that are out-of-range.")
  }
  else 
    order = morder(x, cols, ...)

  switch(class(x),
         "big.matrix" = ReorderBigMatrix(x@address, order),
         "matrix" = switch(typeof(x),
                           'integer' = ReorderRIntMatrix(x, nrow(x), ncol(x), order),
                           'double' = ReorderRNumericMatrix(x, nrow(x), ncol(x), order),
                           stop("Unsupported matrix value type.")),
         stop("invalid class")
  )

  return(invisible(TRUE))
  
}

#' @rdname morder
#' @export
mpermuteCols <- function(x, order=NULL, rows=NULL, allow.duplicates=FALSE, ...)
{
  if (is.null(order) && is.null(rows))
    stop("You must specify either order or cols.")
  
  if (!is.null(order) && !is.null(rows))
    stop("You must specify either order or cols.")
  
  if (!is.null(order))
  {
    if (length(order) != ncol(x))
      stop("order parameter must have the same length as ncol(x)")
    
    if (!allow.duplicates && sum(duplicated(order)) > 0)
      stop("order parameter contains duplicated entries.")
    
    r = range(order)
    if (is.na(r[1]))
      stop("order parameter contains NAs")
    if (r[1] < 1 || r[2] > nrow(x))
      stop("order parameter contains values that are out-of-range.")
  }
  else 
    order = morderCols(x, rows, ...)
  
  switch(class(x),
         "big.matrix" = ReorderBigMatrixCols(x@address, order),
         "matrix" = switch(typeof(x),
                           'integer' = ReorderRIntMatrixCols(x, nrow(x), ncol(x), order),
                           'double' = ReorderRNumericMatrixCols(x, nrow(x), ncol(x), order),
                           stop("Unsupported matrix value type.")),
         stop("unimplemented class")
         )
  
  return(invisible(TRUE))
  
}

#' @rdname big.matrix
#' @export
setGeneric('is.readonly', function(x) standardGeneric('is.readonly'))

#' @rdname big.matrix
setMethod('is.readonly', signature(x='big.matrix'),
  function(x) IsReadOnly(x@address))

#' @rdname big.matrix
#' @export
is.nil <- function(address) {
    if (class(address)!="externalptr") stop("address is not an externalptr.")
#     ans <- .Call("isnil", address)
    ans <- isnil(address)
    return(ans)
}

getCType <- function(x) {
  if (!inherits(x, "big.matrix"))
    stop("getCType takes a big.matrix as an argument.")

  return(CGetType(x@address))
}

.addDimnames <- function(retList, nrow, ncol, drop) {
  if (drop && !is.matrix(retList[[1]]) ) {
    if (length(retList[[1]]) > 1) {
      if (ncol == 1) {
        thesenames <- retList[[2]]
      } else if (nrow == 1)
        thesenames <- retList[[3]]
    } else {
      thesenames <- NULL
    }
    if (!is.null(thesenames)) {
      names(retList[[1]]) <- thesenames
    }
  } else {
    if (!is.matrix(retList[[1]])) { dim(retList[[1]]) = c(nrow, ncol) }
    if (!is.null(retList[[2]]) || !is.null(retList[[3]])) {
      dimnames(retList[[1]]) <- list( retList[[2]], retList[[3]] )
    }
  }
  return(retList[[1]])
}


# setMethod('is.na', signature(x='big.matrix'),
#           function(x){
#               options(bigmemory.typecast.warning=FALSE)
#               #idx <- bigsplit(x, 5)
#               #na.chunks <- lapply(idx, function(y) any(is.na(x[idx[1]:tail(idx, n=1),])))
#               return(as.big.matrix(is.na(x[,])*1, type="integer"))
#           })
