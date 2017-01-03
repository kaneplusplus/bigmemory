#' @template sub.big.matrix_template
#' @export
setGeneric('sub.big.matrix2', function(x, rows = NULL, cols = NULL)
  standardGeneric('sub.big.matrix2'))

#' @rdname sub.big.matrix
setMethod('sub.big.matrix2', signature(x = 'big.matrix'),
          function(x, rows, cols) {
            sub.big.matrix2(describe(x), rows, cols)
          })

#' @rdname big.matrix.descriptor-class
#' @param x A descriptor object
#' @param rows A vector of row indices of the sub.big.matrix
#' @param cols A vector of column indices of the sub.big.matrix
setMethod('sub.big.matrix2', signature(x = 'big.matrix.descriptor'),
          function(x, rows, cols) {
            rbm <- attach.resource(x)
            if (is.sub.big.matrix(x))
              stop("You can't use 'sub.big.matrix2' on a sub.big.matrix")
            if (is.null(rows)) rows <- 1:nrow(rbm) - 1
            if (is.null(cols)) cols <- 1:ncol(rbm) - 1
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
