################################################################################

#' @rdname big.matrix
#' @param ... Arguments passed on to big.matrix.
#' @export
setGeneric('transpose', function(x, ...) standardGeneric('transpose'))

#' @title Transposition
#' @description This function implements a simple cache-oblivious
#' algorithm for the transposition of a `big.matrix`.
#' @param x A [big.matrix][bigmemory::big.matrix-class].
#' @inheritDotParams big.matrix -nrow -ncol -type -init -dimnames
#' @return The transposed `big.matrix`. Its dimension, type and dimnames
#' are automatically determined from the input `big.matrix`.
#' @example examples/example-transpose.R
setMethod('transpose', signature(x = 'big.matrix'),
          big_transpose <- function(x, ...) {
            
            res <- big.matrix(ncol(x), nrow(x), type = typeof(x), 
                              dimnames = dimnames(x)[2:1], ...)
            
            transpose3(res@address, x@address)
            
            res
          })

################################################################################
