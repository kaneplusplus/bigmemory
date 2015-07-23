#' @name sub.big.matrix
#' @rdname sub.big.matrix
#' @title Submatrix support
#' @description
#' This doesn't create a copy, it just provides a new version of the class
#' which provides behavior for a contiguous submatrix of the big.matrix.
#' Non-contiguous submatrices are not supported.
#' @param x either a \code{\link{big.matrix}} or a descriptor.
#' @param firstRow the first row of the submatrix.
#' @param lastRow the last row of the submatrix if not \code{NULL}.
#' @param firstCol the first column of the submatrix.
#' @param lastCol the last column of the submatrix if not \code{NULL}.
#' @param backingpath required path to the filebacked object, if applicable.
#' @details
#' The \code{sub.big.matrix} function allows a user to create a \code{big.matrix}
#' object that references a contiguous set of columns and rows of another
#' \code{big.matrix} object.
#'  
#'  The \code{is.sub.big.matrix} function returns \code{TRUE} if the specified 
#'  argument is a \code{sub.big.matrix} object and return \code{FALSE} 
#'  otherwise.
#' @return
#' A \code{\link{big.matrix}} which is actually a submatrix of a larger \code{big.matrix}.
#' It is not a physical copy.  Only contiguous blocks may form a submatrix.
#' @author John W. Emerson and Michael J. Kane
#' @seealso \code{\link{big.matrix}}
#' @examples \dontrun{
#' x <- big.matrix(10, 5, init=0, type="double")
#' x[,] <- 1:50
#' y <- sub.big.matrix(x, 2, 9, 2, 3)
#' y[,]
#' y[1,1] <- -99
#' x[,]
#' }
#' @keywords methods
