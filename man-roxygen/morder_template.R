#' @rdname morder
#' @title Ordering and Permuting functions for ``big.matrix'' and 
#' ``matrix'' objects
#' @description The \code{morder} function returns a permutation of row 
#' indices which can be used to rearrange an object according to the values 
#' in the specified columns (a multi-column ordering).
#' The \code{mpermute} function actually reorders the rows of a 
#' \code{big.matrix} or \code{matrix} based on 
#' an order vector or a desired ordering on a set of columns.
#' @param x A \code{big.matrix} or \code{matrix} object with numeric values.
#' @param cols The columns of \code{x} to get the ordering for or reorder on
#' @param rows The rows of \code{x} to get the ordering for or reorder on
#' @param na.last for controlling the treatment of \code{NA}s. If 
#' \code{TRUE}, missing values in the data are put last; if \code{FALSE}, 
#' they are put first; if \code{NA}, they are removed.
#' @param decreasing logical. Should the sort order be increasing or 
#' decreasing?
#' @param order A vector specifying the reordering of rows, i.e. the 
#' result of a call to \code{order} or \code{morder}.
#' @param allow.duplicates ff \code{TRUE}, allows a row to be duplicated in
#' the resulting \code{big.matrix} or \code{matrix} (i.e. in this case, 
#' \code{order} would not need to be a permutation of \code{1:nrow(x)}).
#' @param ... optional parameters to pass to \code{morder} when \code{cols} 
#' is specified instead of just using \code{order}.
#' @details The \code{morder} function behaves similar to \code{order}, 
#' returning a permutation of \code{1:nrow(x)} which rearranges objects 
#' according to the values in the specified columns. However, \code{morder} 
#' takes a \code{big.matrix} or an \R \code{matrix} (with numeric type) and 
#' a set of columns (\code{cols}) with which to determine the ordering; 
#' \code{morder} does not incur the same memory overhead required by 
#' \code{order}, and runs more quickly.
#'   
#' The \code{mpermute} function changes the row ordering of a \code{big.matrix}
#' or \code{matrix} based on a vector \code{order} or an ordering based
#' on a set of columns specified by \code{cols}.  It should be noted that
#' this function has side-effects, that is \code{x} is changed when this
#' function is called.
#' 
#' @return \code{morder} returns an ordering vector.
#' \code{mpermute} returns nothing but does change the contents of \code{x}.
#' This type of a side-effect is generally frowned upon in \R, but we ``break''
#' the rules here to avoid memory overhead and improve performance.
#' 
#' @author Michael J. Kane \email{bigmemoryauthors@gmail.com}
#' @seealso \code{\link{order}}
#' @examples 
#' m = matrix(as.double(as.matrix(iris)), nrow=nrow(iris))
#' morder(m, 1)
#' order(m[,1])
#' 
#' m[order(m[,1]), 2]
#' mpermute(m, cols=1)
#' m[,2]
