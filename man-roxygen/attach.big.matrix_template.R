#' @name attach.big.matrix
#' @rdname attach.big.matrix
#' @aliases describe attach.resource
#' @title The basic ``big.matrix'' operations for sharing and re-attaching.
#' @description
#' The \code{describe} function returns the information needed by 
#' \code{attach.big.matrix} to reference a shared or file-backed 
#' \code{big.matrix} object.
#' The \code{attach.big.matrix} and \code{attach.resource} functions create a
#' new \code{big.matrix} object based on the descriptor information referencing
#' previously allocated shared-memory or file-backed matrices.
#' @param x a \code{\link{big.matrix}} object
#' @param obj an object as returned by \code{describe()} or, optionally, 
#' the filename of the descriptor for a filebacked matrix, assumed to be in 
#' the directory specified by the \code{path} (if one is provided)
#' @param ... possibly \code{path} which givesthe path where the descriptor 
#' and/or filebacking can be found
#' @details
#' The \code{describe} function returns a list of the information needed to 
#' attach to a \code{big.matrix} object. 
#' A descriptor file is automatically created when a new filebacked 
#' \code{big.matrix} is created.
#' @return 
#' \code{describe} returns a list of of the information needed to attach to
#' a \code{big.matrix} object.
#' @return
#' \code{attach.big.matrix} return a new instance of type \code{big.matrix}
#' corresponding to a shared-memory or file-backed \code{big.matrix}.
#' @author Michael J. Kane and John W. Emerson 
#' \email{<bigmemoryauthors@@gmail.com>}
#' @seealso \code{\link{bigmemory}}, \code{\link{big.matrix}}, or the class 
#' documentation \code{\linkS4class{big.matrix}}.
#' @example examples/attach.big.matrix_examples.R
#' @keywords classes methods
