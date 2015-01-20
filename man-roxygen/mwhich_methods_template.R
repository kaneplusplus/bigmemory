#' Expanded ``which''-like functionality.
#' 
#' Implements \code{\link{which}}-like functionality for a
#' \code{\link{big.matrix}}, with additional options for efficient comparisons
#' (executed in \acronym{C++}); also works for regular numeric matrices without
#' the memory overhead.
#' test
#' 
#' @name mwhich-methods
#' @aliases mwhich-methods mwhich,big.matrix,ANY,ANY,ANY,character-method
#' mwhich,big.matrix,ANY,ANY,ANY,missing-method
#' mwhich,matrix,ANY,ANY,ANY,character-method
#' mwhich,matrix,ANY,ANY,ANY,missing-method
#' @docType methods
#' @section Methods: 
#' \describe{ 
#'  \item{signature(x = "big.matrix=", cols
#'  = "ANY", vals = "ANY",", " comps = "ANY", op = "character")}{
#'  ... } 
#'  \item{signature(x = "big.matrix", cols = "ANY", vals =
#'  "ANY",", " comps = "ANY", op = "missing")}{ ... }
#'  \item{signature(x = "matrix", cols = "ANY", vals = "ANY",", "
#'  comps = "ANY", op = "character")}{ ... } 
#'  \item{signature(x = "matrix", cols = "ANY", vals = "ANY",", 
#'  " comps = "ANY", op = "missing")}{ ... } }
#' @seealso \code{\link{big.matrix}}, \code{\link{which}}, \code{\link{mwhich}}
#' @keywords methods
