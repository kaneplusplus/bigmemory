#' @name mwhich
#' @title Expanded ``which''-like functionality.
#' @description
#' Implements \code{\link{which}}-like functionality for a \code{\link{big.matrix}}, 
#' with additional options for efficient comparisons (executed in \acronym{C++});
#' also works for regular numeric matrices without the memory overhead.
#' @param x a \code{\link{big.matrix}} (or a numeric matrix; see below).
#' @param cols a vector of column indices or names.
#' @param vals a list (one component for each of \code{cols}) of vectors of 
#' length 1 or 2; length 1 is used to test equality (or inequality), while 
#' vectors of length 2 are used for checking values in the range (\code{-Inf} 
#' and \code{Inf} are allowed). If a scalar or vector of length 2 is provided 
#' instead of a list, it will be replicated \code{length(cols)} times.
#' @param comps a list of operators (one component for each of \code{cols}), 
#' including \code{'eq'}, \code{'neq'}, \code{'le'}, \code{'lt'}, \code{'ge'} 
#' and \code{'gt'}.  If a single operator, it will be replicated 
#' \code{length(cols)} times.
#' @param op the comparison operator for combining the results of the 
#' individual tests, either \code{'AND'} or \code{'OR'}.
#' @details 
#' To improve performance and avoid the creation of massive temporary vectors 
#' in \R when doing comparisons, \code{mwhich()} efficiently executes 
#' column-by-column comparisons of values to the specified values or ranges, 
#' and then returns the row indices satisfying the comparison specified by the 
#' \code{op} operator.  More advanced comparisons are then possible 
#' (and memory-efficient) in \R by doing set operations (\code{\link{union}} 
#' and \code{\link{intersect}}, for example) on the results of multiple 
#' \code{mwhich()} calls.
#'  
#'  Note that \code{NA} is a valid argument in conjunction with \code{'eq'} or
#'  \code{'neq'}, replacing traditional \code{is.na()} calls.
#'  And both \code{-Inf} and \code{Inf} can be used for one-sided inequalities.
#'  
#'  If \code{mwhich()} is used with a regular numeric \R \code{matrix}, we 
#'  access the data directly and thus incur no memory overhead.  Interested 
#'  developers might want to look at our code for this case, which uses a handy 
#'  pointer trick (accessor) in \acronym{C++}.
#' @return a vector of row indices satisfying the criteria.
#' @author John W. Emerson \email{<bigmemoryauthors@@gmail.com>}
#' @seealso \code{\link{big.matrix}}, \code{\link{which}}
#' @example examples/mwhich_examples.R
#' @keywords methods
