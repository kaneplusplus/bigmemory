#' Produces a physical copy of a ``big.matrix''
#' 
#' This is needed to make a duplicate of a \code{big.matrix}, with the new copy
#' optionally filebacked.
#' 
#' This is needed to make a duplicate of a \code{\link{big.matrix}}, because
#' traditional syntax would only copy the object (the pointer to the
#' \code{\link{big.matrix}} rather than the \code{\link{big.matrix}} itself).
#' It can also make a copy of only a subset of columns.
#' 
#' @param x a \code{\link{big.matrix}}.
#' @param cols possible subset of columns for the deepcopy; could be numeric,
#' named, or logical.
#' @param rows possible subset of rows for the deepcopy; could be numeric,
#' named, or logical.
#' @param y optional destination object (\code{matrix} or \code{big.matrix});
#' if not specified, a \code{big.matrix} will be created.
#' @param type preferably specified, \code{"integer"} for example.
#' @param separated use separated column organization of the data instead of
#' column-major organization; use with caution if the number of columns is
#' large.
#' @param backingfile the root name for the file(s) for the cache of \code{x}.
#' @param backingpath the path to the directory containing the file-backing
#' cache.
#' @param descriptorfile we recommend specifying this for file-backing.
#' @param binarydescriptor the flag to specify if the binary RDS format should
#' be used for the backingfile description, for subsequent use with
#' \code{\link{attach.big.matrix}}; if \code{NULL} of \code{FALSE}, the
#' \code{dput()} file format is used.
#' @param shared \code{TRUE} by default, and always \code{TRUE} if the
#' \code{big.matrix} is file-backed.  For a non-filebacked \code{big.matrix},
#' \code{shared=FALSE} uses non-shared memory, which can be more stable for
#' large (say, >50\% of RAM) objects.  Shared memory allocation can sometimes
#' fail in such cases due to exhausted shared-memory resources in the system.
#' @return a \code{\link{big.matrix}}.
#' @seealso \code{\link{big.matrix}}
#' @keywords methods
#' @examples
#' x <- as.big.matrix(matrix(1:30, 10, 3))
#' y <- deepcopy(x, -1)    # Don't include the first column.
#' x
#' y
#' head(x)
#' head(y)
