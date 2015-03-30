#' @name write.big.matrix
#' @rdname write.big.matrix
#' @title File interface for a ``big.matrix''
#' @description
#' Create a \code{\link{big.matrix}} by reading from a
#' suitably-formatted ASCII file, or
#' write the contents of a \code{\link{big.matrix}} to a file.
#' @param x a \code{\link{big.matrix}}.
#' @param filename the name of an input/output file.
#' @param sep a field delimiter.
#' @param header if \code{TRUE}, the first line (after a possible skip) 
#' should contain column names.
#' @param col.names a vector of names, use them even if column names exist 
#' in the file.
#' @param row.names a vector of names, use them even if row names appear to 
#' exist in the file.
#' @param has.row.names if \code{TRUE}, then the first column contains row 
#' names.
#' @param ignore.row.names if \code{TRUE} when \code{has.row.names==TRUE}, 
#' the row names will be ignored.
#' @param type preferably specified, \code{"integer"} for example.
#' @param skip number of lines to skip at the head of the file.
#' @param separated use separated column organization of the data instead of 
#' column-major organization.
#' @param backingfile the root name for the file(s) for the cache of \code{x}.
#' @param backingpath the path to the directory containing the file backing 
#' cache.
#' @param descriptorfile the file to be used for the description of the 
#' filebacked matrix.
#' @param binarydescriptor the flag to specify if the binary RDS format should 
#' be used for the backingfile description, for subsequent use with 
#' \code{\link{attach.big.matrix}}; if \code{NULL} of \code{FALSE}, the 
#' \code{dput()} file format is used.
#' @param extraCols the optional number of extra columns to be appended to the
#' matrix for future use.
#' @param shared if \code{TRUE}, the resulting \code{big.matrix} can be shared 
#' across processes.
#' @details
#' Files must contain only one atomic type
#' (all \code{integer}, for example).  You, the user, should know whether
#' your file has row and/or column names, and various combinations of options
#' should be helpful in obtaining the desired behavior.
#' 
#' When reading from a file, if \code{type} is not specified we try to
#' make a reasonable guess for you without
#' making any guarantees at this point. 
#' Unless you have really large integer values, we recommend
#' you consider \code{"short"}.  If you have something that is essentially
#' categorical, you might even be able use \code{"char"}, with huge memory
#' savings for large data sets.
#' 
#' Any non-numeric entry will be ignored and replaced with \code{NA},
#' so reading something that traditionally would be a \code{data.frame}
#' won't cause an error.  A warning is issued.
#' 
#' Wishlist: we'd like to provide an option to ignore specified columns while
#' doing reads.
#' Or perhaps to specify columns targeted for factor or character conversion
#' to numeric values.  Would you use such features?  Email us and let us know!
#' @return
#' a \code{\link{big.matrix}} object is returned by \code{read.big.matrix}, 
#' while \code{write.big.matrix} creates an output file (a path could be part 
#' of \code{filename}).
#' @author John W. Emerson and Michael J. Kane 
#' \email{<bigmemoryauthors@@gmail.com>}
#' @seealso \code{\link{big.matrix}}
#' @example examples/write.big.matrix_examples.R
#' @keywords methods
