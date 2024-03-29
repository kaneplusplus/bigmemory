#' @name big.matrix-class
#' @docType class
#' @rdname big.matrix-class
#' @title Class "big.matrix"
#' @description The \code{big.matrix} class is designed for matrices with 
#' elements of type \code{double}, \code{integer}, \code{short}, or \code{char}.
#' A \code{big.matrix} acts much like a traditional \R matrix, but helps protect
#' the user from many inadvertent memory-consuming pitfalls of traditional \R 
#' matrices and data frames.  The objects are allocated to shared memory,
#' and if file-backing is used they may exceed virtual memory in size.  Sadly,
#' 32-bit operating system constraints -- largely Windows and some MacOS versions 
#' --will be a limiting factor with file-backed matrices; 64-bit operating 
#' systems are recommended.
#' @section Objects from the Class:
#' Unlike many \R objects, objects should not be created by calls of the form
#' \code{new("big.matrix", ...)}.  The functions \code{big.matrix()}
#' and \code{filebacked.big.matrix()} are intended for the user.
#' @section Slots:
#' \describe{\item{\code{address}:}{Object of class \code{"externalptr"} 
#' points to the memory location of the \acronym{C++} data structure.}}
#' @section Methods:
#' \describe{ 
#' As you would expect:
#' \item{[<-}{\code{signature(x = "big.matrix", i = "ANY", j = "ANY")}: ... }
#' \item{[<-}{\code{signature(x = "big.matrix", i = "ANY", j = "missing")}: ... }
#' \item{[<-}{\code{signature(x = "big.matrix", i = "missing", j = "ANY")}: ... }
#' \item{[<-}{\code{signature(x = "big.matrix", i = "missing", j = "missing")}: ... }
#' \item{[<-}{\code{signature(x = "big.matrix", i = "matrix", j = "missing")}: ... }
#' \item{[}{\code{signature(x = "big.matrix", i = "ANY", j = "ANY", drop = "missing")}: ... }
#' \item{[}{\code{signature(x = "big.matrix", i = "ANY", j = "ANY", drop = "logical")}: ... }
#' \item{[}{\code{signature(x = "big.matrix", i = "ANY", j = "missing", drop = "missing")}: ... }
#' \item{[}{\code{signature(x = "big.matrix", i = "ANY", j = "missing", drop = "logical")}: ... }
#' \item{[}{\code{signature(x = "big.matrix", i = "matrix", j = "missing", drop = "logical")}: ... }
#' \item{[}{\code{signature(x = "big.matrix", i = "missing", j = "ANY", drop = "missing")}: ... }
#' \item{[}{\code{signature(x = "big.matrix", i = "missing", j = "ANY", drop = "logical")}: ... }
#' \item{[}{\code{signature(x = "big.matrix", i = "missing", j = "missing", drop = "missing")}: ... }
#' \item{[}{\code{signature(x = "big.matrix", i = "missing", j = "missing", drop = "logical")}: ... }
#' 
#' The following are probably more interesting:
#' \item{describe}{\code{signature(x = "big.matrix")}: provide necessary and 
#' sufficient information for the sharing or re-attaching of the object. }
#' \item{dim}{\code{signature(x = "big.matrix")}: returns the dimension of the 
#' \code{big.matrix}. }
#' \item{length}{\code{signature(x = "big.matrix")}: returns the product of the 
#' dimensions of the \code{big.matrix}. }
#' \item{dimnames<-}{\code{signature(x = "big.matrix", value = "list")}: set 
#' the row and column names, prohibited by default (see \code{\link{bigmemory}} 
#' to override). }
#' \item{dimnames}{\code{signature(x = "big.matrix")}: get the row and column
#' names. }
#' \item{head}{\code{signature(x = "big.matrix")}: get the first 6 (or 
#' \code{n}) rows. }
#' \item{as.matrix}{\code{signature(x = "big.matrix")}: coerce a 
#' \code{big.matrix} to a \code{matrix}. }
#' \item{is.big.matrix}{\code{signature(x = "big.matrix")}: return \code{TRUE} 
#' if it's a \code{big.matrix}. }
#' \item{is.filebacked}{\code{signature(x = "big.matrix")}: return \code{TRUE} 
#' if there is a file-backing. }
#' \item{is.separated}{\code{signature(x = "big.matrix") }: return \code{TRUE} 
#' if the \code{big.matrix} is organized as a separated column vectors.}
#' \item{is.sub.big.matrix}{\code{signature(x = "big.matrix")}: return 
#' \code{TRUE} if this is a sub-matrix of a \code{big.matrix}. }
#' \item{ncol}{\code{signature(x = "big.matrix")}: returns the number of 
#' columns.  }
#' \item{nrow}{\code{signature(x = "big.matrix")}: returns the number of rows. }
#' \item{print}{\code{signature(x = "big.matrix")}: a traditional \code{print()} 
#' is intentionally disabled, and returns \code{head(x)} unless 
#' \code{options()$bm.print.warning==FALSE}; in this case, \code{print(x[,])} 
#' is the result, which could be very big! }
#' 
#' \item{sub.big.matrix}{\code{signature(x = "big.matrix")}: for 
#' contiguous submatrices. }
#' \item{tail}{\code{signature(x = "big.matrix")}: returns the last 6 (or 
#' \code{n}) rows. }
#' \item{typeof}{\code{signature(x = "big.matrix")}: return the type of the 
#' atomic elements of the \code{big.matrix}.}
#' \item{write.big.matrix}{\code{signature(bigMat = "big.matrix", 
#' fileName = "character")}: produce an ASCII file from the \code{big.matrix}.}
#' \item{apply}{\code{signature(x = "big.matrix")}: \code{apply()} where 
#' \code{MARGIN} may only be 1 or 2, but otherwise conforming to what you 
#' would expect from \code{apply()}.}
#' }
#' @author Michael J. Kane and John W. Emerson 
#' \email{bigmemoryauthors@gmail.com}
#' @seealso \code{\link{big.matrix}}
#' @examples
#' showClass("big.matrix")
#' @keywords classes
