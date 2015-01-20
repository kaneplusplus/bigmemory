\name{as.big.matrix-methods}
\docType{methods}
\alias{as.big.matrix-methods}
\alias{as.big.matrix,matrix-method}
\alias{as.big.matrix,vector-method}
\alias{as.big.matrix,data.frame-method}
\title{Create a ``big.matrix'' from a matrix or vector.}
\description{
Create a \code{\link{big.matrix}} from a \code{matrix} or \code{vector}
or \code{\link{data.frame}};
a \code{vector} will result in a \code{\link{big.matrix}} with one column.
A data frame will have character vectors converted to factors, and then
all factors converted to numeric factor levels.  All labels or character
values will be lost.
}
\section{Methods}{
\describe{
\item{\code{signature(x = "matrix")}}{ ... }
\item{\code{signature(x = "vector")}}{ ... }
\item{\code{signature(x = "data.frame")}}{ ... }
}}
\keyword{methods}
