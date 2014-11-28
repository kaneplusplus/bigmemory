\name{mwhich-methods}
\docType{methods}
\alias{mwhich-methods}
\alias{mwhich,big.matrix,ANY,ANY,ANY,character-method}
\alias{mwhich,big.matrix,ANY,ANY,ANY,missing-method}
\alias{mwhich,matrix,ANY,ANY,ANY,character-method}
\alias{mwhich,matrix,ANY,ANY,ANY,missing-method}
\title{Expanded ``which''-like functionality.}
\description{
Implements \code{\link{which}}-like functionality for a \code{\link{big.matrix}},
with additional options for efficient comparisons (executed in \acronym{C++});
also works for regular numeric matrices without the memory overhead.
}
\section{Methods}{
\describe{
\item{\code{signature(x = "big.matrix", cols = "ANY", vals = "ANY",
  comps = "ANY", op = "character")}}{ ... }
\item{\code{signature(x = "big.matrix", cols = "ANY", vals = "ANY",
  comps = "ANY", op = "missing")}}{ ... }
\item{\code{signature(x = "matrix", cols = "ANY", vals = "ANY",
  comps = "ANY", op = "character")}}{ ... }
\item{\code{signature(x = "matrix", cols = "ANY", vals = "ANY",
  comps = "ANY", op = "missing")}}{ ... }
}}
\seealso{\code{\link{big.matrix}}, \code{\link{which}}, \code{\link{mwhich}}}
\keyword{methods}
