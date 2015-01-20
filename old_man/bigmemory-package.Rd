\name{bigmemory-package}
\alias{bigmemory-package}
\alias{bigmemory}
\docType{package}
\title{
Manage massive matrices with shared memory and memory-mapped files.
}
\description{
Create, store, access, and manipulate massive matrices.  Matrices are,
by default,
allocated to shared memory and may use memory-mapped files.  Packages
\pkg{biganalytics}, \pkg{synchronicity}, \pkg{bigalgebra}, and \pkg{bigtabulate} provide
advanced functionality.  Access to and manipulation of a \code{\link{big.matrix}}
object is exposed in \R by an S4 class whose interface is simlar
to that of an \R \code{\link{matrix}}.  Use of these packages in parallel environments
can provide substantial speed and memory efficiencies.  \pkg{bigmemory} also
provides a \acronym{C++} framework for the development of new tools that can
work both with \code{big.matrix} and native \R \code{matrix} objects.
}
\details{
\tabular{ll}{
Package: \tab bigmemory\cr
Type: \tab Package\cr
Version: \tab 4.4.14\cr
Date: \tab 2014-09-04\cr
License: \tab LGPL-3 | APv2\cr
Copyright: \tab (C) 2014 Michael J. Kane and John W. Emerson\cr
URL: \tab http://www.bigmemory.org\cr
LazyLoad: \tab yes\cr
}

Index of functions/methods (grouped in a friendly way):
\preformatted{
big.matrix, filebacked.big.matrix, as.big.matrix

is.big.matrix, is.separated, is.filebacked

describe, attach.big.matrix, attach.resource

sub.big.matrix, is.sub.big.matrix

dim, dimnames, nrow, ncol, print, head, tail, typeof, length

read.big.matrix, write.big.matrix

mwhich

morder, mpermute

deepcopy

flush
}

Multi-gigabyte data sets challenge and frustrate \R
users, even on well-equipped hardware.
Use of \acronym{C/C++} can provide efficiencies,
but is cumbersome for interactive data analysis and
lacks the flexibility and power of \R's rich statistical
programming environment.  The package \pkg{bigmemory} and sister
packages \pkg{biganalytics}, \pkg{synchronicity}, \pkg{bigtabulate},
and \pkg{bigalgebra} bridge this gap, implementing massive matrices
and supporting their manipulation and exploration.
The data structures may be allocated to shared memory, allowing separate
processes on the same computer to share access to a single copy of the
data set.  The data structures may also be file-backed, allowing users
to easily manage and analyze data sets larger than available RAM and
share them across nodes of a cluster.
These features of the Bigmemory Project open the door for powerful and
memory-efficient parallel analyses and data mining of massive data sets.

This project (\pkg{bigmemory} and its sister packages)
is still actively developed, although the design and current features
can be viewed as "stable."  Please feel free to email us with any
questions: bigmemoryauthors@gmail.com.
}
\note{
Various options are available. \code{options(bigmemory.typecast.warning)}
can be set to avoid annoying warnings that might occur if, for example, you
assign \R objects (typically type double) to char, short, or integer
\code{\link{big.matrix}} objects.  
\code{options(bigmemory.print.warning)} protects against extracting and
printing a massive
matrix (which would involve the creation of a second massive copy of the matrix).
\code{options(bigmemory.allow.dimnames)} by default prevents the setting of
\code{dimnames} attributes, because they aren't allocated to shared memory
and changes will not be visible across processes.
\code{options(bigmemory.default.type)} is \code{"double"} be default (a
change in default behavior as of 4.1.1) but may be changed by the user.

Versions >=4.0 represent a major redesign, with the mutexes (locking)
abstracted to package \pkg{synchronicity}, the exploratory data analysis
functionality relocated
to package \pkg{biganalytics}, and new linear algebra support available in
package \pkg{bigalgebra}.  
Package \pkg{bigtabulate} extends the \pkg{bigmemory} package with table-,
tapply-, and split-like behavior.  The functions may also be used
with regular \R matrices for speed and memory-efficiency gains.
Package \pkg{bigmemory} itself is now minimalist,
providing only the core functionality.  As an example, the \code{apply()}
method appears in \pkg{biganalytics}, supporting exploration and analysis,
while \code{\link{mwhich}}, \code{\link{morder}} and \code{\link{mpermute}}
appear in \pkg{bigmemory} as fundamental tools for data manipulation.

Versions <4.0 supported a limited number of columns
(due to mutex limitations): roughly 50,000 on a typical Linux system.
This restriction has been removed in versions >=4.0.  There
were row limitations (due to a bug that has now been fixed) in versions
<=3.8 of roughly 1 billion, but this has been fixed in versions
>=3.82.

Note that you can't simply use a \code{big.matrix} with many (most) existing
\R functions (e.g. \code{\link{lm}}, \code{\link{kmeans}}).  One nice
exception is \code{\link{split}}, because this function only accesses
subsets of the matrix.
}
\author{
Michael J. Kane and John W. Emerson

Maintainers: Michael J. Kane <bigmemoryauthors@gmail.com>
}
\keyword{package}
\references{The Bigmemory Project: \url{http://www.bigmemory.org/}.}
\seealso{For example,
\code{\link{big.matrix}}, \code{\link{mwhich}}, \code{\link{read.big.matrix}}
}
\examples{

# Our examples are all trivial in size, rather than burning huge amounts
# of memory.

x <- big.matrix(5, 2, type="integer", init=0,
                dimnames=list(NULL, c("alpha", "beta")))
x
x[1:2,]
x[,1] <- 1:5
x[,"alpha"]
colnames(x)
options(bigmemory.allow.dimnames=TRUE)
colnames(x) <- NULL
x[,]

}
