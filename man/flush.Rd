\name{flush}
\alias{flush}
\title{Updating a big.matrix filebacking.}
\description{
For a file-backed \code{big.matrix} object, \code{flush()} forces
any modified information to be written to the file-backing.
}
\usage{
flush(con)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{con}{a filebacked \code{\link{big.matrix}}.}
}
\details{
This function flushes any modified data (in \acronym{RAM}) of a file-backed
\code{big.matrix} to disk.  This may be useful for
improving performance in cases where allowing the operating system to decide
on flushing creates a bottleneck (likely near the threshold of available \acronym{RAM}).
}
\value{
\code{TRUE} or \code{FALSE} (invisible), indicating whether or not the flush was successful.
}
%\author{John W. Emerson and Michael J. Kane }
\examples{

x <- big.matrix(nrow=3, ncol=3, backingfile='flushtest.bin',
  descriptorfile='flushtest.desc', type='integer')
x[1,1] <- 0
flush(x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
