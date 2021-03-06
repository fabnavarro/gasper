% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/full.R
\name{full}
\alias{full}
\title{Convert symmetric sparse matrix to full matrix.}
\usage{
full(sA)
}
\arguments{
\item{sA}{Sparse matrix to convert.}
}
\description{
Convert a symmetric sparse matrix sA to full matrix A.
}
\examples{
sA <- pittsburgh$sA
A <- full(sA)
}
\seealso{
\code{\link{fullup}}

\code{\link{fullup}}
}
