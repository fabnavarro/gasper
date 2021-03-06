% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smoothmodulus.R
\name{smoothmodulus}
\alias{smoothmodulus}
\title{Modulus of smoothness.}
\usage{
smoothmodulus(f, A, L)
}
\arguments{
\item{f}{Signal.}

\item{A}{Adjacency matrix (sparse or dense).}

\item{L}{Laplacian matrix (sparse or dense).}
}
\description{
Compute the modulus of smoothness of a graph signal.
}
\examples{
data(minnesota)
A <- minnesota$A
L <- laplacian_mat(A)
x <- minnesota$xy[ ,1]
f <- sin(x)
smoothmodulus(f, A, L)
}
