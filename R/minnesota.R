#' Minnesota road network.
#'
#' A dataset containing the minnesota road network
#' (as well as the two signals).
#'
#' @format list of 6 elements
#' \describe{
#' \item{xy}{coordinates}
#' \item{A}{adjacency matrix}
#' \item{sA}{sparse version of A}
#' \item{f1}{eta=0.01 and k=2}
#' \item{f2}{eta = 0.001 and k=4}
#' \item{labels}{labels}
#' }
#' @source D. Gleich. The MatlabBGL Matlab library. \url{https://www.cs.purdue.edu/homes/dgleich/packages/matlab_bgl/index.html}.
"minnesota"
