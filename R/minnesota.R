#' Minnesota Road Network
#'
#' A dataset representing the Minnesota road network along with two associated synthetic signals.
#'
#' @format A list with 5 elements:
#' \itemize{
#'   \item \code{xy} A matrix indicating the spatial location of each node.
#'   \item \code{sA} A sparse matrix representation of the road network's adjacency matrix.
#'   \item \code{f1} Synthetic signal generated with parameters \eqn{\eta = 0.01}{eta = 0.01} and \eqn{k = 2}{k = 2}.
#'   \item \code{f2} Synthetic signal generated with parameters \eqn{\eta = 0.001}{eta = 0.001} and \eqn{k = 4}{k = 4}.
#'   \item \code{labels} A character vector with labels that represent various points of entry, border crossings, and notable cities within Minnesota, with some nodes possibly lacking specific location identifiers.
#' }
#'
#' @details
#' The Minnesota roads graph represents a planar structure consisting of 2642 vertices and 6606 edges.
#'
#' The signals come from the referenced paper generated using \code{\link{randsignal}} with parameters \eqn{\eta=0.01, k=2}{eta=0.01, k=2} and \eqn{\eta=0.001,k=4}{eta=0.001, k=4}.
#'
#' @source D. Gleich. The MatlabBGL Matlab library.
#'
#' @references
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.
"minnesota"
