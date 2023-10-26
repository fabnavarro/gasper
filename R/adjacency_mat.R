#' Compute the Adjacency Matrix of a Gaussian Weighted Graph
#'
#' \code{adjacency_mat} calculates the adjacency matrix of a Gaussian weighted graph based on the distance between points in \eqn{\mathbb{R}^3}{R^3}.
#'
#' @export adjacency_mat
#' @param pts Matrix representing the coordinates of N points in \eqn{\mathbb{R}^3}{R^3}. Each row should correspond to a point.
#' @param f A scalar potential function. By default, the Gaussian potential \eqn{\exp(-x^2/8)}{exp(-x^2/8)} is used.
#' @param s Numeric threshold used to sparsify the adjacency matrix. Any value below this threshold will be set to zero. Default is 0.
#'
#' @details
#' The function computes pairwise distances between each point in \code{pts} and weights the adjacency matrix based on the scalar potential \code{f}. The final adjacency matrix can be sparsified by setting values below the threshold \code{s} to zero.
#'
#' @return A matrix representing the adjacency matrix of the Gaussian weighted graph.
#'
#' @examples
#' pts <- swissroll(N=100, seed=0, a=1, b=4)
#' W <- adjacency_mat(pts)
#' @seealso \code{\link{laplacian_mat}} for calculating the Laplacian matrix,
#' \code{\link{swissroll}} for generating a Swiss roll dataset.

adjacency_mat <- function(pts, f = function(x) {exp(-x^2/8)},
                          s = 0) {
  N <- nrow(pts)
  ptsi <- pts[rep(1:N, each = N), ]
  ptsj <- pts[rep(1:N, times = N), ]
  tmp <- f(sqrt(rowSums((ptsi - ptsj)^2)))
  res <- tmp * (abs(tmp) > s)
  dim(res) <- c(N, N)
  return(res)
}
