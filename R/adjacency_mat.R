#' Compute the adjacency matrix of the gaussian weighted graph
#'
#' @export adjacency_mat
#' @param pts coordinates of N points in \eqn{R^3}{R^3}.
#' @param f is a scalar potential (\eqn{\exp(-x^2/2t^2)}{exp(-x^2/2*t^2)} for gaussian potential)
#' @param s is a threhold to sparisfy the matrix
#' @examples
#' pts <- swissroll(N=100, seed=0, a=1, b=4)
#' W <- adjacency_mat(pts)
#' @seealso \code{\link{laplacian_mat}}
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
