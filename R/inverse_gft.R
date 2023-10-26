#' Compute Inverse Graph Fourier Transform
#'
#' \code{inverse_gft} computes the Inverse Graph Fourier Transform (IGFT) of a given transformed graph signal \eqn{\hat{f}}{\hat{f}}.
#'
#' @export inverse_gft
#' @param L Laplacian matrix of the graph (matrix).
#' @param hatf Numeric vector. Graph Fourier Transform of the signal to be inverted.
#' @param U Matrix of the eigenvectors of the Laplacian matrix. If NULL (default), the function will compute the eigendecomposition of the Laplacian.
#' @return \code{f} Numeric vector. Original graph signal obtained from the inverse transform of \eqn{\hat{f}}{\hat{f}}.
#' @seealso \code{\link{forward_gft}}
#' @details
#'The IGFT retrieves the original graph signal from its projection in the eigenspace of the graph's Laplacian matrix. It enables the reconstruction of graph signals from their frequency domain representation. The "frequency" in the context of graph signal processing refers to the decomposition of the signal using the graph's Laplacian eigenvectors.
#'
#' The IGFT of a transformed graph signal \eqn{\hat{f}}{\hat{f}} is given by:
#' \deqn{
#' f = U \hat{f}
#' }{f = U \hat{f}}
#' where \eqn{U}{U} represents the matrix of eigenvectors of the graph's Laplacian.
#'
#' When the eigenvectors \eqn{U}{U} are not provided, the function computes them from the Laplacian matrix \eqn{L}{L}.
#'
#' @references
#' Ortega, A., Frossard, P., Kovačević, J., Moura, J. M., & Vandergheynst, P. (2018). Graph signal processing: Overview, challenges, and applications. Proceedings of the IEEE, 106(5), 808-828.
#'
#' Shuman, D. I., Narang, S. K., Frossard, P., Ortega, A., & Vandergheynst, P. (2013). The emerging field of signal processing on graphs: Extending high-dimensional data analysis to networks and other irregular domains. IEEE signal processing magazine, 30(3), 83-98.

inverse_gft <- function(L, hatf, U = NULL) {
  if (is.null(U)) {
    eigen_decomp <- eigensort(L)
    U <- eigen_decomp$evectors
  }
  f <- U %*% hatf
  return(as.vector(f))
}
