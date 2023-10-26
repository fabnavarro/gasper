#' Compute Forward Graph Fourier Transform
#'
#' \code{forward_gft} computes the Graph Fourier Transform (GFT) of a given graph signal \eqn{f}{f}.
#'
#' @export forward_gft
#' @param L Laplacian matrix of the graph (matrix).
#' @param f Graph signal to analyze (numeric vector).
#' @param U Eigenvectors of the Laplacian matrix (matrix). If NULL (default), the function will compute the eigendecomposition of the Laplacian.
#' @return \code{hatf} Graph Fourier Transform of \eqn{f}{f} (numeric vector).
#' @seealso \code{\link{inverse_gft}}
#' @details
#'
#' The GFT is the projection of the graph signal onto the eigenspace of the graph's Laplacian matrix. It allows to analyze the frequency content of signals defined on graphs. In this context, the "frequency" of a graph signal refers to its decomposition in terms of the graph's Laplacian eigenvectors, which are similar to the harmonics of classical Fourier analysis.
#'
#' The GFT of a graph signal \eqn{f}{f} is given by:
#' \deqn{
#' \hat{f} = U^T f
#' }{\hat{f} = U^T f}
#' where \eqn{U}{U} denotes the matrix of eigenvectors of the graph's Laplacian.
#'
#' When the eigenvectors \eqn{U}{U} are not provided, the function computes them using the Laplacian matrix \eqn{L}{L}.
#'
#' @references
#' Ortega, A., Frossard, P., Kovačević, J., Moura, J. M., & Vandergheynst, P. (2018). Graph signal processing: Overview, challenges, and applications. Proceedings of the IEEE, 106(5), 808-828.
#'
#' Shuman, D. I., Narang, S. K., Frossard, P., Ortega, A., & Vandergheynst, P. (2013). The emerging field of signal processing on graphs: Extending high-dimensional data analysis to networks and other irregular domains. IEEE signal processing magazine, 30(3), 83-98.

forward_gft <- function(L, f, U=NULL) {
  if (is.null(U)) {
    eigen_decomp <- eigensort(L)
    U <- eigen_decomp$evectors
  }
  hatf <- as.vector(t(U)%*%f)
  return(hatf)
}
