#' Compute Inverse Graph Fourier Transform.
#'
#' Computes the Inverse Graph Fourier Transform for a given transformed graph signal 'hatf'. ' It utilizes the eigenvectors U of the Laplacian matrix, and if U is not provided, it computes the eigendecomposition of the Laplacian.
#'
#' @export igft
#' @param L Laplacian matrix of the graph.
#' @param hatf Graph Fourier Transform of the signal to be inverted.
#' @param U Eigenvectors of the Laplacian matrix. If NULL (default), the function will compute the eigendecomposition of the Laplacian.
#' @return \code{f} Original graph signal obtained from the inverse transform of hatf.
#' @seealso \code{\link{gft}}

igft <- function(L, hatf, U = NULL) {
  if (is.null(U)) {
    eigen_decomp <- eigensort(L)
    U <- eigen_decomp$evectors
  }
  f <- U %*% hatf
  return(as.vector(f))
}
