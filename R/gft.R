#' Compute Graph Fourier Transform.
#'
#' Computes the Graph Fourier Transform for a given graph signal f. It utilizes the eigenvectors U of the Laplacian matrix, and if U is not provided, it computes the eigendecomposition of the Laplacian.
#'
#' @export gft
#' @param L Laplacian matrix of the graph.
#' @param f Graph signal to analyze.
#' @param U Eigenvectors of the Laplacian matrix.  If NULL (default), the function
#'   will compute the eigendecomposition of the Laplacian.
#' @return \code{hatf} Graph Fourier Transform of f.
#' @seealso \code{\link{igft}}

gft <- function(L, f, U=NULL) {
  if (is.null(U)) {
    eigen_decomp <- eigensort(L)
    U <- eigen_decomp$evectors
  }
  hatf <- as.vector(t(U)%*%f)
  return(hatf)
}
