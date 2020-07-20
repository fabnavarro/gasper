#' Spectral decomposition of a symetric matrix.
#'
#' Computes eigenvalues and eigenvectors of matrices
#' (output sorted in increasing order).
#'
#' @export eigensort
#' @param x Symetric matrix whose spectral decomposition is to be computed.
#' @examples
#' A <- matrix(1, ncol=2, nrow=2)
#' dec <- eigensort(A)

eigensort <- function(x) {
  dec <- eigen(x, symmetric = T)
  A <- dec$vectors
  return(list(evalues = rev(dec$values), evectors = A[, ncol(A):1]))
}
