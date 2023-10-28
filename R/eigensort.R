#' Spectral Decomposition of a Symmetric Matrix
#'
#' \code{eigensort} performs the spectral decomposition of a symmetric matrix. The eigenvalues and eigenvectors are sorted in increasing order by eigenvalues.
#'
#' @export eigensort
#' @param M Symmetric matrix, either sparse or dense, to be decomposed.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{evalues}: A vector of sorted eigenvalues in increasing order.
#'   \item \code{evectors}: A matrix of corresponding eigenvectors.
#' }
#'
#' @examples
#' A <- matrix(1, ncol=2, nrow=2)
#' dec <- eigensort(A)

eigensort <- function(M) {
  dec <- eigen(M, symmetric = T)
  A <- dec$vectors
  return(list(evalues = rev(dec$values),
              evectors = A[, ncol(A):1]))
}
