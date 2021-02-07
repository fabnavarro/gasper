#' Laplacian matrix.
#'
#' Compute the (unormalized) laplacian matrix from the adjacency matrix.
#'
#' @export laplacian_mat
#' @importFrom methods is
#' @importFrom Matrix Diagonal rowSums
#' @param W Adjacency matrix.
#' @return \code{L} (unormalized) Laplacian matrix.

laplacian_mat <- function(W) {
  if(is(W, 'sparseMatrix')){
    D <- Diagonal(nrow(W), rowSums(W))
  } else {
    D <- rowSums(W)
  }
  L <- D - W
  return(L)
}
