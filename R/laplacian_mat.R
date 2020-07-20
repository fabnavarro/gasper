#' Laplacian matrix.
#'
#' Compute the (unormalized) laplacian matrix from the adjacency matrix.
#'
#' @export laplacian_mat
#' @param W Adjacency matrix.
#'
laplacian_mat <- function(W) {
  D <- diag(rowSums(W))
  return(D - W)
}
