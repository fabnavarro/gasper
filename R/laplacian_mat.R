#' Compute the Graph Laplacian Matrix
#'
#' \code{laplacian_mat} computes various forms of the graph Laplacian matrix for a given adjacency matrix \code{W}.
#'
#' @export laplacian_mat
#' @importFrom methods is
#' @importFrom Matrix Diagonal rowSums
#' @param W Adjacency matrix (dense or sparseMatrix).
#' @param type Character string, type of Laplacian matrix to compute. Can be "unnormalized" (default), "normalized", or "randomwalk".
#' @return \code{L} The graph Laplacian matrix.
#' @details
#'
#' The function supports three types of Laplacian matrices:
#' \itemize{
#'   \item Unnormalized Laplacian:
#'     \deqn{L = D - W}{L = D - W}
#'   \item Normalized Laplacian:
#'     \deqn{L_{norm} = I - D^{-1/2} W D^{-1/2}}{L_{norm} = I - D^{-1/2} W D^{-1/2}}
#'   \item Random Walk Laplacian:
#'     \deqn{L_{rw} = I - D^{-1} W}{L_{rw} = I - D^{-1} W}
#'}
#'
#' Where:
#' \itemize{
#'   \item \eqn{D}{D} is the degree matrix, a diagonal matrix where each diagonal element \eqn{D_{ii}}{D_{ii}} represents the sum of the weights of all edges connected to node \eqn{i}{i}.
#'   \item \eqn{W}{W} is the adjacency matrix of the graph.
#'   \item \eqn{I}{I} is the identity matrix.
#'}
#'
#' The function supports both standard and sparse matrix representations of the adjacency matrix.
#'
#' @references
#' Chung, F. R. (1997). Spectral graph theory (Vol. 92). American Mathematical Soc.
#' @examples
#' # Define the 3x3 adjacency matrix
#' W <- matrix(c(0, 1, 0,
#'               1, 0, 1,
#'               0, 1, 0), ncol=3)
#'
#' # Non-sparse cases
#' laplacian_mat(W, "unnormalized")
#' laplacian_mat(W, "normalized")
#' laplacian_mat(W, "randomwalk")
#'
#' # Convert W to a sparse matrix
#' W_sparse <- as(W, "sparseMatrix")
#'
#' # Sparse cases
#' laplacian_mat(W_sparse, "unnormalized")
#' laplacian_mat(W_sparse, "normalized")
#' laplacian_mat(W_sparse, "randomwalk")

laplacian_mat <- function(W, type = "unnormalized") {
  if (is(W, 'sparseMatrix')) {
    D <- Diagonal(nrow(W), rowSums(W))
    D_half_inv <- Diagonal(nrow(W), 1/sqrt(rowSums(W)))
    D_inv <- Diagonal(nrow(W), 1/rowSums(W))
  } else {
    D <- diag(rowSums(W))
    D_half_inv <- diag(1/sqrt(rowSums(W)))
    D_inv <- diag(1/rowSums(W))
  }
  if (type == "unnormalized") {
    L <- D - W
  } else if (type == "normalized") {
    L <- diag(nrow(W)) - D_half_inv %*% W %*% D_half_inv
  } else if (type == "randomwalk") {
    L <- diag(nrow(W)) - D_inv %*% W
  } else {
    stop("Invalid type provided.
         Choose from 'unnormalized',
         'normalized', or 'randomwalk'.")
  }
  return(L)
}
