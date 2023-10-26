#' Spectral Coordinates for Graph Drawing
#'
#' Calculates the spectral coordinates of a graph using the two smallest non-zero eigenvalues of the graph Laplacian.
#'
#' @export spectral_coords
#' @importFrom RSpectra eigs_sym
#' @param adj_mat A symmetric adjacency matrix or sparse matrix representing an undirected graph.
#' @return A matrix where each row represents the spectral coordinates of a node in the graph.
#' @seealso \code{\link{plot_graph}}, \code{\link{plot_signal}}
#' @references
#' Chung, F. R. K. (1997). Spectral Graph Theory. American Mathematical Soc.
#'
#' Hall, K. M. (1970). An r-dimensional quadratic placement algorithm. Management science, 17(3), 219-229.

#- todo add test to deal with eigenvalues with algebraic multiplicity>1 particulary for 0
# non_zero_eig_vals <- eig_dec$evalues[eig_dec$evalues > 1e-10]
# smallest_indices <- which(eig_dec$evalues %in% non_zero_eig_vals)[1:2]
# u2 <- eig_dec$evectors[, smallest_indices[1]]
# u3 <- eig_dec$evectors[, smallest_indices[2]]

spectral_coords <- function(adj_mat) {
  L <- laplacian_mat(adj_mat)

  if (max(dim(adj_mat)) > 3000) {
    # Use Rspectra::eigs_sym for larger matrices
    adj_mat <- as(adj_mat, "dgCMatrix")
    u <- eigs_sym(L, 3, which = "SM")$vectors
    u <- u[,nrow(u):2]
  } else {
    eig_dec <- eigensort(L)
    u2 <- eig_dec$evectors[, 2]
    u3 <- eig_dec$evectors[, 3]
    #non_zero_eig_vals <- eig_dec$evalues[eig_dec$evalues > 1e-10]
    #smallest_indices <- which(eig_dec$evalues %in% non_zero_eig_vals)[1:2]
    #u2 <- eig_dec$evectors[, smallest_indices[1]]
    #u3 <- eig_dec$evectors[, smallest_indices[2]]
    u <- cbind(u2, u3)
  }
  return(u)
}
