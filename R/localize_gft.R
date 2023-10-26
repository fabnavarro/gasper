#' Localize Kernel at a Graph Vertex Using GFT
#'
#' This function localizes a kernel at a specific vertex using the Graph Fourier Transform (GFT).
#'
#' @export localize_gft
#' @param i Integer index of the node where to localize the kernel.
#' @param L Laplacian matrix of the graph.
#' @param evectors Numeric matrix of the eigenvectors of the Laplacian matrix. If NULL (default), the function will compute the eigendecomposition of the Laplacian.
#' @return \code{s} Kernel localized at vertex \code{i} using GFT.
#'
#' @details
#' The GFT represents the signal in the graph's frequency domain through the eigendecomposition of the Laplacian matrix.
#'
#' The kernel is localized by transforming an impulse signal centered at vertex \eqn{i}{i} using the GFT. The impulse for vertex \eqn{i}{i} is represented by a vector \eqn{s} with all zeros except for a single one at the i-th position.
#' The GFT of a signal \eqn{s} is given by:
#' \deqn{\hat{s} = U^T s}
#' where \eqn{U} is the matrix of eigenvectors of the Laplacian.
#'
#' Applying the GFT to the impulse signal provides a spatial representation of the eigenvector (or kernel) associated with a specific frequency (eigenvalue) centered around vertex \eqn{i}{i}. This depicts how the kernel influences the local neighborhood of the vertex.
#'
#' @examples
#' \dontrun{
#' L <- laplacian_mat(grid1$sA)
#' vertex_i <- sample(1:nrow(L), 1)
#' s <- localize_gft(vertex_i, L=L)
#' plot_signal(grid1, s)
#' s_gft <- forward_gft(L, s)
#' barplot(abs(s_gft), main="GFT of Localized Signal",
#'         xlab="Eigenvalue Index", ylab="Magnitude")
#' }
#' @seealso \code{\link{forward_gft}},\code{\link{localize_sgwt}}

localize_gft <- function(i, L, evectors=NULL) {
  if (is.null(evectors)) {
    eigen_decomp <- eigensort(L)
    evectors <- eigen_decomp$evectors
  }
  N <- nrow(evectors)
  s <- numeric(N)
  s[i] <- 1
  localized_kernel <- forward_gft(L, s, evectors)
  return(localized_kernel)
}
