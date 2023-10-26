#' Localize a Kernel at a Specific Vertex using SGWT
#'
#' This function localizes a kernel at a specific vertex using the Spectral Graph Wavelet Transform (SGWT).
#'
#' @export localize_sgwt
#' @param i Integer index of the node where to localize the kernel.
#' @param evalues Numeric vector of the eigenvalues of the Laplacian matrix.
#' @param evectors Numeric matrix of the eigenvectors of the Laplacian matrix.
#' @param b Numeric scalar that controls the number of scales in the SGWT. It must be greater than 1.
#' @return \code{s} Kernel localized at vertex \code{i} using SGWT.
#'
#' @details
#'
#' The SGWT offers a comprehensive understanding of graph signals by providing insights into both vertex (spatial) and spectral (frequency) domains.
#'
#' The kernel is localized by transforming an impulse signal centered at vertex \eqn{i}{i} using the SGWT.
#' The SGWT leverages a wavelet function \eqn{\psi(\lambda)} to provide a multi-resolution analysis of the graph signal.
#' The impulse signal at vertex \eqn{i}{i} is a vector \eqn{s} with a one at the i-th position and zeros elsewhere.
#' The SGWT is given by:
#' \deqn{W_s(\lambda) = s \ast \psi(\lambda) = U \psi(\Lambda) U^T s}
#' where \eqn{U} is the matrix of eigenvectors of the Laplacian and \eqn{\Lambda} is the diagonal matrix of eigenvalues.
#' The localized spatial view of the kernel's behavior around vertex \eqn{i}{i} is achieved by transforming this impulse signal using the above expression.
#'
#' To gain insights into the spectral localization of this localized kernel, one can analyze its GFT to understand how the energy of the kernel is distributed across various graph frequencies. As SGWT scales move from coarse to fine, energy concentration of the localized kernel shifts from lower to higher graph frequencies, indicating tighter spectral localization.
#'
#' @examples
#' \dontrun{
#' # Compute the Laplacian matrix and its eigen-decomposition
#' L <- laplacian_mat(grid1$sA)
#' decomp <- eigensort(L)
#'
#' # Randomly select a vertex
#' vertex_i <- sample(1:nrow(L), 1)
#'
# Localize the SGWT kernel at the selected vertex
#' s_sgwt <- localize_sgwt(vertex_i, evalues=decomp$evalues, evectors=decomp$evectors, b=2)
#'
#' # Select one scale j from s_sgwt.
#' N <- nrow(grid1$sA)
#' j <- 5 # change scale j to view other scales
#' s <- s_sgwt[ ((j-1)*N+1):(j*N)]
#'
#' # Plot the localized kernel (for the chosen scale) as a signal on the graph
#' plot_signal(grid1, s)
#'
#' # Plot the magnitude of the GFT coefficients
#' barplot(abs(s_gft), main="GFT of Localized Signal",
#'         xlab="Eigenvalue Index", ylab="Magnitude")
#' }
#' @seealso \code{\link{forward_sgwt}}, \code{\link{forward_gft}}, \code{\link{forward_gft}}

localize_sgwt <- function(i, evalues, evectors, b=2) {
  N <- nrow(evectors)
  s <- numeric(N)
  s[i] <- 1
  localized_kernel <- forward_sgwt(s, evalues, evectors, b)
  return(localized_kernel)
}
