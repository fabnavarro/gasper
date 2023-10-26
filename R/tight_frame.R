#' Tight-Frame Computation
#'
#' Constructs a tight-frame wavelet on graphs
#'
#' @export tight_frame
#' @param evalues Numeric vector containing the eigenvalues of the Laplacian matrix.
#' @param evectors Matrix of the corresponding eigenvectors of the Laplacian matrix.
#' @param b Numeric scalar. Parameter that controls the number of scales in the wavelet decomposition.
#' @param filter_func Function used to compute the filter values. By default, it uses the \code{\link{zetav}} function but other frame filters can be passed.
#' @param filter_params List of additional parameters required by filter_func. Default is an empty list.
#'
#' @return Matrix of the tight-frame wavelet coefficients.
#'
#' @note
#' \code{tight_frame} can be adapted for other filters by passing a different filter function to the \code{filter_func} parameter.
#' The computation of \eqn{k_{\text{max}}}{k_max} using \eqn{\lambda_{\text{max}}}{lambda_max} and \eqn{b}{b} applies primarily to the default \code{zetav} filter. It can be overridden by providing it in the \code{filter_params} list for other filters.
#'
#' @examples
#' \dontrun{
#' # Extract the adjacency matrix from the grid1 and compute the Laplacian
#' L <- laplacian_mat(grid1$sA)
#'
#' # Compute the spectral decomposition of L
#' decomp <- eigensort(L)
#'
#' # Generate the tight frame coefficients using the tight_frame function
#' tf <- tight_frame(decomp$evalues, decomp$evectors)
#' }
#'
#' @references
#' Coulhon, T., Kerkyacharian, G., & Petrushev, P. (2012). Heat kernel generated frames in the setting of Dirichlet spaces. Journal of Fourier Analysis and Applications, 18(5), 995-1066.
#'
#' Göbel, F., Blanchard, G., von Luxburg, U. (2018). Construction of tight frames on graphs and application to denoising. In Handbook of Big Data Analytics (pp. 503-522). Springer, Cham.
#'
#' Leonardi, N., & Van De Ville, D. (2013). Tight wavelet frames on multislice graphs. IEEE Transactions on Signal Processing, 61(13), 3357-3367.
#'
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.

tight_frame <- function(evalues, evectors, b = 2,
                        filter_func=zetav,
                        filter_params=list()) {
  lmax <- max(evalues)
  kmax <- floor(log(lmax)/log(b)) + 2
  if ("kmax" %in% names(filter_params)) {
    kmax <- filter_params$kmax
    filter_params$kmax <- NULL
  }
  N <- length(evalues)
  r <- array(0, c(N, N, kmax + 1))
  for (k in 0:kmax) {
    G <- diag(sqrt(do.call(filter_func,
                           c(list(evalues, k, b),
                             filter_params))))
    A <- evectors %*% G
    W <- A %*% t(evectors)
    W <- t(W)
    r[, , k + 1] <- W
  }
  dim(r) <- c(N, N * (kmax + 1))
  r <- t(r)
  return(r)
}
