#' Compute Inverse Spectral Graph Wavelet Transform
#'
#' \code{inverse_sgwt} computes the inverse (adjoint) Spectral Graph Wavelet Transform (SGWT) for wavelet coefficients \code{wc}.
#'
#' @export inverse_sgwt
#' @param wc Numeric vector representing the spectral graph wavelet coefficients to reconstruct the graph signal from.
#' @param evalues Numeric vector of eigenvalues of the Laplacian matrix.
#' @param evectors Matrix of eigenvectors of the Laplacian matrix.
#' @param b Numeric scalar that control the number of scales in the SGWT. It must be greater than 1.
#' @param filter_func Function used to compute the filter values. By default, it uses the \code{\link{zetav}} function but other frame filters can be passed.
#' @param filter_params List of additional parameters required by filter_func. Default is an empty list.
#' @return \code{f} A graph signal obtained by applying the SGWT adjoint to \code{wc}.
#' @seealso \code{\link{forward_sgwt}}, \code{\link{tight_frame}}
#'
#' @details
#'
#' The computation corresponds to the frame defined by the \code{\link{tight_frame}} function. Other filters can be passed as parameters. Given the tightness of the frame, the inverse is simply the application of the adjoint linear transformation to the wavelet coefficients.
#'
#' Given wavelet coefficients \code{wc}, \code{inverse_sgwt} reconstructs the original graph signal using the inverse SGWT.
#'
#' The eigenvalues and eigenvectors of the graph Laplacian are denoted as \eqn{\Lambda}{Lambda} and \eqn{U}{U} respectively. The parameter \eqn{b}{b} controls the number of scales, and \eqn{\lambda_{\text{max}}}{lambda_max} is the largest eigenvalue.
#'
#' For each scale \eqn{j = 0,\ldots, J}{j = 0,..., J}, where
#' \deqn{J = \left\lfloor \frac{\log(\lambda_{\text{max}})}{\log(b)} \right\rfloor + 2}{J = floor(log(lambda_max)/log(b)) + 2} the reconstructed signal for that scale is computed as:
#' \deqn{
#' \mathbf{f}_j = (U \mathbf{wc}_j \odot g_j) U^T
#' }{\mathbf{f}_j = (U wc_j * g_j) U^T}
#' where \deqn{g_j(\lambda) = \sqrt{\psi_j(\lambda)}}{g_j(lambda) = sqrt(psi_j(lambda))} and \eqn{\odot}{*} denotes element-wise multiplication.
#'
#' The final result is the sum of \eqn{\mathbf{f}_j}{f_j} across all scales to reconstruct the entire graph signal.
#'
#' @note
#' \code{inverse_sgwt} can be adapted for other filters by passing a different filter function to the \code{filter_func} parameter.
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
#' # Create a sample graph signal
#' f <- rnorm(nrow(L))
#'
#' # Compute the forward Spectral Graph Wavelet Transform
#' wc <- forward_sgwt(f, decomp$evalues, decomp$evectors)
#'
#' # Reconstruct the graph signal using the inverse SGWT
#' f_rec <- inverse_sgwt(wc, decomp$evalues, decomp$evectors)
#' }
#'
#' @references
#' Göbel, F., Blanchard, G., von Luxburg, U. (2018). Construction of tight frames on graphs and application to denoising. In Handbook of Big Data Analytics (pp. 503-522). Springer, Cham.
#'
#' Hammond, D. K., Vandergheynst, P., & Gribonval, R. (2011). Wavelets on graphs via spectral graph theory. Applied and Computational Harmonic Analysis, 30(2), 129-150.
#'
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.

inverse_sgwt <- function(wc, evalues, evectors, b = 2,
                         filter_func=zetav,
                         filter_params=list()) {
  lmax <- max(evalues)
  kmax <- floor(log(lmax)/log(b)) + 2
  if ("kmax" %in% names(filter_params)) {
    kmax <- filter_params$kmax
    filter_params$kmax <- NULL
  }
  N <- length(evalues)
  f <- matrix(0, nrow = N, ncol = kmax+1)
  for (k in 0:kmax) {
    #G <- sqrt(zetav(evalues, k, b))
    G <- sqrt(do.call(filter_func,
                      c(list(evalues, k, b),
                        filter_params)))
    f[, k+1] <- ((t(wc[(k*N+1):((k+1)*N)])%*%evectors)*G)%*%t(evectors)
  }
  return(rowSums(f))
}
