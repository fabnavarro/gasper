#' Compute Forward Spectral Graph Wavelet Transform.
#'
#' Compute forward SGWT for signal f (without frame calculation). The calculation corresponds to the frame defined by the `tight_frame` function (without explicit calculation of the latter).
#'
#' @export forward_sgwt
#' @param f Graph signal to analyze.
#' @param evalues Eigenvalues of the Laplacian matrix.
#' @param evectors Eigenvectors of the Laplacian matrix.
#' @param b Parameter that control the number of scales.
#' @return \code{wc} wavelet coefficients.
#' @seealso \code{\link{inverse_sgwt}}, \code{\link{tight_frame}}
#' @references
#' Göbel, F., Blanchard, G., von Luxburg, U. (2018). Construction of tight frames on graphs and application to denoising. In Handbook of Big Data Analytics (pp. 503-522). Springer, Cham.
#'
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.
#'
#' Hammond, D. K., Vandergheynst, P., & Gribonval, R. (2011). Wavelets on graphs via spectral graph theory. Applied and Computational Harmonic Analysis, 30(2), 129-150.
#- todo: export zetav et gv

forward_sgwt <- function(f, evalues, evectors, b = 2) {
  lmax <- max(evalues)
  kmax <- floor(log(lmax)/log(b)) + 2
  N <- length(evalues)
  wc <- matrix(0, nrow = N*(kmax+1), ncol = 1)
  for (k in 0:kmax) {
    G <- sqrt(zetav(evalues, k, b))
    wc[(k*N+1):((k+1)*N)] <- evectors%*%(G*(t(evectors)%*%f))
  }
  return(wc)
}
