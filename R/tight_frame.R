#' Tight-frame computation.
#'
#' Constructs tight-frame.
#'
#' @export tight_frame
#' @param evalues Eigenvalues of the Laplacian matrix.
#' @param evectors Eigenvectors of the Laplacian matrix.
#' @param b Parameter that control the number of scales.
#' @references
#' Coulhon, T., Kerkyacharian, G., & Petrushev, P. (2012). Heat kernel generated frames in the setting of Dirichlet spaces. Journal of Fourier Analysis and Applications, 18(5), 995-1066.
#'
#' Göbel, F., Blanchard, G., von Luxburg, U. (2018). Construction of tight frames on graphs and application to denoising. In Handbook of Big Data Analytics (pp. 503-522). Springer, Cham.
#'
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.

tight_frame <- function(evalues, evectors, b = 2) {
  lmax <- max(evalues)
  kmax <- floor(log(lmax)/log(b)) + 2
  N <- length(evalues)
  r <- array(0, c(N, N, kmax + 1))
  for (k in 0:kmax) {
    G <- diag(sqrt(zetav(evalues, k, b)))
    A <- evectors %*% G
    W <- A %*% t(evectors)
    W <- t(W)
    r[, , k + 1] <- W
  }
  dim(r) <- c(N, N * (kmax + 1))
  r <- t(r)
  return(r)
}
