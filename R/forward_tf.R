#' Compute Forward Spectral Graph Wavelet Transform.
#'
#' Compute forward SGWT for signal f (without frame calculation). The calculation corresponds to the frame defined by the `tight_frame` function (without explicit calculation of the latter).
#'
#' @export forward_tf
#' @param f Graph signal to analyze.
#' @param evalues Eigenvalues of the Laplacian matrix.
#' @param evectors Eigenvectors of the Laplacian matrix.
#' @param b Parameter that control the number of scales.
#' @return \code{wc} wavelet coefficients.
#' @references
#' Göbel, F., Blanchard, G., von Luxburg, U. (2018). Construction of tight frames on graphs and application to denoising. In Handbook of Big Data Analytics (pp. 503-522). Springer, Cham.
#'
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.
#'
#' Hammond, D. K., Vandergheynst, P., & Gribonval, R. (2011). Wavelets on graphs via spectral graph theory. Applied and Computational Harmonic Analysis, 30(2), 129-150.
#- todo: export zetav et gv

forward_tf <- function(f, evalues, evectors, b = 2) {
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

zetav <- function(x, k, b) {
  if (k == 0) {
    return(gv(x, b))
  } else {
    return(gv(b^(-k) * x, b) - gv(b^(-k + 1) * x, b))
  }
}

gv <- function(x, b) {
  low <- outer(x, 0, "<")
  mid1 <- outer(x, 0, ">=")
  mid2 <- outer(x, 1/b, "<=")
  mid3 <- outer(x, 1/b, ">=")
  mid4 <- outer(x, 1, "<=")
  up <- outer(x, 1, ">")

  gg <- rep(0, length(x))
  gg[low] <- 1
  gg[mid1 & mid2] <- 1
  gg[mid3 & mid4] <- b * x[mid3 & mid4]/(1 - b) + b/(b - 1)
  gg[up] <- 0
  return(gg)
}
