#' Graph Von Neumann Estimator.
#'
#' Graph equivalent of the Von Neummann variance estimator.
#'
#' @export GVN
#' @param y Noisy data.
#' @param A Adjacency matrix.
#' @param L Laplacian matrix.
#' @examples
#' data(minnesota)
#' A <- minnesota$A
#' L <- laplacian_mat(A)
#' x <- minnesota$xy[ ,1]
#' n <- length(x)
#' f <- sin(x)
#' sigma <- 0.1
#' noise <- rnorm(n, sd = sigma)
#' y <- f + noise
#' sigma^2
#' GVN(y, A, L)
#' @references
#' von Neumann, J. (1941). Distribution of the ratio of the mean square successive difference to the variance. \emph{Ann. Math. Statistics}, 35(3), 433--451.
#'
#' de Loynes, B., Navarro, F., Olivier, B. (2019). Data-driven Thresholding in Denoising with Spectral Graph Wavelet Transform. arXiv preprint arXiv:1906.01882.

GVN <- function(y, A, L) {
  sig <- 0.5 * sum(A * outer(y, y, "-")^2)/sum(diag(L))
  return(sig)
}
