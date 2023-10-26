#' Graph Von Neumann Variance Estimator
#'
#' \code{GVN} computes graph equivalent of the Von Neummann variance estimator.
#'
#' @export GVN
#' @importFrom Matrix diag
#' @param y Numeric vector that represents the noisy data.
#' @param A Adjacency matrix of the graph.
#' @param L Laplacian matrix of the graph.
#' @return The Graph Von Neumann variance estimate for the given noisy data.
#' @examples
#' \dontrun{
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
#' }
#' @seealso \code{\link{HPFVN}}
#' @references
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.
#'
#' von Neumann, J. (1941). Distribution of the ratio of the mean square successive difference to the variance. \emph{Ann. Math. Statistics}, 35(3), 433--451.
#'
#' @details
#' In many real-world scenarios, the noise level \eqn{\sigma^2} remains generally unknown. Given any function \eqn{g : \mathbb R_+ \rightarrow \mathbb R_+}, the expectation of the graph signal can be expressed as:
#' \deqn{\mathbf E[\widetilde f^T g(L) \widetilde f] = f^T g(L) f + \mathbf E[\xi^T g(L) \xi] = f^T g(L) f + \sigma^2 \mathrm{Tr}(g(L))}
#' A biased estimator of the variance \eqn{\sigma^2} can be given by:
#' \deqn{\hat \sigma^2_1 = \frac{\widetilde f^T g(L) \widetilde f}{\mathrm{Tr}(g(L))}}
#' Assuming the original graph signal is smooth enough that \eqn{f^T g(L) f} is negligible compared to \eqn{\mathrm{Tr}(g(L))}, \eqn{\hat \sigma^2} provides a reasonably accurate estimate of \eqn{\sigma^2}. For this function, a common choice is \eqn{g(x) = x}, leading to:
#' \deqn{\hat \sigma^2_1 = \frac{\widetilde f^T L \widetilde f}{\mathrm{Tr}(L)} = \frac{\sum_{i,j \in V} w_{ij} |\widetilde f(i) - \widetilde f(j)|^2}{2 \mathrm{Tr}(L)}}
#' This is the graph adaptation of the Von Neumann estimator, hence the term Graph Von Neumann estimator (GVN).
#'

GVN <- function(y, A, L) {
  sig <- 0.5 * sum(A * outer(y, y, "-")^2)/sum(Matrix::diag(L))
  return(sig)
}
