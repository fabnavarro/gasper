#' Apply Beta Threshold to Data
#'
#' \code{betathresh} performs a generalized thresholding operation on the data \code{y}. The thresholding operation is parameterized by the parameter \code{beta}.
#'
#' @export betathresh
#' @param y Numeric vector or matrix representing the noisy data.
#' @param t Non-negative numeric value representing the threshold.
#' @param beta Numeric value indicating the type of thresholding.
#' @return \code{x} Numeric vector or matrix of the filtered result.
#' @details
#'
#'The function offers flexibility by allowing for different types of thresholding based on the \code{beta} parameter. Soft thresholding, commonly used in wavelet-based denoising corresponds to \code{beta}=1 . James-Stein thresholding corresponds to \code{beta}=2. The implementation includes a small constant for numerical stability when computing the thresholding operation.
#'
#' The thresholding operator is defined as:
#' \deqn{
#' \tau(x,t) = x \max \left( 1 - t^{\beta} |x|^{-\beta}, 0 \right)
#' }{\tau(x,t) = x * max(1 - t^beta * |x|^-beta, 0)}
#' with \eqn{\beta \geq 1}{beta >= 1}.
#'
#' @examples
#' # Define a 2x2 matrix
#' mat <- matrix(c(2, -3, 1.5, -0.5), 2, 2)
#'
#' # Apply soft thresholding with a threshold of 1
#' betathresh(mat, 1, 1)
#' @references
#' Donoho, D. L., & Johnstone, I. M. (1995). Adapting to unknown smoothness via wavelet shrinkage. Journal of the american statistical association, 90(432), 1200-1224.
#'
#' de Loynes, B., Navarro, F., & Olivier, B. (2021). Data-driven thresholding in denoising with spectral graph wavelet transform. Journal of Computational and Applied Mathematics, 389, 113319.

betathresh <- function(y, t, beta=2) {
  x <- pmax(0, 1 - t^beta/pmax(abs(y), 1e-10)^beta) * y
  return(x)
}
