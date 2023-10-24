#' Apply Beta Threshold to Data.
#'
#' This function performs a generalized thresholding operation on noisy data based on the parameter \eqn{\beta}{beta}. Soft thresholding is applied when \eqn{\beta = 1} and JS thresholding is applied when \eqn{\beta = 2}.
#'
#' @export betathresh
#' @param y Noisy data (numeric vector or matrix).
#' @param t Threshold (non-negative numeric).
#' @param beta Thresholding type. \eqn{\beta = 1} corresponds to soft thresholding and \eqn{\beta = 2} corresponds to James-Stein thresholding.
#' @return \code{x} Filtered result.
#' @details
#'
#' Soft thresholding is commonly used in wavelet-based denoising techniques, where coefficients below a certain threshold are shrunk toward zero. JS thresholding is another variant. The implementation includes a small constant for numerical stability when computing the thresholding operation.
#'
#' The thresholding operator is defined as:
#' \deqn{
#' \tau(x,t) = x \times \max \left( 1 - t^{\beta} \times |x|^{-\beta}, 0 \right)
#' }{\tau(x,t) = x * max(1 - t^beta * |x|^-beta, 0)}
#'
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
#' De Loynes, B., Navarro, F., & Olivier, B. (2021). Data-driven thresholding in denoising with spectral graph wavelet transform. Journal of Computational and Applied Mathematics, 389, 113319.

betathresh <- function(y, t, beta) {
  x <- pmax(0, 1 - t^beta/pmax(abs(y), 1e-10)^beta) * y
  return(x)
}
