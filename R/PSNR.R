#' Compute the Peak Signal to Noise Ratio
#'
#' \code{PSNR} function computes the Peak Signal to Noise Ratio (PSNR) between two signals or images.
#'
#' @export PSNR
#' @param x Numeric vector/matrix. Original reference signal/image.
#' @param y numeric vector/matrix. Restored or noisy signal/image.
#' @return \code{PSNR} Numeric. Peak Signal to Noise Ratio.
#'
#' @details
#'Higher values of PSNR indicate closer similarity between the original and the compared signal or image.
#'
#' The PSNR is defined by:
#' \deqn{\mathrm{PSNR}(x,y) = 10 \log_{10}\left(\frac{\max(\max(x),\max(y))^2}{\mathrm{MSE}(x, y)}\right)}{PSNR(x,y) = 10*log10( max(max(x),max(y))^2 / mean((x-y)^2) )}
#'
#' @examples
#' x <- cos(seq(0, 10, length=100))
#' y <- x + rnorm(100, sd=0.5)
#' PSNR(x, y)
#' @seealso \code{\link{SNR}}

PSNR <- function(x, y){
  d <- mean((x - y)^2)
  m <- max(abs(x))
  p <- 10 * log10(m^2 / d)
  return(p)
}


