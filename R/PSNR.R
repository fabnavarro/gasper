#' Peak Signal to Noise Ratio.
#'
#' Compute the Peack Signal to Noise Ratio, defined by:
#' \deqn{PSNR(x,y)=10 \log_{10}( \max(\max(x),\max(y))^2 / |x-y|^2 )}{PSNR(x,y)=10*log10( max(max(x),max(y))^2 / |x-y|^2 )}
#'
#' @export PSNR
#' @param x Original reference signal/image.
#' @param y Restored or noisy signal/image.
#' @return Peak Signal to Noise ratio.
#' @examples
#' x <- cos(seq(0, 10, length=100))
#' y <- x + rnorm(100, sd=0.5)
#' PSNR(x, y)
#' @seealso \code{\link{SNR}}

PSNR <- function(x, y){
  d <- mean((x-y)^2)
  m <- max(abs(x))
  p <- 10*log10(m^2/d)
  return(p)
}
