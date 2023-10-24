#' Compute the Signal to Noise Ratio (SNR).
#'
#' \code{SNR} computes the SNR between two signals, indicating the level of desired signal to the level of background noise.
#'
#' @export SNR
#' @param x Original reference signal (numeric vector/matrix).
#' @param y Restored or noisy signal (numeric vector/matrix).
#' @return \code{SNR} Signal to Noise Ratio (numeric).
#'
#' @details
#'
#' Higher values of SNR indicate a cleaner signal compared to the noise level. The SNR is computed as the ratio of the power of the signal (or the square of the Euclidean norm of the signal) to the power of the noise (or the square of the Euclidean norm of the signal difference), represented in decibels (dB).
#'
#' The SNR is defined by:
#' \deqn{SNR(x,y) = 20 \log_{10}\left(\frac{\|x\|_2}{\|x-y\|_2}\right)}{SNR(x,y) = 20*log10( norm(x, "2") / norm(x-y, "2") )}
#'
#'
#' @examples
#' x <- cos(seq(0, 10, length=100))
#' y <- x + rnorm(100, sd=0.5)
#' SNR(x, y)
#' @seealso \code{\link{PSNR}}

SNR <- function(x, y) {
  Asignal <- norm(as.vector(x), "2")
  Anoise <- norm(as.vector(x)-as.vector(y), "2")
  return(20*log10(Asignal/Anoise))
}
