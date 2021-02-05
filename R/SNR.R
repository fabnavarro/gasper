#' Signal to Noise Ratio.
#'
#' @export SNR
#' @param x Original reference signal.
#' @param y Restored or noisy signal.
#' @return Signal to Noise ratio.
#' @examples
#' x <- cos(seq(0, 10, length=100))
#' y <- x + rnorm(100, sd=0.5)
#' SNR(x, y)

SNR <- function(x, y) {
  Asignal <- norm(as.matrix(x), "2")
  Anoise <- norm(as.matrix(x)-as.matrix(y), "2")
  return(20*log10(Asignal/Anoise))
}
