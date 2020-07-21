#' Apply Beta Threshold.
#'
#' @export betathresh
#' @param y Noisy Data.
#' @param t Threshold.
#' @param b Thresholding type (b=1: soft, b=2: JS).
#' @return \code{x} Filtered result.

betathresh <- function(y, t, b) {
  x <- pmax(0, 1 - t^b/pmax(abs(y), 1e-10)^b) * y
  return(x)
}
