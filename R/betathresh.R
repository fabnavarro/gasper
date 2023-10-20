#' Apply Beta Threshold.
#'
#' @export betathresh
#' @param y Noisy Data.
#' @param t Threshold.
#' @param beta Thresholding type (beta=1: soft, beta=2: JS).
#' @return \code{x} Filtered result.

betathresh <- function(y, t, beta) {
  x <- pmax(0, 1 - t^beta/pmax(abs(y), 1e-10)^beta) * y
  return(x)
}
