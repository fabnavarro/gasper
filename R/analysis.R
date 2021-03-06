#' Analysis operator.
#'
#' Compute the analysis operator for coefficient y.
#'
#' @export analysis
#' @param y Graph signal to analyze.
#' @param tf frame coefficients.
#' @return \code{coef} Transform coefficients.
#' @seealso \code{\link{synthesis}}, \code{\link{tight_frame}}

analysis <- function(y, tf) {
  coef <- tf %*% y
  return(coef)
}

