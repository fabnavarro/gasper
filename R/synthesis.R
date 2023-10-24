#' Compute the Synthesis Operator for Transform Coefficients.
#'
#' \code{synthesis} computes the graph signal synthesis from its transform coefficients using the provided frame coefficients.
#'
#' @export synthesis
#' @param coeff Transform coefficients of the graph signal (numeric vector/matrix).
#' @param tf Frame coefficients (numeric matrix).
#' @return \code{y} Synthesized graph signal (numeric vector/matrix).
#'
#' @details
#' The \code{synthesis} operator uses the frame coefficients to retrieve the graph signal from its representation in the transform domain.
#'
#' The synthesis is computed as:
#' \deqn{y = tf^{T} \times coeff}{y = t(coeff) \%*\% tf}
#'
#' @examples
#' \dontrun{
#' # Extract the adjacency matrix from the grid1 and compute the Laplacian
#' L <- laplacian_mat(grid1$sA)
#'
#' # Compute the spectral decomposition of L
#' decomp <- eigensort(L)
#'
#' # Generate the tight frame coefficients using the tight_frame function
#' tf <- tight_frame(decomp$evalues, decomp$evectors)
#'
#' # Create a random graph signal.
#' f <- rnorm(nrow(L))
#'
#' # Compute the transform coefficients using the analysis operator
#' coef <- analysis(f, tf)
#'
#' # Retrieve the graph signal using the synthesis operator
#' f_rec <- synthesis(coef, tf)
#' }

synthesis <- function(coeff, tf) {
  y <- t(coeff) %*% tf
  return(as.vector(y))
}

