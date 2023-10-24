#' Compute the Analysis Operator for a Graph Signal.
#'
#' \code{analysis} function computes the transform coefficients of a given graph signal using the provided frame coefficients.
#'
#' @export analysis
#' @param y Graph signal to analyze (numeric vector/matrix).
#' @param tf Frame coefficients (numeric matrix).
#' @return \code{coef} Transform coefficients of the graph signal (numeric vector/matrix).
#'
#' @details
#' The \code{analysis} operator uses the frame coefficients to transform a given graph signal into its representation in the transform domain.
#'
#' The transform is computed as:
#' \deqn{coef = tf \times y}{coef = tf \%*\% y}
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
#' }
#'
#' @seealso \code{\link{synthesis}}, \code{\link{tight_frame}}

analysis <- function(y, tf) {
  coef <- tf %*% y
  return(coef)
}
