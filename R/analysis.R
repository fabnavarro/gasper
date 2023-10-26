#' Compute the Analysis Operator for a Graph Signal
#'
#' \code{analysis} computes the transform coefficients of a given graph signal using the provided frame coefficients.
#'
#' @export analysis
#' @param y Numeric vector or matrix representing the graph signal to analyze.
#' @param tf Numeric matrix of frame coefficients.
#' @return \code{coef} Numeric vector or matrix of transform coefficients of the graph signal.
#'
#' @details
#' The \code{analysis} operator uses the frame coefficients to transform a given graph signal into its representation in the transform domain. It is defined by the linear map \eqn{T_{\mathfrak F} : \mathbb R^V \rightarrow \mathbb R^I}{T_{F} : R^V -> R^I}. Given a function \eqn{f \in \mathbb R^V}{f in R^V}, the analysis operation is defined as:
#' \deqn{T_{\mathfrak F}f=(\langle f,r_i \rangle)_{i \in I}}{T_Ff = (f, r_i) for all i in I}
#' where \eqn{r_i}{r_i} are the frame coefficients.
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
