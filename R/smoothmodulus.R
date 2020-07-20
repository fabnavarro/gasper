#' Modulus of smoothness.
#'
#' Compute the modulus of smoothness of a graph signal.
#'
#' @export smoothmodulus
#' @param f Signal.
#' @param A Adjacency matrix.
#' @param L Laplacian matrix.
#' @examples
#' data(minnesota)
#' A <- minnesota$A
#' L <- laplacian_mat(A)
#' x <- minnesota$xy[ ,1]
#' f <- sin(x)
#' smoothmodulus(f, A, L)

smoothmodulus <- function(f, A, L) {
  modulef <- 0.5 * sum(A * outer(f, f, "-")^2)
  return(modulef)
}
