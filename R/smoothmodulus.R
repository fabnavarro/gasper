#' Modulus of smoothness.
#'
#' Compute the modulus of smoothness of a graph signal.
#'
#' @export smoothmodulus
#' @param f Signal.
#' @param A Adjacency matrix (sparse or dense).
#' @examples
#' data(minnesota)
#' A <- minnesota$A
#' x <- minnesota$xy[ ,1]
#' f <- sin(x)
#' smoothmodulus(f, A)

smoothmodulus <- function(f, A) {
  modulef <- 0.5 * sum(A * outer(f, f, "-")^2)
  return(modulef)
}
