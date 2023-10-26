#' Modulus of Smoothness for Graph Signal
#'
#' \code{smoothmodulus} computes the modulus of smoothness for a graph signal.
#'
#' @export smoothmodulus
#' @param f Numeric vector representing the signal on the graph nodes
#' @param A Adjacency matrix of the graph (matrix, can be either sparse or dense).
#' @details
#'
#' \code{smoothmodulus} provide a measure that quantifies the smoothness of a signal on a graph. In other words, it provides a measure of how much a signal varies between adjacent nodes.
#'
#' The modulus of smoothness is calculated using:
#' \eqn{\mu(f) = 0.5 \times \sum_{(i,j) \in E} A_{ij} (f_i - f_j)^2}{mu(f) = 0.5 * sum(A * (f[i] - f[j])^2)}
#' where \eqn{E}{E} is the set of edges, \eqn{A_{ij}}{A_ij} is the adjacency matrix entry for nodes i and j, and \eqn{f_i}{f_i} and \eqn{f_j}{f_j} are the signal values at nodes i and j respectively.
#'
#' This metric essentially sums up the squared differences of signal values across adjacent nodes, weighted by the adjacency matrix. A high value indicates a more variable or irregular signal across the graph, while a lower value indicates a smoother signal.
#'
#' @return A numeric scalar value indicating the modulus of smoothness for the graph signal.
#'
#' @examples
#' \dontrun{
#' A <- grid1$A
#' x <- grid1$xy[,1]
#' f <- sin(x)
#' smoothmodulus(f, A)
#' }

smoothmodulus <- function(f, A) {
  modulef <- 0.5 * sum(A * outer(f, f, "-")^2)
  return(modulef)
}
