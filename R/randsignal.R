#' Generate Random Signal with Varying Regularity.
#'
#' \code{randsignal} constructs a random signal with specific regularity properties, utilizing the adjacency matrix \eqn{A}{A} of the graph, a smoothness parameter \eqn{\eta}{eta}, and an exponent \eqn{k}{k}. The generation is carried out in sparse matrices in order to scale up.
#'
#' @export randsignal
#' @importFrom methods as
#' @importFrom Matrix Matrix
#' @importFrom stats rbinom
#' @importFrom RSpectra eigs
#' @param eta Smoothness parameter (numeric, between 0 and 1).
#' @param k Smoothness parameter (integer).
#' @param A Adjacency matrix. Must be symmetric (matrix).
#' @param r Optional (numeric). Corresponding to the largest eigenvalue of \code{A} in magnitude (numeric).
#'
#' @details The generated signal is formulated as
#' \eqn{f = A^k x_{\eta} / r^k}{f = A^k x_eta / r^k}
#' where \eqn{x_{\eta}}{x_eta} represents Bernoulli random variables, and \eqn{r}{r} is the largest eigenvalue of the matrix \eqn{A}{A}. The power \eqn{k}{k} essentially captures the influence of a node's \eqn{k}{k}-hop neighborhood in the generated signal, implying that a higher \eqn{k}{k} would aggregate more neighborhood information resulting in a smoother signal. The normalization by the largest eigenvalue ensures that the signal remains bounded. This signal generation can be related to the Laplacian quadratic form that quantifies the smoothness of signals on graphs. B y controlling the parameters \eqn{\eta}{eta} and \eqn{k}{k}, we can modulate the smoothness or regularity of the generated signal.
#'
#' @return \code{f} a numeric vector representing the output signal.
#' @examples
#' \dontrun{
#' # Generate a signal with smoothness parameters eta = 0.7 and k = 3
#' f <- randsignal(eta = 0.7, k = 3, A = grid1$sA)
#' }

randsignal <- function(eta, k, A, r){
  if(inherits(A, 'sparseMatrix')==F){
    if(isSymmetric.matrix(A)){
      A <- as(A, "dsCMatrix")
    }
    else{
      A <- as(A, "dgCMatrix")
    }
  }
  if(missing(r)){
    #dspec <- eigen(A, only.values = T)
    #vp <- dspec$values
    vp <- eigs(A, k=1)$values
    r <- max(abs(vp))
    xi <- Matrix(rbinom(nrow(A), 1, eta))
    Ak <- powermat(A, k)
    f  <- Ak%*%xi/r^k
  }
  else{
    xi <- Matrix(rbinom(nrow(A), 1, eta))
    Ak <- powermat(A, k)
    f  <- Ak%*%xi/r^k
  }
  return(as.vector(f))
}

powermat <- function(A, k) {
  Ak <- A
  i <- 2
  while ( i <= k ) {
    Ak <- Ak %*% A
    i <- i + 1
  }
  return(Ak)
}


#Generate \eqn{f = A^k x_{\eta} / r^k}{f = A^k * x_eta / r^k}, with A the adjacency matrix and \eqn{x_{\eta}} realization of Bernoulli random variables of parameter \eqn{\eta} and \eqn{r} the largest eigenvalue (in magnitude). The generation is carried out in sparse matrices in order to scale up.
