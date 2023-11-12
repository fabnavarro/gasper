#' Generate Random Signal with Varying Regularity
#'
#' \code{randsignal} constructs a random signal with specific regularity properties, utilizing the adjacency matrix \code{A} of the graph, a smoothness parameter \code{eta}, and an exponent \code{k}.
#'
#' @export randsignal
#' @importFrom methods as
#' @importFrom Matrix Matrix
#' @importFrom stats rbinom
#' @importFrom RSpectra eigs
#' @param eta Numeric. Smoothness parameter (between 0 and 1).
#' @param k Interger. Smoothness parameter.
#' @param A Adjacency matrix. Must be symmetric.
#' @param r Optional. Largest eigenvalue of \code{A} in magnitude (obtained using the \code{eigs} function from the \code{RSpectra} package if not provided).
#'
#' @details
#'
#' This method is inspired by the approach described in the first referenced paper.
#'
#' The generated signal is formulated as
#' \eqn{f = A^k x_{\eta} / r^k}{f = A^k x_eta / r^k}
#' where \eqn{x_{\eta}}{x_eta} represents Bernoulli random variables, and \eqn{r}{r} is the largest eigenvalue of the matrix \eqn{A}{A}.
#'
#' The power \eqn{k}{k} essentially captures the influence of a node's \eqn{k}{k}-hop neighborhood in the generated signal, implying that a higher \eqn{k}{k} would aggregate more neighborhood information resulting in a smoother signal.
#'
#' The normalization by the largest eigenvalue ensures that the signal remains bounded. This signal generation can be related to the Laplacian quadratic form that quantifies the smoothness of signals on graphs. By controlling the parameters \eqn{\eta}{eta} and \eqn{k}{k}, we can modulate the smoothness or regularity of the generated signal.
#'
#'@note
#' While the \code{randsignal} function uses the adjacency matrix to parameterize and generate signals reflecting node-to-node interactions, the smoothness of these signals can subsequently be measured using the \code{smoothmodulus} function.
#'
#'The generation is carried out in sparse matrices format in order to scale up.
#'
#' @return \code{f} a numeric vector representing the output signal.
#' @seealso \code{\link{smoothmodulus}}
#' @examples
#' \dontrun{
#' # Generate a signal with smoothness parameters eta = 0.7 and k = 3
#' f <- randsignal(eta = 0.7, k = 3, A = grid1$sA)
#' }
#' @references
#' Behjat, H., Richter, U., Van De Ville, D., & Sörnmo, L. (2016). Signal-adapted tight frames on graphs. IEEE Transactions on Signal Processing, 64(22), 6017-6029.
#'
#' de Loynes, B., Navarro, F., & Olivier, B. (2021). Data-driven thresholding in denoising with spectral graph wavelet transform. Journal of Computational and Applied Mathematics, 389, 113319.

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

