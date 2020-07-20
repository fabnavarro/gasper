#' Generate random signal with varying regularity.
#'
#' Generate f  = A^k * x_eta / lambda_1^k,
#' with A the adjacency matrix and
#' x_eta realization of Bernoulli random variables of parameter eta.
#'
#' @export randsignal
#' @param eta Smoothness parameter.
#' @param k Smoothness parameter.
#' @param A Adjacency matrix.
#' @return \code{f} output signal.

randsignal <- function(eta, k, A){
  dspec <- eigen(A, only.values = T)
  vp <- dspec$values
  r <- max(abs(vp)) #vp could be complex
  xi <- as.matrix(rbinom(nrow(A), 1, eta))
  Ak <- powermat(A, k)
  f  <- Ak%*%xi/r^k
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
