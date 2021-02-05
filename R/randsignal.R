#' Generate random signal with varying regularity.
#'
#' Generate \eqn{f = A^k x_eta / r^k}{f = A^k * x_eta / r^k},
#' with A the adjacency matrix and
#' x_eta realization of Bernoulli random variables of parameter eta and r the largest eigenvalue (in magnitude).
#'
#' @export randsignal
#' @param eta Smoothness parameter.
#' @param k Smoothness parameter.
#' @param A Adjacency matrix.
#' @param r Optional argument corresponding to the largest eigenvalue (in magnitude), avoids the need to calculate the full spectrum.
#' @return \code{f} output signal.

randsignal <- function(eta, k, A, r){
  if(missing(r)){
    dspec <- eigen(A, only.values = T)
    vp <- dspec$values
    r <- max(abs(vp))
    xi <- as.matrix(rbinom(nrow(A), 1, eta))
    Ak <- powermat(A, k)
    f  <- Ak%*%xi/r^k
  }
  else{
    xi <- as.matrix(rbinom(nrow(A), 1, eta))
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
