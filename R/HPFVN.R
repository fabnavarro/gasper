#' High Pass Filter Von Neumann Estimator.
#'
#' Graph extension of the Von Neummann variance estimator using finest scale coefficients.
#'
#' @export HPFVN
#' @param wcn Noisy wavelet coefficients.
#' @param b Parameter that control the number of scales.
#' @param evalues Laplacian spectrum.
#' @examples
#' graphname <- "grid1"
#' groupname <- "AG-Monien"
#' graph <- download_graph(graphname,groupname)
#' A <- graph$sA
#' L <- laplacian_mat(A)
#' n <- nrow(L)
#' val1 <- eigensort(L)
#' evalues <- val1$evalues
#' evectors <- val1$evectors
#' lmax <- max(evalues)
#' f <- randsignal(eta=0.01,k=5,A=A)
#' sigma <- 0.1
#' noise <- rnorm(n, sd = sigma)
#' y <- f + noise
#' b <- 2
#' wcn <- forward_sgwt(y, evalues, evectors, b=b)
#' HPFVN(wcn, evalues, b)
#' @references
#' von Neumann, J. (1941). Distribution of the ratio of the mean square successive difference to the variance. \emph{Ann. Math. Statistics}, 35(3), 433--451.
#'
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.

HPFVN <- function(wcn, evalues, b){
  n <- length(evalues)
  kmax <- length(wcn)/n
  #- Estimated variance at each scale
  # scay <- rep(0, kmax+1)
  # for(j in 1:(kmax+1)) {
  #   scay[j] <- sqrt(sum(wcn[(n*(j-1)+1):(n*j)]^2)/
  #                     sum(zetav(evalues, j-1, b)))
  # }
  # return(scay)
  sig <- sqrt(sum(wcn[(n*kmax):(n*(kmax+1))]^2)/
                 sum(zetav(evalues, kmax, b)))
  return(sig)
}

zetav <- function(x, k, b) {
  if (k == 0) {
    return(gv(x, b))
  } else {
    return(gv(b^(-k) * x, b) - gv(b^(-k + 1) * x, b))
  }
}

gv <- function(x, b) {
  low <- outer(x, 0, "<")
  mid1 <- outer(x, 0, ">=")
  mid2 <- outer(x, 1/b, "<=")
  mid3 <- outer(x, 1/b, ">=")
  mid4 <- outer(x, 1, "<=")
  up <- outer(x, 1, ">")

  gg <- rep(0, length(x))
  gg[low] <- 1
  gg[mid1 & mid2] <- 1
  gg[mid3 & mid4] <- b * x[mid3 & mid4]/(1 - b) + b/(b - 1)
  gg[up] <- 0
  return(gg)
}

