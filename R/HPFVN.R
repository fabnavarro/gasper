#' High Pass Filter Von Neumann Estimator.
#'
#' Graph extension of the Von Neummann variance estimator using finest scale coefficients.
#'
#' @export HPFVN
#' @param wcn Noisy wavelet coefficients.
#' @param b Parameter that control the number of scales.
#' @param evalues Laplacian spectrum.
#' @examples
#' matrixname <- "grid1"
#' groupname <- "AG-Monien"
#' graph <- download_graph(matrixname,groupname)
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
