#' High Pass Filter Von Neumann Estimator
#'
#' \code{HPFVN} computes graph extension of the Von Neummann variance estimator using finest scale coefficients (as in classical wavelet approaches).
#'
#' @export HPFVN
#' @param wcn Numeric vector of noisy wavelet coefficients.
#' @param b numeric parameter that control the number of scales.
#' @param evalues Numeric vector corresponding to Laplacian spectrum.
#' @param filter_func Function used to compute the filter values. By default, it uses the \code{\link{zetav}} function but other frame filters can be passed.
#' @param filter_params List of additional parameters required by filter_func. Default is an empty list.
#'
#' @note
#' \code{HPFVN} can be adapted for other filters by passing a different filter function to the \code{filter_func} parameter.
#'
#' The computation of \eqn{k_{\text{max}}}{k_max} using \eqn{\lambda_{\text{max}}}{lambda_max} and \eqn{b}{b} applies primarily to the default \code{zetav} filter. It can be overridden by providing it in the \code{filter_params} list for other filters.
#'
#' @examples
#' \dontrun{
#' A <- grid1$sA
#' L <- laplacian_mat(A)
#' x <- grid1$xy[ ,1]
#' n <- length(x)
#' val1 <- eigensort(L)
#' evalues <- val1$evalues
#' evectors <- val1$evectors
#' f <- sin(x)
#' sigma <- 0.1
#' noise <- rnorm(n, sd = sigma)
#' y <- f + noise
#' b <- 2
#' wcn <- forward_sgwt(y, evalues, evectors, b=b)
#' sigma^2
#' HPFVN(wcn, evalues, b)}
#' @seealso \code{\link{GVN}}
#' @references
#' Donoho, D. L., & Johnstone, I. M. (1994). Ideal spatial adaptation by wavelet shrinkage. biometrika, 81(3), 425-455.
#'
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.
#'
#' von Neumann, J. (1941). Distribution of the ratio of the mean square successive difference to the variance. \emph{Ann. Math. Statistics}, 35(3), 433--451.
#'
#' @details
#' The High Pass Filter Von Neumann Estimator (HPFVN) is the graph analog of the classical Von Neumann estimator, focusing on the finest scale coefficients. It leverages the characteristics of the graph signal's wavelet coefficients to estimate the variance:
#' \deqn{\hat \sigma^2 = \frac{\sum_{i=nJ+1}^{n(J+1)} (\mathcal{W} y)^2_i}{\mathrm{Tr}~\psi_J(L)}}

HPFVN <- function(wcn, evalues, b,
                  filter_func=zetav,
                  filter_params=list()){
  n <- length(evalues)
  kmax <- floor(log(max(evalues))/log(b)) + 2
  if ("kmax" %in% names(filter_params)) {
    kmax <- filter_params$kmax
    filter_params$kmax <- NULL
  }
  #- Estimated variance at each scale
  # scay <- rep(0, kmax+1)
  # for(j in 1:(kmax+1)) {
  #   scay[j] <- sqrt(sum(wcn[(n*(j-1)+1):(n*j)]^2)/
  #                     sum(zetav(evalues, j-1, b)))
  # }
  # return(scay)
  #sig <- sum(wcn[(n*kmax+1):(n*(kmax+1))]^2)/
  #               sum(zetav(evalues, kmax, b))
  sig <- sum(wcn[(n*kmax+1):(n*(kmax+1))]^2) /
    sum(do.call(filter_func,
                c(list(evalues, kmax, b),
                  filter_params)))
  return(sig)
}
