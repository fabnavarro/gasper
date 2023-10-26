#' Plot Tight-Frame Filters
#'
#' \code{plot_filter} provides a graphical representation of tight-frame filters as functions of the eigenvalues of the Laplacian matrix.
#'
#' @export plot_filter
#' @importFrom graphics lines plot
#' @param lmax Largest eigenvalue of the Laplacian matrix (numeric scalar).
#' @param b Parameter that controls the number of scales (numeric scalar).
#' @param N Number of discretization points for the x-axis. By default, N is set to 1000.
#' @param filter_func Function used to compute the filter values. By default, it uses the \code{\link{zetav}} function but other frame filters can be pass.
#' @param filter_params List of additional parameters required by filter_func. Default is an empty list.
#' @details
#' The plotted functions represent the square root of the values given by the \code{zetav} function at different scales.
#'
#' This function plots the square roots of the functions forming the partition of unity, corresponding to the construction of tight frames on the graph. The square root operation is essential as it ensures the Parseval identity, making the constructed frame "tight" and preserving the energy of signals on the graph when mapped to their frame representation.
#'
#' \code{plot_filter} first determines the number of scales based on the largest eigenvalue \eqn{\lambda_{\text{max}}}{lambda_max} and the parameter \eqn{b}{b} as:
#' \deqn{
#' k_{\text{max}} = \left\lfloor \frac{\log(\lambda_{\text{max}})}{\log(b)} \right\rfloor + 2
#' }{k_max = floor(log(lambda_max)/log(b)) + 2}
#'
#' The function then plots the square root of the values given by the \code{\link{zetav}} function over the range [0, \eqn{\lambda_{\text{max}}}{lambda_max}] for each scale.
#'
#' @note
#' \code{plot_filter} can be adapted for other filters by passing a different filter function to the \code{filter_func} parameter.
#' The computation of \eqn{k_{\text{max}}}{k_max} using \eqn{\lambda_{\text{max}}}{lambda_max} and \eqn{b}{b} applies primarily to the default \code{zetav} filter. It can be overridden by providing it in the \code{filter_params} list for other filters.
#'
#' @examples
#' plot_filter(6,2)
#' @seealso
#' \code{\link{zetav}}
#' @references
#' Coulhon, T., Kerkyacharian, G., & Petrushev, P. (2012). Heat kernel generated frames in the setting of Dirichlet spaces. Journal of Fourier Analysis and Applications, 18(5), 995-1066.
#'
#' Göbel, F., Blanchard, G., von Luxburg, U. (2018). Construction of tight frames on graphs and application to denoising. In Handbook of Big Data Analytics (pp. 503-522). Springer, Cham.
#'
#' Leonardi, N., & Van De Ville, D. (2013). Tight wavelet frames on multislice graphs. IEEE Transactions on Signal Processing, 61(13), 3357-3367.
#'
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.

plot_filter <- function(lmax, b, N=1000,
                        filter_func=zetav, filter_params=list()){
  kmax <- floor(log(lmax)/log(b)) + 2
  if ("kmax" %in% names(filter_params)) {
    kmax <- filter_params$kmax
    filter_params$kmax <- NULL
  }
  x <- seq(0, lmax, length.out = N)
  plot(x,
       sqrt(do.call(filter_func, c(list(x, 0, b), filter_params))),
       type="l", xlab = expression(lambda), ylab = "", lwd=2)
  for (j in 1:kmax){
    lines(x,
          sqrt(do.call(filter_func, c(list(x, j, b),
                                      filter_params))), col=j+1, lwd=2)
  }
}

# plot_filter <- function(lmax, b, N=1000){
#   kmax <- floor(log(lmax)/log(b)) + 2
#   x <- seq(0, lmax, length.out = 1000)
#   plot(x, sqrt(zetav(x, 0, b)), type="l",
#        xlab = expression(lambda), ylab = "", lwd=2)
#   for (j in 1:kmax){
#     lines(x, sqrt(zetav(x, j, b)), col=j+1,
#           lwd=2)
#   }
# }

