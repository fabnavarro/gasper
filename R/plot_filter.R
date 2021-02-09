#' Plot tight-frame filters.
#'
#' Plot tight-frame kernels/filters.
#'
#' @export plot_filter
#' @importFrom graphics lines plot
#' @param lmax Largest eigenvalues of the Laplacian matrix.
#' @param b Parameter that control the number of scales.
#' @param N Number of discretization points (by default N=1000).

plot_filter <- function(lmax, b, N=1000){
  kmax <- floor(log(lmax)/log(b)) + 2
  x <- seq(0, lmax, length.out = 1000)
  plot(x, sqrt(zetav(x, 0, b)), type="l",
       xlab = expression(lambda), ylab = "", lwd=2)
  for (j in 1:kmax){
    lines(x, sqrt(zetav(x, j, b)), col=j+1,
          lwd=2)
  }
}
