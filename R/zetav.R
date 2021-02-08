#' Evaluates tight-frame kernel functions
#'
#' Evaluates kernel associated with the particular (Littlewood-Paley type) tight-frame construction, based on a unity partition, proposed in the reference papers.
#'
#' @export zetav
#' @param x Support to evaluate the kernel (vector).
#' @param k Scale index (scalar).
#' @param b Parameter that control the number of scales (scalar).
#' @references
#' Coulhon, T., Kerkyacharian, G., & Petrushev, P. (2012). Heat kernel generated frames in the setting of Dirichlet spaces. Journal of Fourier Analysis and Applications, 18(5), 995-1066.
#'
#' Göbel, F., Blanchard, G., von Luxburg, U. (2018). Construction of tight frames on graphs and application to denoising. In Handbook of Big Data Analytics (pp. 503-522). Springer, Cham.
#'
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.

zetav <- function(x, k, b) {
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
  if (k == 0) {
    return(gv(x, b))
  } else {
    return(gv(b^(-k) * x, b) - gv(b^(-k + 1) * x, b))
  }
}
