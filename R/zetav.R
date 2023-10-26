#' Evaluate Localized Tight-Frame Filter Functions
#'
#' \code{zetav} evaluates the filters associated with a specific tight-frame construction.
#'
#' @export zetav
#' @param x A vector representing the support on which to evaluate the filter
#' @param k A scalar representing the scale index.
#' @param b A scalar parameter that governs the number of scales (b=2 default).
#' @return Returns a numeric vector of evaluated filter values.
#' @examples
#' \dontrun{
#'   x <- seq(0, 2, by = 0.1)
#'   g <- zetav(x, 1, 2)
#'   plot(x, g, type = "l")
#' }
#'
#' @details
#' The function \code{zetav} evaluates the partition of unity functions \eqn{\psi} following the methodology described in the references similar to the Littlewood-Paley type, based on a partition of unity, as proposed in the reference papers. This approach, inspired by frame theory, facilitates the construction of filter banks, ensuring effective spectral localization.
#'
#' A finite collection \eqn{(\psi_j)_{j=0, \ldots, J}} is a finite partition of unity on the compact interval \eqn{[0, \lambda_{\mathrm{max}}]}. It satisfies:
#' \deqn{
#' \psi_j : [0,\lambda_{\mathrm{max}}] \rightarrow [0,1]~\textrm{for all}~ j \in \{1,\ldots,J\}~\textrm{and}~\forall \lambda \in [0,\lambda_{\mathrm{max}}],~\sum_{j=0}^J \psi_j(\lambda)=1.
#' }{
#' psi_j: [0, lambda_{\mathrm{max}}] -> [0,1] for all j in J and for all lambda in [0, lambda_{\mathrm{max}}], sum(psi_j(lambda)) from j=0 to J = 1.
#' }
#'
#' Let \eqn{\omega : \mathbb R^+ \rightarrow [0,1]} be a function with support in [0,1]. It's defined as:
#' \deqn{
#' \omega(x) = \begin{cases}
#' 1 & \text{if } x \in [0,b^{-1}] \\
#' b \cdot \frac{x}{1 - b} + \frac{b}{b - 1} & \text{if } x \in (b^{-1}, 1] \\
#' 0 & \text{if } x > 1
#' \end{cases}
#' }{
#' omega(x) = {
#'  1                  if 0 <= x <= 1/b
#'  b*x/(1-b) + b/(b-1) if 1/b < x <= 1
#'  0                  if x > 1
#' }
#' }
#' For a given \eqn{b > 1}. Based on this function \eqn{\omega}, the partition of unity functions \eqn{\psi} are defined as:
#' \deqn{
#' \psi_0(x) = \omega(x)
#' }{
#' psi_0(x) = omega(x)
#' }
#' and for all \eqn{j \geq 1}{j >= 1}:
#' \deqn{
#' \psi_j(x) = \omega(b^{-j} x) - \omega(b^{-j+1} x)
#' }{
#' psi_j(x) = omega(b^(-j) x) - omega(b^(-j+1) x)
#' }
#' where \eqn{J} is defined by:
#' \deqn{
#' J = \left \lfloor \frac{\log \lambda_{\mathrm{max}}}{\log b} \right \rfloor + 2
#' }{
#' J = floor(log(lambda_{\mathrm{max}}) / log(b)) + 2
#' }
#'
#' Given this finite partition of unity \eqn{(\psi_j)_{j=0, \ldots, J}}, the Parseval identity implies that the following set of vectors forms a tight frame:
#' \deqn{
#' \mathfrak F = \left \{ \sqrt{\psi_j}(\mathcal{L})\delta_i : j=0, \ldots, J, i \in V \right \}.
#' }{
#' F = {sqrt(psi_j)(L)delta_i, j=0, ..., J, i in V}.
#' }
#'
#' @references
#' Coulhon, T., Kerkyacharian, G., & Petrushev, P. (2012). Heat kernel generated frames in the setting of Dirichlet spaces. Journal of Fourier Analysis and Applications, 18(5), 995-1066.
#'
#' Göbel, F., Blanchard, G., von Luxburg, U. (2018). Construction of tight frames on graphs and application to denoising. In Handbook of Big Data Analytics (pp. 503-522). Springer, Cham.
#'
#' Leonardi, N., & Van De Ville, D. (2013). Tight wavelet frames on multislice graphs. IEEE Transactions on Signal Processing, 61(13), 3357-3367.
#'
#' de Loynes, B., Navarro, F., Olivier, B. (2021). Data-driven thresholding in denoising with Spectral Graph Wavelet Transform. Journal of Computational and Applied Mathematics, Vol. 389.

zetav <- function(x, k, b=2) {
  gv <- function(x, b) {
    low <- outer(x, 0, "<")
    mid1 <- outer(x, 0, ">=")
    mid2 <- outer(x, 1/b, "<=")
    mid3 <- outer(x, 1/b, ">=")
    mid4 <- outer(x, 1, "<=")
    up <- outer(x, 1, ">")

    gg <- rep(0, length(x))
    gg[low] <- 0
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
