#' Swiss Roll Graph Generation
#'
#' Generates points for a Swiss roll graph. The function maps points from the square \eqn{[0,1]^2} into the Swiss roll using the specified transformations.
#'
#' @export swissroll
#' @importFrom stats runif
#' @param a,b Shape parameters (numeric).
#' @param N Number of points drawn (numeric).
#' @param seed Optionally specify a RNG seed for reproducible experiments (numeric).
#' @return N x 3 array for 3d points.
#'
#' @details Given points \eqn{(x,y)} within the unit square \eqn{[0,1]^2}, the Swiss roll transformation is achieved using:
#' \eqn{Sx = \pi \sqrt{(b^2 - a^2) x + a^2}} and
#' \eqn{Sy = \frac{\pi^2 (b^2 - a^2) y}{2}}.
#' The transformed \eqn{(x,y)} coordinates are then projected into 3D space to produce the characteristic rolled shape.
#'
#' @examples
#' \dontrun{
#' pts <- swissroll(N=500, seed=0, a=1, b=4)
#' plot3D::scatter3D(pts[,1], pts[,2], pts[,3], colvar=NULL, col="red")
#' }
#' @seealso \code{\link{adjacency_mat}}

swissroll <- function(N = 500, seed = NULL, a = 1, b = 4) {
  set.seed(seed)
  x <- runif(N)
  y <- runif(N)
  Sx <- pi * sqrt((b^2 - a^2) * x + a^2)
  px <- Sx * cos(Sx)
  py <- pi^2 * (b^2 - a^2) * y/2
  pz <- Sx * sin(Sx)
  return(array(c(px, py, pz), c(N, 3)))
}
