#' Swiss roll graph generation
#'
#' Map the square \eqn{[0,1]^2}{[0,1]^2} in swiss roll
#' for all \eqn{x,y} in \eqn{[0,1]^2}{[0,1]^2}, set
#' \deqn{Sx=\pi \sqrt{(b^2-a^2)x + a^2)}}{Sx=pi*sqrt((b**2-a**2)*x+a**2)}
#' \deqn{Sy=\pi (b^2-a^2)y/2}{Sy=pi**(b**2-a**2)*y/2}
#'
#' @export swissroll
#' @import stats
#' @param a,b Shape parameters.
#' @param N Number of points drawn.
#' @param seed Optionally specify a RNG seed (for reproducible experiments).
#' @return N x 3 array for 3d points.
#' @examples
#' pts <- swissroll(N=500, seed=0, a=1, b=4)
#' scatterplot3d::scatterplot3d(pts, y=NULL, z=NULL)
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
