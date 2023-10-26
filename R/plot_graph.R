#' Plot Graph
#'
#' Visualizes a graph using ggplot2. It plots nodes as points and edges as segments connecting these points.
#'
#' @details
#' The function is primarily designed to work with the output from the \code{\link{download_graph}} function. This ensures that the graph visualization upholds the structure and properties of the retrieved graph. However, the function can also be utilized to visualize custom graph structures, provided they match to the input format.
#'
#'@note If node coordinates \code{xy} are not provided, they will be calculated using spectral methods \code{\link{spectral_coords}}. For large graphs, this can be computationally intensive and may take significant time. Use with caution for large graphs if node coordinates are not supplied.
#'
#' @export plot_graph
#' @importFrom methods is
#' @importFrom Matrix summary
#' @param z A list containing graph data. This list must have the following components:
#'          \itemize{
#'            \item{sA}  An adjacency matrix or a sparse Matrix representation of the graph.
#'            \item{xy}  A matrix or dataframe containing the x and y coordinates of each node in the graph.
#'          }
#' @param size Numeric. Dot size for nodes. Default is 0.75.
#' @examples
#' data(grid1)
#' plot_graph(grid1)
#' @seealso \code{\link{download_graph}}, \code{\link{plot_signal}}, \code{\link{spectral_coords}}

plot_graph <- function(z, size=0.75) {
  if(!"xy" %in% names(z)){
    z$xy <- spectral_coords(z$sA)
  }
  if(is(z$sA, 'sparseMatrix')){
    z$sA <- summary(z$sA)
  }
  x <- z$xy[, 1]
  y <- z$xy[, 2]
  ind_i <- z$sA[, 1]
  ind_j <- z$sA[, 2]
  y1 <- x[ind_j]
  y2 <- y[ind_j]
  df1 <- data.frame(x = x, y = y)
  df2 <- data.frame(x = x[ind_i],
                    y = y[ind_i],
                    y1 = y1,
                    y2 = y2)
  p1 <- ggplot(df1, aes(x, y)) +
  geom_segment(aes(x = x, y = y,
                   xend = y1, yend = y2),
               color = "gray", data = df2) +
    geom_point(size = size) + theme_void()
  print(p1)
}
