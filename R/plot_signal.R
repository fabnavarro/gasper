#' Plot a Signal on Top of a Given Graph
#'
#' Visualize a signal over a graph.
#'
#'@details
#' This function allows visualization of a graph signal \code{f} superimposed on the structure of a graph defined by \code{z}. It offers an intuitive way to analyze  the behavior of graph signals in the vertex domain.
#'
#'@note If node coordinates \code{xy} are not provided, they will be calculated using spectral methods \code{\link{spectral_coords}}. For large graphs, this can be computationally intensive and may take significant time. Use with caution for large graphs if node coordinates are not supplied.
#'
#' @export plot_signal
#' @importFrom methods is
#' @importFrom Matrix summary
#' @param z A list containing graph data. This list must have the following components:
#'          \itemize{
#'            \item{sA}  An adjacency matrix or a sparse Matrix representation of the graph.
#'            \item{xy}  A matrix or dataframe containing the x and y coordinates of each node in the graph.
#'          }
#' @param f Signal to plot.
#' @param size Numeric. Dot size for nodes. Default is 0.75.
#' @param limits Set colormap limits.
#' @examples
#' f <- rnorm(length(grid1$xy[,1]))
#' plot_signal(grid1, f)
#' @seealso \code{\link{plot_graph}}, \code{\link{spectral_coords}}

plot_signal <- function(z, f, size=0.75, limits=range(f)) {
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
                    y1 = y1, y2 = y2)
  p2 <- ggplot(df1, aes(x, y)) +
    geom_segment(aes(x = x, y = y,
                     xend = y1, yend = y2),
                 color = "gray", data = df2) +
    geom_point(size = size, aes(colour = f)) +
    scale_colour_distiller(palette = "Spectral",
                           limits = limits) +
    theme_void() +
    theme(#legend.position = "bottom",
          legend.text=element_text(size=8),
          legend.key.size = unit(0.8,"line"),

          legend.margin = margin(0.0,0.0,0.0,0.0),
          plot.margin = unit(c(0.01,0.01,0.01,0.01), "cm"))
  print(p2)
}
