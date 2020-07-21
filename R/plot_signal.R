#' Plot a signal on top of a given graph
#'
#' @export plot_signal
#' @param z Graph data.
#' @param f Signal to plot.
#' @param size Dot size.
#' @param limits Set colormap limits.
#' @examples
#' f <- rnorm(2642)
#' plot_signal(minnesota, f)

plot_signal <- function(z, f, size=0.75, limits=range(f)) {
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
