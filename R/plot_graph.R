#' Graph plot
#'
#' @export plot_graph
#' @param z Graph data.
#' @param size Dot size.
#' @examples
#' plot_graph(minnesota)

plot_graph <- function(z, size=0.75) {
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
