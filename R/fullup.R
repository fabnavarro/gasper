#' Convert symmetric sparse matrix to full matrix.
#'
#' Convert a symmetric sparse matrix sA stored as upper triangular matrix to full matrix A.
#'
#' @export fullup
#' @importFrom methods is
#' @param sA Sparse upper triangular matrix to convert.
#' @examples
#' graphname <- "grid1"
#' groupname <- "AG-Monien"
#' download_graph(graphname,groupname)
#' A <- fullup(grid1$sA)

fullup <- function(sA) {
  if(is(sA, 'sparseMatrix')){
    A <- as.matrix(sA)
  } else {
    A <- full(rbind(sA,cbind(sA[,2],sA[,1],sA[,3])))
  }
  return(A)
}
