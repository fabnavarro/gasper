#' Convert symmetric sparse matrix to full matrix.
#'
#' Convert a symmetric sparse matrix sA stored as upper triangular matrix to full matrix A.
#'
#' @export fullup
#' @importFrom methods is
#' @param sA Sparse upper triangular matrix to convert.
#' @examples
#' data(grid1)
#' A <- fullup(grid1$sA)
#' @seealso \code{\link{full}}

fullup <- function(sA) {
  if(is(sA, 'sparseMatrix')){
    A <- as.matrix(sA)
  } else {
    A <- full(rbind(sA,cbind(sA[,2],sA[,1],sA[,3])))
  }
  return(A)
}
