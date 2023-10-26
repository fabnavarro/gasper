#' Convert Symmetric Sparse Matrix to Full Matrix
#'
#' \code{fullup} converts a symmetric sparse matrix \code{sA}, stored as an upper triangular matrix, to a full matrix \code{A}.
#'
#' @export fullup
#' @importFrom methods is
#' @param sA Matrix (sparseMatrix). Symmetric upper triangular matrix to be converted.
#' @return \code{A} Full symmetric matrix.
#'
#' @details This function can be used for transforming matrices that have been stored in a memory-efficient format (i.e., the upper triangle portion of a symmetric matrix) to their full format. The conversion is done either by directly transforming the sparse matrix or by leveraging the \code{\link{full}} function.
#'
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
