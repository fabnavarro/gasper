#' Conversion of Symmetric Sparse Matrix to Full Matrix
#'
#' \code{full} converts a symmetric sparse matrix, represented as \code{sA}, into a full matrix \code{A}.
#'
#' @export full
#' @importFrom methods is
#' @param sA Symmetric sparse matrix, either in a sparse matrix format or in a three-column format, that needs to be converted into a full matrix.
#' @return \code{A} Full matrix constructed from the symmetric sparse matrix \code{sA}.
#' @seealso \code{\link{fullup}}
#' @examples
#' sA <- pittsburgh$sA
#' A <- full(sA)

full <- function(sA) {
  if(is(sA, 'sparseMatrix')){
    A <- as.matrix(sA)
  } else {
    if (sA[1,1]!=1){
      ndim <- length(unique(sA[,2]))
    }
    else
    {
      ndim <- length(unique(sA[,1]))
    }
    A <- matrix(0, ncol = ndim, nrow = ndim)
    A[cbind(sA[,1],sA[,2])] <- sA[,3]
  }
  return(A)
}
