#' Tight frame computation.
#'
#' Construct tight (Parseval) frame.
#'
#' @export tight_frame
#' @param evalues Eigenvalues of the Laplacian matrix.
#' @param evectors Eigenvectors of the Laplacian matrix.
#' @param b Parameter that control the number of scales.

tight_frame <- function(evalues, evectors, b = 2) {
  lmax <- max(evalues)
  kmax <- floor(log(lmax)/log(b)) + 2
  N <- length(evalues)

  r <- array(0, c(N, N, kmax + 1))
  for (k in 0:kmax) {
    G <- diag(sqrt(zetav(evalues, k, b)))
    A <- evectors %*% G
    #A <- matmult(evectors, G)
    W <- A %*% t(evectors)
    #W <- matmult(A, t(evectors))
    W <- t(W)
    r[, , k + 1] <- W
  }
  dim(r) <- c(N, N * (kmax + 1))
  r <- t(r)
  return(r)
}

zetav <- function(x, k, b) {
  if (k == 0) {
    return(gv(x, b))
  } else {
    return(gv(b^(-k) * x, b) - gv(b^(-k + 1) * x, b))
  }
}

gv <- function(x, b) {
  low <- outer(x, 0, "<")
  mid1 <- outer(x, 0, ">=")
  mid2 <- outer(x, 1/b, "<=")
  mid3 <- outer(x, 1/b, ">=")
  mid4 <- outer(x, 1, "<=")
  up <- outer(x, 1, ">")

  # gg <- rep(NA, length(x))
  gg <- rep(0, length(x))
  gg[low] <- 1
  gg[mid1 & mid2] <- 1
  gg[mid3 & mid4] <- b * x[mid3 & mid4]/(1 - b) + b/(b - 1)
  gg[up] <- 0
  return(gg)
}
