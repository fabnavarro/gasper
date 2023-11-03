#' Matrix Data from SuiteSparse Matrix Collection
#'
#' This dataset represents the collection of matrices from the SuiteSparse Matrix Collection. The structure of the dataframe mirrors the structure presented on the SuiteSparse Matrix Collection website.
#'
#' @format A data frame with 2893 rows and 8 columns:
#' \describe{
#'   \item{\code{ID}}{Integer. Unique identifier for each matrix.}
#'   \item{\code{Name}}{Character. Name of the matrix.}
#'   \item{\code{Group}}{Character. Group name the matrix belongs to.}
#'   \item{\code{Rows}}{Integer. Number of rows in the matrix.}
#'   \item{\code{Cols}}{Integer. Number of columns in the matrix.}
#'   \item{\code{Nonzeros}}{Integer. Number of non-zero elements in the matrix.}
#'   \item{\code{Kind}}{Character. Kind or category of the matrix.}
#'   \item{\code{Date}}{Character. Date when the matrix was added or updated.}
#' }
#' @source SuiteSparse Matrix Collection website: \url{https://sparse.tamu.edu/}
#'
#' @note Data download date: 2023-11-01
#' Note that the number of matrices in the SuiteSparse Matrix Collection may have increased since this download date.
#'
#' @details
#' The SuiteSparse Matrix Collection is a large set of sparse matrices that arise in real applications. It is widely used by the numerical linear algebra community for the development and performance evaluation of sparse matrix algorithms. The Collection covers a wide spectrum of domains, including both geometric and non-geometric domains.
#'
#' @references
#' Davis, T. A., & Hu, Y. (2011). The University of Florida sparse matrix collection. ACM Transactions on Mathematical Software (TOMS), 38(1), 1-25.
#'
#' Kolodziej, S. P., Aznaveh, M., Bullock, M., David, J., Davis, T. A., Henderson, M., Hu, Y., & Sandstrom, R. (2019). The suitesparse matrix collection website interface. Journal of Open Source Software, 4(35), 1244.
"SuiteSparseData"
