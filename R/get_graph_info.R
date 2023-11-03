#' Retrieve Information Tables about a Specific Graph from the SuiteSparse Matrix Collection
#'
#' \code{get_graph_info} fetches the overview tables about a specified graph/matrix from the SuiteSparse Matrix Collection.
#'
#' @export get_graph_info
#' @importFrom httr GET timeout http_error message_for_status
#' @importFrom curl has_internet
#' @param matrixname Name of the matrix/graph for which to fetch information.
#' @param groupname Name of the group that provides the matrix/graph.
#' @return A list of tables with detailed information about the specified matrix/graph:
#' \itemize{
#'   \item "Matrix Information"
#'   \item "Matrix Properties"
#'   \item "SVD Statistics" (if available)
#' }
#'
#' @details The tables contain detailed information and properties about the graph/matrix, such as its size, number of non-zero elements, etc. Visit \url{https://sparse.tamu.edu/} of see \code{\link{SuiteSparseData}} to explore groups and matrix names.
#'
#' @references
#' Davis, T. A., & Hu, Y. (2011). The University of Florida sparse matrix collection. ACM Transactions on Mathematical Software (TOMS), 38(1), 1-25.
#'
#' Kolodziej, S. P., Aznaveh, M., Bullock, M., David, J., Davis, T. A., Henderson, M.,
#' Hu, Y., & Sandstrom, R. (2019). The suitesparse matrix collection website interface. Journal of Open Source Software, 4(35), 1244.
#'
#' @note
#' The \code{rvest} package is used for parsing HTML, if it is not installed, the function will prompt for installation.
#'
#' @examples
#' \dontrun{
#' matrixname <- "grid1"
#' groupname <- "AG-Monien"
#' info_tables <- get_graph_info(matrixname,groupname)
#'
#' # Matrix Information
#' info_tables[[1]]
#'
#' # Matrix Properties
#' info_tables[[2]]
#'
#' # SVD Statistics
#' info_tables[[3]]
#' }
#' #' @seealso \code{\link{download_graph}}, \code{\link{SuiteSparseData}}

#- todo change download graph dependcy to rvest
#- todo maybe check other possible conflict with different graphs
#- @importFrom rvest read_html html_table

get_graph_info <- function(matrixname, groupname) {
  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop("The 'rvest' package is required to use this function.
          Please install it with install.packages('rvest').",
         call. = FALSE)
  }
  url <- paste0("https://sparse.tamu.edu/",
                groupname, "/", matrixname)
  gracefully_fail(url)
  page_content <- rvest::read_html(url)
  tables <- rvest::html_table(page_content)
  if (length(tables) == 3) {
    tables <- tables[-3]
  }
  if (length(tables) == 4) {
    tables <- tables[-4]
    tables[[3]] <- tables[[3]][-nrow(tables[[3]]), ]
  }
  tables[[2]] <- tables[[2]][,-3]
  tables <- lapply(tables, function(t) {
    df <- as.data.frame(t)
    rownames(df) <- df[[1]]
    df <- df[-1]
    return(df)
  })
  names(tables[[1]])="MatrixInformation"
  names(tables[[2]])="MatrixProperties"

  table_names <- c("MatrixInfo", "MatrixProp")
  if (length(tables) == 3) {
    table_names <- c(table_names, "SVDStats")
    names(tables[[2]])="SVDStatistics"
  }
  names(tables) <- table_names
  return(tables)
}

gracefully_fail <- function(remote_file) {
  # Fail gracefully if API or internet not available
  # Based on code:
  # https://github.com/lgnbhl/wikisourcer/blob/master/R/utils.R
  # See full discussion to be compliant with the CRAN policy
  # https://community.rstudio.com/t/internet-resources-should-fail-gracefully/49199
  try_GET <- function(x, ...) {
    tryCatch(
      GET(url = x, timeout(60), ...),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }
  is_response <- function(x) {
    class(x) == "response"
  }
  # First check internet connection
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }
  # Then try for timeout problems
  resp <- try_GET(remote_file)
  if (!is_response(resp)) {
    message(resp)
    return(invisible(NULL))
  }
  # Then stop if status > 400
  if (httr::http_error(resp)) {
    message_for_status(resp)
    return(invisible(NULL))
  }
}


