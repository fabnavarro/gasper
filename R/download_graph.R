#' Download Sparse Matrix form the SuiteSparse Matrix Collection
#'
#' \code{download_graph} allows to download sparse matrices from the SuiteSparse Matrix Collection.
#'
#' @export download_graph
#' @importFrom httr GET timeout http_error message_for_status
#' @importFrom curl has_internet
#' @importFrom utils download.file untar read.table
#' @importFrom Matrix readMM
#' @param matrixname Name of the graph to download.
#' @param groupname Name of the group that provides the graph.
#' @param svd Logical, if \code{TRUE}, a ".mat" file containing the singular values of the matrix is downloaded (if available). Default is \code{FALSE}.
#' @param add_info Logical, if \code{TRUE}, additional information about the graph will be fetched and included in the output. Default is \code{FALSE}.
#' @return A list containing several components:
#'         \itemize{
#'           \item \code{sA}: A sparse matrix representation of the downloaded graph.
#'           \item \code{xy}: Coordinates associated with the graph nodes (if available).
#'           \item \code{dim}: A data frame with the number of rows, columns, and numerically nonzero elements.
#'           \item \code{temp}: The path to the temporary directory where the matrix and downloaded files (including singular values if requested) are stored.
#'           \item \code{info}: Additional information about the graph (included when \code{add_info} is \code{TRUE}).
#' }
#'
#' @details
#' \code{download_graph} automatically converts the downloaded matrix into a sparse matrix format. If coordinates are associated with the graphs, they are downloaded and included in the output. Visit \url{https://sparse.tamu.edu/} or see \code{\link{SuiteSparseData}} to explore groups and matrix names.
#'
#' @note This temporary directory can be accessed, for example, via \code{list.files(grid1$temp)}. To open the read .mat files (containing singular values),  "R.matlab" or "foreign" packages can be used. After using the downloaded data, you can delete the content of the temporary folder.
#'
#' When \code{add_info} is set to \code{TRUE}, the function retrieves comprehensive information about the graph using \code{\link{get_graph_info}}.
#'
#' @references
#' Davis, T. A., & Hu, Y. (2011). The University of Florida sparse matrix collection. ACM Transactions on Mathematical Software (TOMS), 38(1), 1-25.
#'
#' Kolodziej, S. P., Aznaveh, M., Bullock, M., David, J., Davis, T. A., Henderson, M., Hu, Y., & Sandstrom, R. (2019). The suitesparse matrix collection website interface. Journal of Open Source Software, 4(35), 1244.
#'
#' @examples
#' \dontrun{
#' matrixname <- "grid1"
#' groupname <- "AG-Monien"
#' download_graph(matrixname,groupname)
#' list.files(grid1$temp)
#' }
#' @seealso \code{\link{get_graph_info}}, \code{\link{SuiteSparseData}}

download_graph <- function(matrixname, groupname, svd = FALSE, add_info = FALSE) {
    url <- paste("https://sparse.tamu.edu/MM/",
                 groupname,"/",
                 matrixname,".tar.gz",sep = "")
    temp <- tempfile()
    tempd <- tempdir()
    gracefully_fail(url)

    download.file(url, temp)
    untar(temp, exdir=tempd)
    if (svd) {
      url_svd <- paste0("https://suitesparse-collection-website.herokuapp.com/svd/",
                        groupname, "/", matrixname, "_SVD.mat")
      gracefully_fail(url_svd)
      svdpath <- file.path(tempd,
                           matrixname,
                           paste0(matrixname, "_SVD.mat"))
      download.file(url_svd, svdpath)
    }
    if (Sys.info()['sysname']=="Windows"){
      tempp <- paste(tempd,
                     paste(matrixname,"\\",sep=""),
                     sep ="\\")
    } else{
      tempp <- file.path(tempd,
                         paste(matrixname,"/",sep=""))
    }
    temppath <- paste(tempp,
                      matrixname,".mtx",sep="")

    tmp <- readLines(temppath)
    nskip <- length(grep("%",tmp))+1

    #store graph descrition in tmp folder
    graphdesc <- paste(tempp,
                        matrixname,sep="")

    # df <- read.table(temppath,
    #                  comment.char = "%",
    #                  skip = nskip)
    gdim <- scan(temppath,
                 skip = nskip-1,
                 nmax = 3,
                 quiet = TRUE)
    NumRows <- gdim[1]
    NumCols <- gdim[2]
    NonZeros <- gdim[3]
    graphdim <- data.frame(NumRows,
                           NumCols,
                           NonZeros)

    m <- readMM(temppath)
    m <- as(m, "dMatrix")
    if(nrow(m)==ncol(m)){
      m <- as(m, "CsparseMatrix")
    } else{
      m <- as(m, "CsparseMatrix")
    }

    # if (ncol(df)==2){
    #   df$V3 <- rep(1, nrow(df))
    # }

    #if (length(list.files(tempp))!=1) {

    # add graph_info
    if (add_info) {
      info <- get_graph_info(matrixname, groupname)
    }
    if (length(list.files(path=tempp,
                          pattern = "_coord.mtx"))==1) {
      writeLines(tmp[1:(nskip)],
                 graphdesc)
      temppathc <- paste(tempp,
                         matrixname,"_coord.mtx",sep="")

      tmpc <- readLines(temppathc)
      nskipc <- length(grep("%",tmpc))+1
      dfc <- read.table(temppathc,
                        comment.char = "%",
                        skip = nskipc)
      if (ncol(dfc)==1){
        dfc <- as.matrix(dfc)
        dim(dfc) <- c(nrow(dfc)/2,2)
        colnames(dfc) <- c("x", "y")
      }
      else
      {
        colnames(dfc) <- c("x", "y")
      }
      # return(assign(matrixname,
      #               list("sA"=m,
      #                    "xy"=dfc,
      #                    "dim"=graphdim,
      #                    "temp"=tempp),
      #               envir = parent.frame()))
      result <- list("sA"=m, "xy"=dfc, "dim"=graphdim, "temp"=tempp)
      if (add_info) {
        result$info <- info
      }
      return(assign(matrixname, result, envir = parent.frame()))
    }
    else {
      writeLines(tmp[1:(nskip)],
                 graphdesc)
      # return(assign(matrixname,
      #               list("sA"=m,
      #                    "dim"=graphdim,
      #                    "temp"=tempp),
      #               envir = parent.frame()))
      result <- list("sA"=m, "dim"=graphdim, "temp"=tempp)
      if (add_info) {
        result$info <- info
      }
      return(assign(matrixname, result, envir = parent.frame()))
    }
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

