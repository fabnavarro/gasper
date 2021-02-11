#' Download sparse matrix form the SuiteSparse Matrix Collection.
#'
#' If coordinates are associated with the graphs,
#' they are automatically downloaded and added to the output. See \url{https://sparse.tamu.edu/} for the list of groups and graph names.
#'
#' @export download_graph
#' @importFrom utils download.file untar read.table
#' @importFrom Matrix readMM
#' @param matrixname Name of the graph to download.
#' @param groupname Name of the group that provides the graph.
#' @return \code{matrixname} a list contening the sparse matrix \code{sA}, \code{xy} coordinates (if any), \code{dim} the number of rows, columns and numerically nonzero elements  and \code{info}, the path to a plain txt file containing information associated with \code{sA} (accessible for example via \code{file.show(matrixname$info)}).
#' @references
#' Davis, T. A., & Hu, Y. (2011). The University of Florida sparse matrix collection. ACM Transactions on Mathematical Software (TOMS), 38(1), 1-25.
#' @examples
#' matrixname <- "grid1"
#' groupname <- "AG-Monien"
#' download_graph(matrixname,groupname)
#' file.show(grid1$info)

download_graph <- function(matrixname, groupname) {
    url <- paste("https://sparse.tamu.edu/MM/",
                 groupname,"/",
                 matrixname,".tar.gz",sep = "")

    temp <- tempfile()
    tempd <- tempdir()
    download.file(url,temp)
    untar(temp, exdir=tempd)

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
    m <- as(m, "CsparseMatrix")
    if(nrow(m)==ncol(m)){
      m <- as(m, "dsCMatrix")
    } else{
      m <- as(m, "dgCMatrix")
    }

    # if (ncol(df)==2){
    #   df$V3 <- rep(1, nrow(df))
    # }

    #if (length(list.files(tempp))!=1) {
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
      return(assign(matrixname,
                    list("sA"=m,
                         "xy"=dfc,
                         "dim"=graphdim,
                         "info"=graphdesc),
                    envir = parent.frame()))
    }
    else {
      writeLines(tmp[1:(nskip)],
                 graphdesc)
      return(assign(matrixname,
                    list("sA"=m,
                         "dim"=graphdim,
                         "info"=graphdesc),
                    envir = parent.frame()))
    }

}

