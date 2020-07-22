#' Download sparse matrix form the SuiteSparse Matrix Collection.
#'
#' If coordinates are associated with the graphs,
#' they are automatically downloaded and added to the output. See \url{https://sparse.tamu.edu/} for the list of groups and graph names.
#'
#' @export download_graph
#' @import utils
#' @param graphname Name of the graph to download.
#' @param groupname Name of the group that provides the graph.
#' @return \code{graphname} a list of dataframe contening W and xy coordinates.
#' @examples
#' graphname <- "grid1"
#' groupname <- "AG-Monien"
#' download_graph(graphname,groupname)
#' plot_graph(grid1)

download_graph <- function(graphname, groupname) {
    url <- paste("https://sparse.tamu.edu/MM/",
                 groupname,"/",
                 graphname,".tar.gz",sep = "")

    temp <- tempfile()
    tempd <- tempdir()
    download.file(url,temp)
    untar(temp, exdir=tempd)

    if (Sys.info()['sysname']=="Windows"){
      tempp <- paste(tempd,
                     paste(graphname,"\\",sep=""),
                     sep ="\\")
    } else{
      tempp <- file.path(tempd,
                         paste(graphname,"/",sep=""))
    }

    temppath <- paste(tempp,
                      graphname,".mtx",sep="")

    tmp <- readLines(temppath)
    nskip <- length(grep("%",tmp))+1
    df <- read.table(temppath,
                     comment.char = "%",
                     skip = nskip)
    if (ncol(df)==2){
      df$V3 <- rep(1, nrow(df))
    }

    if (length(list.files(tempp)!=1)) {
      temppathc <- paste(tempp,
                         graphname,"_coord.mtx",sep="")

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
      return(assign(graphname,list("sA"=df,"xy"=dfc),
                    envir = parent.frame()))
    }
    else {
      return(assign(graphname,list("sA"=df),
                    envir = parent.frame()))
    }

}

