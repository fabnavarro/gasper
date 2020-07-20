#' Pittsburgh network.
#'
#' A dataset containing the pittsubrgh network.
#'
#' @format list of 7 elements
#' \describe{
#' \item{A}{pittsburgh adjacency matrix}
#' \item{sA}{pittsburgh sparse adjacency matrix}
#' \item{xy}{coordinates}
#' \item{f}{signal used in Trend filtering on graphs}
#' \item{y}{noisy signal used in Trend filtering on graph}
#' \item{f1}{eta=0.01, k=5}
#' \item{geo}{geometry}
#' }
#' @source The sources come from different codes provided by Yu-Xiang Wang (UC Santa Barbara) and are associated with the article: "Trend Filtering on Graphs, JMLR, 2016". \url{https://sites.cs.ucsb.edu/~yuxiangw/resources.html}.
"pittsburgh"
