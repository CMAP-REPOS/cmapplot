#' Job change in CMAP region by cluster, 2001-17
#'
#' A test dataset containing 2001-17 job change and other data for the 22 specialized traded clusters analyzed in the CMAP Traded Clusters report.
#'
#' @format A data frame with 22 rows and 5 variables:
#' \describe{
#'   \item{code}{Integer. code of cluster}
#'   \item{name}{Char. textual description/cluster title}
#'   \item{category}{Factor. Either "goods-producing" or "services"}
#'   \item{assessment}{Factor. "Leading", "Mixed", or "Trailing"}
#'   \item{jobchange}{Integer. Total change in employment in the cluster between 2001-17}
#' }
#' @source CMAP traded clusters report
#'
"cluster_jobchange"
