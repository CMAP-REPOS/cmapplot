#' A list of all recessions in American history
#'
#' An internal dataset containing a list of all recessions in American history, as recorded by
#' the National Bureau of Economic Research (NBER). As future recessions occur, this file will
#' need to be updated using the script below.
#'
#' @format A tibble. 33 rows and 4 variables:
#' \describe{
#'    \item{start_char, end_char}{Chr. Easily readable labels for the beginning and end of the recession}
#'    \item{start_int, end_int}{Double. Dates expressed as years, with decimels referring to months. (e.g. April = 4/12 = .333)}
#'    \item{start_date, end_date}{Date. Dates expressed in R datetime format, using the first day of the specified month.}
#' }
#'
#' @source from https://www.nber.org/cycles/NBER%20chronology.xlsx
#'
#' @examples
#' # Use this code to generate an updated recessions dataframe:
#'
#' \dontrun{
#' library(RCurl)
#' library(readxl)
#' library(tidyverse)
#' temp.file <- paste(tempfile(),".xlsx",sep = "")
#' download.file("https://www.nber.org/cycles/NBER%20chronology.xlsx", temp.file, mode = "wb")
#'
#' recessions <- read_excel(temp.file, skip = 2) %>%
#'   # drop end matter
#'   slice(1:(n()-7)) %>%
#'   # drop first row trough
#'   slice(-1) %>%
#'   as_tibble() %>%
#'   # rename character values
#'   rename(start_char = 1, end_char = 2) %>%
#'   mutate(
#'    # numeric values expressed as months since January 1800
#'     start_int = `Peak month number`/12 + 1800,
#'     end_int = `Trough month number`/12 + 1800,
#'     # convert to R date
#'     start_date =  as.Date(str_replace(start_char, " ", " 1, "), format = "%B %d, %Y"),
#'     end_date =  as.Date(str_replace(end_char, " ", " 1, "), format = "%B %d, %Y")) %>%
#'   select(-3:-8)s
#'}
#'
"recessions"


#' Job change in CMAP region by cluster, 2001-17
#'
#' A test dataset containing 2001-17 job change and other data for the 22 specialized traded clusters
#' analyzed in the CMAP Traded Clusters report.
#'
#' @format A tibble. 22 rows and 5 variables:
#' \describe{
#'    \item{code}{Integer. code of cluster}
#'    \item{name}{Char. textual description/cluster title}
#'    \item{category}{Factor. Either "goods-producing" or "services"}
#'    \item{assessment}{Factor. "Leading", "Mixed", or "Trailing"}
#'    \item{jobchange}{Integer. Total change in employment in the cluster between 2001-17}
#' }
#' @source CMAP traded clusters report
#'
#' @examples
#' # A bar chart
#' ggplot(cluster_jobchange, aes(x = reorder(name, jobchange), y = jobchange, fill = category)) +
#'   geom_col() +
#'   coord_flip()
#'
#'
"cluster_jobchange"


#' Basic regional economic stats in 2001 and 2017
#'
#' A test dataset containing count of jobs, earnings, and establishments in the Chicago region in both 2001 and 2017.
#'
#' @format A tibble. 18 rows and 4 variables:
#' \describe{
#'    \item{variable}{Chr. Indicates the meaning of the data stored in `value`. Jobs, Real Earnings, or Establishments.}
#'    \item{year}{Factor. 2001 or 2017.}
#'    \item{sector}{Chr. local, tradedgoods, or tradedservices. Together, these three sectors account for all clusters in the region.}
#'    \item{value}{Int. The value indicated as described by the other columns}
#' }
#' @source CMAP traded clusters report
#'
#' @examples
#' # a grouped and stacked bar chart (via `interaction()`)
#' ggplot(economy_basic, aes(x = interaction(year, variable), y = value, fill = sector)) +
#'   geom_col(position = "fill") +
#'   scale_y_continuous(labels = scales::percent)
#'
"economy_basic"


#' Gross Regional Product by cluster, 2007-17
#'
#' A test dataset containing real GRP data for the CMAP region.
#'
#' @format A tibble. 121 rows and 5 variables:
#' \describe{
#'    \item{cluster}{Chr. The name of the cluster}
#'    \item{category}{Factor. "goods-producing" or "services"}
#'    \item{assessment}{Factor. "Trailing", "Mixed", or "Leading"}
#'    \item{year}{Double. The year of the data}
#'    \item{realgrp}{Double. The real gross regional product of the cluster in year `year`.
#'    Not exactly sure on the inflation year but I believe it is 2012}
#' }
#' @source CMAP traded clusters report
#'
#' @examples
#' # a time-series line chart
#' ggplot(grp_over_time, aes(x = year, y = realgrp, color = cluster)) +
#'   geom_line()
#'
"grp_over_time"


#' Wage percentiles by cluster
#'
#' A test dataset containing the 10th, 25th, 50th, 75th, and 90th percentile wage by cluster in the CMAP region.
#'
#' @format A tibble. 45 rows and 3 variables:
#' \describe{
#'    \item{cluster}{Chr. The name of the cluster}
#'    \item{percentile}{Double. The percentile wage being reported}
#'    \item{wage}{Double. The wage. I believe 2017 data.}
#' }
#' @source CMAP traded clusters report
#'
#' @examples
#' # a non-time-series line chart
#' ggplot(percentile_wages, aes(x = percentile, y = wage, color = cluster)) +
#'   geom_line()
#'
"percentile_wages"


#' Population and Labor Force by Age
#'
#' A test dataset containing percentage breakdowns of the population and labor force by various age buckets in 2010 and 2017.
#'
#' @format A tibble. 12 rows and 4 variables:
#' \describe{
#'    \item{variable}{Chr. Indicates the meaning of the data stored in `value`. "population" or "laborforce".}
#'    \item{year}{Factor. 2010 or 2017.}
#'    \item{age}{Chr. The age bucket. Either 16-24, 25-54, or 55+.}
#'    \item{value}{Double. The value indicated by the other variables.}
#' }
#' @source CMAP traded clusters report
#'
#' @examples
#' # a grouped and stacked bar chart (via `interaction()`)
#' ggplot(pop_and_laborforce_by_age, aes(x = interaction(year, variable), y = value, fill = age)) +
#'   geom_col(position = position_stack(reverse = TRUE))
#'
"pop_and_laborforce_by_age"


#' Traded employment by race
#'
#' A test dataset containing the percentage breakdowns of the working population employed in traded clusters, by race.
#'
#'
#' @format A tibble. 12 rows and 4 variables:
#' \describe{
#'    \item{Race}{Chr. White, Asian, Hispanic, Other, Black, or Regional Average.}
#'    \item{variable}{Chr. SpecializedTraded, UnspecializedTraded, or Total.
#'    Total is a sum of SpecializedTraded and UnspecializedTraded. The invisible remainder (e.g. `1-Total` or
#'    `1-(SpecializedTraded+UnspecializedTraded)`) is the percentage employed in local clusters.}
#'    \item{value}{Double. The value indicated by the other variables.}
#' }
#' @source CMAP traded clusters report
#'
#' @examples
#' # a stacked bar chart
#' \dontshow{library(dplyr)}
#' df <- dplyr::filter(
#'   traded_emp_by_race,
#'   variable %in% c("SpecializedTraded", "UnspecializedTraded")
#' )
#' ggplot(df, aes(x = reorder(Race, -value), y = value, fill = variable)) +
#'   geom_col(position = position_stack(reverse = TRUE)) +
#'   scale_y_continuous(labels = scales::percent)
#'
"traded_emp_by_race"
