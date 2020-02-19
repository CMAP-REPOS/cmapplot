#' Add recessions to time series graphs
#'
#' \code{add_recessions} returns ggplot geoms that add rectangles representing
#' recessions to a plot. It will either return a single \code{geom_rect} geom or
#' \code{geom_rect} and \code{geom_text} geoms, where the text places the word
#' "recession" near each rect.
#'
#' @usage add_recessions(min = 2000, max = 2010, xdata = c("int", "date"), text
#'   = TRUE, text_nudge_x = .2, fill = "#002d49", alpha = 0.11)
#'
#' @param min Numeric, the year of the earliest recession to display.
#' @param max Numeric, the year of the latest recession to display.
#' @param xdata Char, a string indicating whether the x axis of the primary data
#'   being graphed is in integer or date format. This argument will accept any
#'   of the following values: \code{c("int", "integer", "integers", "i", "date",
#'   "dates", "d")}
#' @param text Logical, whether or not to include labels that identify each box
#'   as a recession.
#' @param text_nudge_x Numeric, the amount to shift the labels along the x axis.
#'   Measured as a distance from the end of each recession (a value of 0 will
#'   place the label on the edge of each rectangle). The default of 0.2 seems to
#'   work well in many situations but may need to be overridden.
#' @param fill Char, the color of the rectangles to draw.
#' @param alpha Numeric, the opacity of the rectangles to draw.
#'
#' @section Default color: The CMAP color palette gray used for recessions is
#'   \code{#e3e8eb}. The default fill and alpha values of \code{#002d49} and
#'   \code{0.11} replicate the palette color at the highest possible
#'   transparency. This is done because there is no known way to place the
#'   recession geom behind the graph's grid lines. The default therefore
#'   produces the approved CMAP color while altering the appearance of any
#'   overlapping grid lines as little as possible. This was generated using the
#'   hints found here:
#'   \url{https://stackoverflow.com/questions/6672374/convert-rgb-to-rgba-over-white}.
#'
#' @examples
#' grp_goods <- dplyr::filter(grp_over_time, category == "Goods-Producing")
#' grp_goods <- dplyr::mutate(grp_goods, year2 = as.Date(lubridate::date_decimal(year)))
#'
#' # INTEGER X AXIS:
#' ggplot(data = grp_goods, mapping = aes(x = year, y = realgrp, color = cluster)) +
#'   add_recessions(min = 2007, max = 2019, xdata = "int", text = TRUE) +
#'   geom_line() +
#'   scale_x_continuous("Year") +
#'   theme_minimal()
#'
#' # DATE X AXIS:
#' ggplot(data = grp_goods,
#' mapping = aes(x = year2, y = realgrp, color = cluster)) +
#'   add_recessions(min = 2007, max = 2019, xdata = "date", text = TRUE, text_nudge_x = 100) +
#'   geom_line() +
#'   scale_x_date("Year") +
#'   theme_minimal()
#'
#' # BELOW EXAMPLES SHOW MORE THAN 1 RECESSION
#' \dontrun{
#' library(tidyverse)
#'
#' time_series <- tibble(date_dec = 1800:2020, var = rnorm(221), var2 = rnorm(221)) %>%
#' pivot_longer(cols = starts_with("var"),
#'              names_to = "var") %>%
#'   mutate(date_date = as.Date(lubridate::date_decimal(date_dec))) %>%
#'   select(date_dec, date_date, var, value)
#'
#' # A plot with an integer-based x axis
#' ggplot(data = filter(time_series, date_dec >= 1980 & date_dec <= 2019),
#'        mapping = aes(x = date_dec, y = value, color = var)) +
#'   add_recessions(min = 1980, max = 2019, xdata = "int", text = TRUE) +
#'   geom_line() +
#'   scale_x_continuous("Year") +
#'   theme_cmap()
#'
#' # A plot with a date-based x axis
#' ggplot(data = filter(time_series, date_dec >= 1980 & date_dec <= 2019),
#'        mapping = aes(x = date_date, y = value, color = var)) +
#'   add_recessions(min = 1980, max = 2019, xdata = "date", text = TRUE) +
#'   geom_line() +
#'   scale_x_date() +
#'   theme_cmap()
#' }
#'
#' @seealso
#'
#' \url{https://htmlpreview.github.io/?https://github.com/brodieG/ggbg/blob/development/inst/doc/extensions.html#stat-compute}
#' \url{https://rpubs.com/hadley/97970}
#' \url{https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html}
#'
#' @export
add_recessions <- function(min = 2000, max = 2010, xdata = c("int", "date"),
                           text = TRUE, text_nudge_x = 0.2, fill = "#002d49", alpha = 0.11) {

  # handle multiple constructions of `xdata`
  if (xdata %in% c("i", "int", "integer", "integers")) {
    xdata <- "int"
  } else if (xdata %in% c("d", "date", "dates")) {
    xdata <- "date"
  } else {
    stop("Incorrect `xdata` specified", call. = FALSE)
  }

  # construct recessions dataset for this geom:
  # - remove recessions outside of range
  recessions2 <- dplyr::filter(cmapplot::recessions, end_int > min & start_int < max)
  # - if `min` or `max` fall in  middle of a recession, modify recession to
  #   end at specified term. Then, rebuild dates from ints
  recessions2 <- dplyr::mutate(recessions2,
    start_int = if_else(start_int < min, min, start_int),
    end_int = if_else(end_int > max, max, end_int),
    start_date = as.Date(lubridate::date_decimal(start_int)),
    end_date = as.Date(lubridate::date_decimal(end_int))
  )
  # - select the best data type (int or date format)
  recessions2 <- dplyr::select(recessions2, ends_with(xdata))
  # - rename variables so they are easy to call below
  names(recessions2) <- c("start", "end")

  # build rectangles
  elements <- geom_rect(data = recessions2,
                        inherit.aes = FALSE,
                        mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = +Inf),
                        fill = fill, alpha = alpha)

  # if text annotations are called for:
  if (text) {
    # build annotations
    annotations <- geom_text(data = recessions2,
                             inherit.aes = FALSE,
                             mapping = aes(x = end, y = +Inf),
                             label = "    Recession    ", angle = 270,
                             hjust = "left", vjust = "bottom",
                             position = position_nudge(x = text_nudge_x, y = 0))

    # add annotations
    elements <- list(elements, annotations)
  }

  # return final elements
  return(elements)
}
