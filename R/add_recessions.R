#' Add recessions to time series as background rectangles
#' https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
#' Note: color approximates #e3e8eb. hex code and alpha calculated with help from
#' https://stackoverflow.com/questions/6672374/convert-rgb-to-rgba-over-white
#'
#' @export
add_recessions <- function(min = 2000, max = 2010, xdata = c("int", "date"), fill = "#002d49", alpha = 0.11, text = TRUE, text_nudge_x = .2){

  # handle multiple constructions of `xdata`
  if (xdata %in% c("i", "int", "integer", "integers")){
    xdata <- "int"
  } else if (xdata %in% c("d", "date", "dates")){
    xdata <- "date"
  } else{
    stop("Incorrect `xdata` specified", call. = FALSE)
  }

  # construct recessions dataset for this geom
  recessions2 <- recessions %>%
    # remove recessions outside of range
    filter(end_int > min & start_int < max) %>%
    # if `min` or `max` fall in  middle of a recession, modify recession to end at specified term. Then, rebuild dates from ints
    mutate(start_int = if_else(start_int < min, min, start_int),
           end_int = if_else(end_int > max, max, end_int),
           start_date = as.Date(lubridate::date_decimal(start_int)),
           end_date = as.Date(lubridate::date_decimal(end_int))) %>%
    # select the best data type (int or date format)
    select(ends_with(xdata)) %>%
    # rename variables so they are easy to call below
    set_names(c("start", "end"))

  # build rectangles
  elements <- geom_rect(data = recessions2,
                        inherit.aes = FALSE,
                        mapping = aes(xmin = start, xmax=end, ymin=-Inf, ymax=+Inf),
                        fill = fill, alpha = alpha)

  # if text annotations are called for:
  if(text){
    # build annotations
    annotations <- geom_text(data = recessions2,
                             inherit.aes = FALSE,
                             mapping = aes(x = end, y = +Inf, label = "Recession   ", angle = 90, hjust = "right"),
                             nudge_x = text_nudge_x)

    # add annotations
    elements <- list(elements, annotations)
  }

  # return final elements
  return(elements)
}



#' clean up axis breaks where n = desired number of ticks.
#' from https://joshuacook.netlify.com/post/integer-values-ggplot-axis/
#' @export
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}




# time_series <- tibble(date = 1800:2020, var = rnorm(221), var2 = rnorm(221)) %>%
#   pivot_longer(cols = starts_with("var"),
#                names_to = "var")
#
# time_series2 <- tibble(date = as.Date(lubridate::date_decimal(time_series$date)),
#                         var = time_series$var)
#
# ggplot(data =
#          filter(time_series, date >= 1931 & date <= 1951),
#          #filter(time_series2, date >= lubridate::ymd("1931-01-01") & date <= lubridate::ymd("1951-01-01")),
#        aes(x = date, y = value, color = var)) +
#   add_recessions(min = 1931, max = 1951, xdata = "int", text = TRUE) +
#   geom_line() +
#   #scale_x_date() +
#   scale_x_continuous("Year", breaks = integer_breaks(n = 6)) +
#   theme_cmap()




# resourceS:
# https://htmlpreview.github.io/?https://github.com/brodieG/ggbg/blob/development/inst/doc/extensions.html#stat-compute
# https://rpubs.com/hadley/97970
# https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

