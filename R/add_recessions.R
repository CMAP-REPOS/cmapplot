#' Add recessions to time series as background rectangles
#' https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
#' Note: color approximates #e3e8eb. hex code and alpha calculated with help from
#' https://stackoverflow.com/questions/6672374/convert-rgb-to-rgba-over-white
#'
#' @export
add_recessions <- function(min = 2000, max = 2010, fill = "#002d49", alpha = 0.11, text = TRUE, text_nudge_x = .2){
  # build rectangles
  elements <- geom_rect(data = filter(recessions, end_int > min & start_int < max),
                          inherit.aes = FALSE,
                          mapping = aes(xmin = start_int, xmax=end_int, ymin=-Inf, ymax=+Inf),
                          fill = fill, alpha = alpha)

  # build annotations
  annotations <- geom_text(filter(recessions, end_int > min & start_int < max),
                      inherit.aes = FALSE,
                      mapping = aes(x = end_int, y = +Inf, label = "Recession   ", angle = 90, hjust = "right"),
                      nudge_x = text_nudge_x)

  # add annotations if called for
  if(text){
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




# time_series <- tibble(date = 1800:2020, var = rnorm(221))
 ggplot(data = filter(time_series, date >= 2000 & date <= 2010), aes(x = date, y = var)) +
   add_recessions(min = 2000, text = TRUE) + geom_line() + scale_x_continuous("Year", breaks = integer_breaks(n = 5)) + theme_cmap()




# resourceS:
# https://htmlpreview.github.io/?https://github.com/brodieG/ggbg/blob/development/inst/doc/extensions.html#stat-compute
# https://rpubs.com/hadley/97970
# https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

