#' Add recessions to time series as background rectangles
#' https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
#' @export
add_recessions <- function(min = 2000, max = 2010){
  geom_rect(data = filter(recessions, end_int > min & start_int < max), inherit.aes = FALSE, aes(xmin = start_int, xmax=end_int, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2)
}

# from https://joshuacook.netlify.com/post/integer-values-ggplot-axis/
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}



# time_series <- tibble(date = 1800:2020, var = rnorm(221))
#
# ggplot(data = filter(time_series, date >= 2000 & date <= 2010), aes(x = date, y = var)) +
#   geom_line() + add_r2(min = 1900) + scale_x_continuous("Year", breaks = integer_breaks(n = 5))




# resourceS:
# https://htmlpreview.github.io/?https://github.com/brodieG/ggbg/blob/development/inst/doc/extensions.html#stat-compute
# https://rpubs.com/hadley/97970
# https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

