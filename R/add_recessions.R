#' Add recessions to time series as background rectangles
#' https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
#' @export
add_recessions <- function(data2 = recessions,
                           inherit.aes = FALSE){
  geom_rect(data = data2, aes(xmin = start_int, xmax=end_int, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2, inherit.aes = inherit.aes)

}
debug("add_recessions")

#' @export
recessions_base <- ggproto(
  "recessions_base", Stat,
  compute_group =
  required_aes = c("x", "y")


)


ggplot(data = grp_over_time, aes(x = year, y = realgrp, color = cluster)) +
  scale_y_continuous("Gross regional product (indexed to 2007)", labels=scales::percent) +
  scale_x_continuous("Year") +
  geom_line() + theme_cmap() + add_recessions()

ggplot(data = grp_over_time, aes(x = year, y = realgrp, color = cluster)) +
  scale_y_continuous("Gross regional product (indexed to 2007)", labels=scales::percent) +
  scale_x_continuous("Year") +
  geom_line() + theme_cmap() +nberShade()

nberShade(ggp)

ggplot(data = grp_over_time) +
  scale_y_continuous("Gross regional product (indexed to 2007)", labels=scales::percent) +
  scale_x_continuous("Year") +
  geom_line(aes(x = year, y = realgrp, color = cluster)) + theme_cmap() +
  geom_rect(data = filter(recessions, start_int > min(grp_over_time$year) & end_int < max(grp_over_time$year)), aes(xmin = start_int, xmax=end_int, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2)



ggplot(data = grp_over_time) +
  scale_y_continuous("Gross regional product (indexed to 2007)", labels=scales::percent) +
  scale_x_continuous("Year") +
  geom_line(aes(x = year, y = realgrp, color = cluster)) + theme_cmap() +
  geom_rect(data = recessions, aes(xmin = start_int, xmax=end_int, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2)


time_series <- tibble(date = 1800:2020, var = rnorm(221))




# from https://joshuacook.netlify.com/post/integer-values-ggplot-axis/
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

add_r2 <- function(min = 2000, max = 2010){
  geom_rect(data = filter(recessions, end_int > min & start_int < max), inherit.aes = FALSE, aes(xmin = start_int, xmax=end_int, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2)
}

ggplot(data = filter(time_series, date >= 2000 & date <= 2010), aes(x = date, y = var)) +
  geom_line() + add_r2() + scale_x_continuous("Year", breaks = integer_breaks(n = 5)) + geom_debug()



View(recessions)

# resourceS:
# https://htmlpreview.github.io/?https://github.com/brodieG/ggbg/blob/development/inst/doc/extensions.html#stat-compute
# https://rpubs.com/hadley/97970
# https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

