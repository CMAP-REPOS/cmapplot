#' clean up integer axis breaks
#'
#' Where n = desired number of ticks. Function uses \code{floor(pretty())} to
#' generate good breaks for the x or y axis of a ggplot. Borrowed with respect
#' from \url{https://joshuacook.netlify.com/post/integer-values-ggplot-axis/}
#'
#' @usage scale_x_continuous(breaks = integer_breaks(n = 5))
#'
#' @param n Numeric, desired number of breaks.
#' @param ... other arguments passed on to \code{\link[base]{pretty}}
#'
#' @examples
#'
#' ggplot(data = dplyr::filter(grp_over_time, category == "Goods-Producing"),
#' mapping = aes(x = year, y = realgrp, color = cluster)) +
#'   geom_line() +
#'   scale_x_continuous("Year", breaks = integer_breaks(n = 4)) +
#'   theme_minimal()
#'
#' @export
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}



