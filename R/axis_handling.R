#'Axis handling helper functions
#'
#'This file includes two helper functions that improve axis handling.
#'
#'@importFrom stringr str_length


#'A function factory for getting integer x- and y-axis values.
#'
#'This function can be supplied as the value for the \code{breaks} argument in
#'the \code{scale_*_continuous} ggplot elements. It forces labels to be
#'displayed as integers with even spacing between them, which is particularly
#'important for time series with years on the X or Y axis. The code was
#'developed by Joshua Cook.
#'
#'@source \url{https://joshuacook.netlify.app/post/integer-values-ggplot-axis/}
#'
#'@param n Integer, the desired number of breaks on the axis.
#'@param ... Pass additional arguments to the base \code{pretty()} function.
#'
#' @examples
#' # Standard implementation
#' ggplot(grp_over_time, aes(x = year, y = realgrp, color = cluster)) +
#'   geom_line() +
#'   scale_x_continuous(breaks = integer_breaks())
#'
#' # Adjusted to add a total of 11 intervals
#' ggplot(grp_over_time, aes(x = year, y = realgrp, color = cluster)) +
#'   geom_line() +
#'   scale_x_continuous(breaks = integer_breaks(n = 11))
#'
#'@export
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

#'A function for abbreviating year labels in time series graphs
#'
#'This function can be supplied as the value for the \code{labels} argument in
#'\code{scale_*_continuous} and
#'\code{scale_*_date}. It will return a set of labels that abbreviates any years
#'to their two-digit representation (e.g., 2008 to '08), but not abbreviating
#'any specified breaks.
#'
#'@param full_by_pos Vector of integers, the position of breaks that should not
#'  be abbreviated.
#'@param full_by_value Vector of integers, the value of breaks that should not
#'  be abbreviated.
#'
#'@export
abbreviate_dates <- function(breaks,
                             full_by_pos = c(1),full_by_date = NULL
                             ) {
    # Determine length of each break
    lengths <- stringr::str_length(as.integer(breaks))

    # # Stop if the breaks are not in a four-digit format - this seems to cause errors with pretty breaks
    # if (!(min(lengths == 4) & max(lengths == 4))) {
    #   stop("Breaks are not in a four-digit format. Cannot abbreviate.")
    # }
    #
    # # Stop if breaks cannot be coerced to a number
    # tryCatch({as.integer(breaks)},
    #          warning = function(w){stop("Breaks cannot be coerced to a year. Cannot abbreviate.")})

    # Abbreviate all values
    abbr <- paste0("'",substr(breaks,3,4))

    # Add back any specified non-abbreviations
    if(!is.null(full_by_date)) {
      full_by_pos <- sort(unique(c(full_by_pos,match(breaks,full_by_date))))
    }
 ##### NOTE - there are issues with pretty breaks, it increments the adjusted labels up by one. Not sure what's going on.
    if(!is.null(full_by_pos)) {
      abbr[full_by_pos] <- breaks[full_by_pos]
    }

    return(abbr)
  }
