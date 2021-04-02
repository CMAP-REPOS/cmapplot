#'Axis handling helper functions
#'
#'`abbr_years()` is a helper functions that allows users to abbreviate year
#'labels to their two-digit representation (e.g., 2008 to '08), but not
#'abbreviating any specified breaks. It does so by creating a new function that
#'takes the breaks supplied by \code{ggplot2} as its only argument. The
#'function was modeled after the syntax and approach of the labeling functions
#'in the \code{scales::label_*} family.
#'
#'@importFrom stringr str_length
#'@importFrom lubridate year month day
#'@importFrom stats na.omit
#'
#'@examples
#'
#'# basic functionality
#'abbr_years()(c(2010:2020))
#'abbr_years(full_by_year = 2000)(c(1990:2010))
#'
#'
#' # Default implementation - this will abbreviate all labels except the first
#' # for both continuous and date scales.
#'
#' df2 <- dplyr::mutate(transit_ridership, year2 = as.Date(lubridate::date_decimal(year)))
#' df1 <- dplyr::filter(df2, year >= 2000)
#'
#' ggplot(df1,
#'        aes(x = year, y = ridership, color = system)) +
#'   geom_line() +
#'   scale_x_continuous(labels = abbr_years())
#'
#' ggplot(df1,
#'        aes(x = year2, y = ridership, color = system)) +
#'   geom_line() +
#'   scale_x_date(labels = abbr_years(dateaxis = TRUE))
#'
#' # If customizations are desired, users can use \code{full_by_pos} and/or
#' # \code{full_by_year} to maintain the full version of the specified labels.
#'
#' ggplot(df2,
#'        aes(x = year2, y = ridership, color = system)) +
#'   geom_line() +
#'   scale_x_date(labels = abbr_years(full_by_year = c(2000), dateaxis = TRUE))
#'
#' # You can also remove the default maintenance of the first label and only
#' # specify specific years.
#' ggplot(df2,
#'        aes(x = year, y = ridership, color = system)) +
#'   geom_line() +
#'   scale_x_continuous(labels = abbr_years(full_by_pos = NULL,
#'                                          full_by_year = c(1990,2020)))
#'
#'
#'@param full_by_pos Vector of integers, the position of breaks that should not
#'  be abbreviated. This defaults to \code{c(1)}, which retains the original
#'  first label and abbreviates subsequent ones. If all breaks should be
#'  abbreviated, this can be set to NULL.
#'@param full_by_year Vector of integers, the value of breaks that should not be
#'  abbreviated. Defaults to NULL.
#'@param dateaxis Bool. \code{FALSE}, the default, directs the function to treat
#'  the breaks as integers. If set to \code{TRUE} the function will instead
#'  treat the breaks as date objects. \code{TRUE} should be used when called
#'  within a \code{scale_*_date} ggplot element.
#'
#'@export
abbr_years <- function(full_by_pos = c(1),
                       full_by_year = NULL,
                       dateaxis = FALSE) {

  fxn <- function(breaks) {

    # If a date axis, breaks are stored as number of days since 1/1/1970. These
    # must be converted to integer years, but this should error if all breaks
    # don't fall on the same calendar day of a distinct year.
    if (dateaxis) {
      dates <- as.Date(breaks, origin = "1970-01-01")

      if (length(unique(month(stats::na.omit(dates)))) != 1 |
          length(unique(day(stats::na.omit(dates)))) != 1) {
        message(paste(
          paste("Currently, breaks are:", paste(dates[!is.na(dates)], collapse = ", ")),
          "This function only works if all breaks are on identical calendar days.",
          sep = "\n")
        )
        stop("Breaks cannot be abbreviated.", call. = FALSE)
      }

      breaks <- lubridate::year(dates)
    }

    # Stop if the breaks are not in a four-digit format.
    if (!all(stringr::str_length(breaks) == 4, na.rm = TRUE)) {
      message(paste(
        paste("Currently, breaks are:", paste(breaks[!is.na(breaks)], collapse = ", ")),
        "Remove any breaks that contain decimals. Consider `breaks = scales::pretty_breaks()`",
        "If the axis is in date format, use `abbr_years(dateaxis = TRUE)`.",
        sep = "\n")
      )
      stop("Breaks cannot be abbreviated.", call. = FALSE)
      }

    # Abbreviate all values
    abbr <- paste0("'",substr(breaks,3,4))

    # If there is a leading NA, increment up positions accordingly
    leading_na <- which.min(is.na(breaks)) - 1
    if(!is.null(full_by_pos)) {
      full_by_pos <- full_by_pos + leading_na
    }

    # Convert specified years into positions
    if(!is.null(full_by_year)) {
      full_by_pos <- sort(unique(c(full_by_pos,match(full_by_year,breaks))))
    }

    # Add back full years for specified positions
    abbr[full_by_pos] <- breaks[full_by_pos]

    return(abbr)
  }

  return(fxn)
}
