#'Axis handling helper functions
#'
#'`abbr_years()` is a helper functions that allows users to abbreviate year
#'labels to their two-digit representation (e.g., 2008 to '08), but not
#'abbreviating any specified breaks. It does so by creating a new function that
#'takes the breaks supplied by \code{ggplot} as its only argument. The
#'function was modeled after the syntax and approach of the labeling functions
#'in the \code{scales::label_*} family.
#'
#'@importFrom stringr str_length
#'
#'@examples
#'\dontrun{
#' # Default implementation - this will abbreviate all labels except the first
#' # for both continuous and date scales, using their respective abbreviation
#' # function. Note the syntax - this function actually produces another
#' # function, and so you must include the opening and closing parentheses
#' # following the function's name (i.e., \code{abbr_year()}).
#'
#' df2 <- dplyr::mutate(transit_ridership,year2 = as.Date(paste0(year,"-01-01")))
#' df1 <- filter(df2,year >= 2000)
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
#'}
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

    # For integer breaks, determine length of each break. Note that when breaks
    # have been modified, ggplot sometimes adds an additional NA break in the
    # first and last positions. We check whether this is the case and, if so,
    # remove those for testing purposes.
    if (!dateaxis) {
      actual_breaks <- breaks[which(!is.na(breaks))]

      lengths <- stringr::str_length(as.integer(actual_breaks))

      # # Stop if the breaks are not in a four-digit format.
      if (!(min(lengths == 4) & max(lengths == 4))) {
        stop("Breaks are not in a four-digit format. Cannot abbreviate.")
        }

      # # Stop if breaks cannot be coerced to a number
      tryCatch({as.integer(actual_breaks)},
               warning = function(w){stop("Breaks cannot be coerced to a year. Cannot abbreviate.")})
    }

    # If a date axis, convert given year integers into the number of days since
    # 1970 (note this is how the breaks are handled internally by ggplot - they
    # live in the following location of a ggplot chart 'a' that has been passed
    # to ggplot_build() ggplot_build(a)$layout$panel_params[[1]]$x$breaks Note
    # that breaks for the y axis can be accessed similarly.
    if (dateaxis) {
      if (!is.null(full_by_year))
      {full_by_year <- as.numeric(as.Date(paste0(full_by_year,"-01-01")))}
    }

    # Abbreviate all values
    abbr <- paste0("'",substr(breaks,3,4))

    # Account for leading NAs and increment up positions accordingly
    leading_na <- which.min(is.na(breaks)) - 1
    if(!is.null(full_by_pos)) {full_by_pos <- full_by_pos + leading_na}

    # Now convert referenced dates into positions
    if(!is.null(full_by_year)) {
      full_by_pos <- sort(unique(c(full_by_pos,match(full_by_year,breaks))))
    }

    # Add back full years for specified positions
    if(!is.null(full_by_pos)) {
      # If the date axis, extract the name of the break in ggplot's named list
      # of numbers, which is the 4-digit year
      if (dateaxis) {
      abbr[full_by_pos] <- names(breaks)[full_by_pos]
      }
      # Otherwise, use the normal breaks
      else {
        abbr[full_by_pos] <- breaks[full_by_pos]
      }
    }

    return(abbr)
  }

  return(fxn)
}
