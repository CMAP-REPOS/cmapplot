#'Axis handling helper functions
#'
#'`abbr_years_cont()` and `abbr_years_date()` are helper functions that allow
#'users to abbreviate year labels to their two-digit representation (e.g., 2008
#'to '08), but not abbreviating any specified breaks.
#'
#'@importFrom stringr str_length
#'
#'@examples
#'\dontrun{
#' # Default implementation - this will abbreviate all labels except the first
#' # for both continuous and date scales, using their respective abbreviation
#' # function.
#'
#' df2 <- dplyr::mutate(transit_ridership,year2 = as.Date(lubridate::date_decimal(year)))
#'
#' df2 <- dplyr::mutate(transit_ridership,year2 = as.Date(lubridate::date_decimal(year)))
#' df1 <- filter(df2,year >= 2000)
#'
#' ggplot(df1,
#'        aes(x = year, y = ridership, color = system)) +
#'   geom_line() +
#'   scale_x_continuous(labels = abbr_years_cont)
#'
#' ggplot(df1,
#'        aes(x = year2, y = ridership, color = system)) +
#'   geom_line() +
#'   scale_x_date(labels = abbr_years_date)
#'
#' # If customizations are desired, the function should be input using the format
#' # displayed below: \code{function(i) abbr_years_cont(i,full_by_pos = *,
#' # full_by_year = *)}.
#'
#' # This example shows the use of \code{full_by_year} to maintain full labels on
#' # the first entry (the default) and add a full label to the year 2000, the first
#' # of the 21st century.
#' ggplot(df2,
#'        aes(x = year2, y = ridership, color = system)) +
#'   geom_line() +
#'   scale_x_date(labels = function(i) abbr_years_date(i,full_by_year = c(2000)))
#'
#' # You can also remove the default maintenance of the first label and only
#' # specify specific years.
#' ggplot(df2,
#'        aes(x = year, y = ridership, color = system)) +
#'   geom_line() +
#'   scale_x_continuous(labels = function(i) abbr_years_cont(i,full_by_pos = NULL,
#'                                                           full_by_year = c(1990,2020)))
#'}
#'
#'@param breaks Vector, the input break values to be abbreviated. This will be
#'  automatically supplied to the function by setting \code{labels =
#'  abbr_years_cont} or \code{labels = abbr_years_date} inside the
#'  \code{scale_*_continuous} argument (for \code{abbr_years_cont()} or inside
#'  the \code{scale_*_date} argument (for \code{abbr_years_date}).
#'@param full_by_pos Vector of integers, the position of breaks that should not
#'  be abbreviated. This defaults to \code{c(1)}, which retains the original
#'  first label and abbreviates subsequent ones. If all breaks should be
#'  abbreviated, this can be set to NULL.
#'@param full_by_year Vector of integers, the value of breaks that should not be
#'  abbreviated. Defaults to NULL.
#'
#'@describeIn abbr_years_cont For continuous scales
#'@export
abbr_years_cont <- function(breaks,
                            full_by_pos = c(1),
                            full_by_year = NULL) {

    # Determine length of each break. Note that when breaks have been modified,
    # ggplot sometimes adds an additional NA break in the first and last
    # positions. We check whether this is the case and, if so, remove those for
    # testing purposes.
    lengths <- stringr::str_length(as.integer(breaks[which(!is.na(breaks))]))

    # # Stop if the breaks are not in a four-digit format.
    # if (!(min(lengths == 4) & max(lengths == 4))) {
    #   stop("Breaks are not in a four-digit format. Cannot abbreviate.")
    # }

    # # Stop if breaks cannot be coerced to a number
    # tryCatch({as.integer(breaks)},
    #          warning = function(w){stop("Breaks cannot be coerced to a year. Cannot abbreviate.")})

    # Abbreviate all values
    abbr <- paste0("'",substr(breaks,3,4))

    # Account for leading NAs and increment up positions accordingly
    leading_na <- which.min(is.na(breaks)) - 1
    full_by_pos <- full_by_pos + leading_na

    # Now convert referenced dates into positions
    if(!is.null(full_by_year)) {
      full_by_pos <- sort(unique(c(full_by_pos,match(full_by_year,breaks))))
    }

    # Add back full years for specified positions
    if(!is.null(full_by_pos)) {
      abbr[full_by_pos] <- breaks[full_by_pos]
    }

    return(abbr)
  }


#'@describeIn abbr_years_cont For date scales
#'
#'@export
abbr_years_date <- function(breaks,
                            full_by_pos = c(1),
                            full_by_year = NULL) {

  # Convert given year integers into the number of days since 1970 (note this is
  # how the breaks are handled internally by ggplot - they live in the following
  # location of a ggplot chart 'a' that has been passed to ggplot_build()
  #          ggplot_build(a)$layout$panel_params[[1]]$x$breaks
  # Note that breaks for the y axis can be accessed similarly.
  if (!is.null(full_by_year))
    {full_by_year <- as.numeric(as.Date(paste0(full_by_year,"-01-01")))}

  # Stop if breaks cannot be coerced to a date
  tryCatch({as.Date(breaks)},
           warning = function(w){stop("Breaks cannot be coerced to a date. Cannot abbreviate.")})

  # Abbreviate all values - ggplot appears to correctly use the names of the
  # breaks (which are the year values) when performing this operation on the
  # breaks.
  abbr <- paste0("'",substr(breaks,3,4))

  # Account for leading NAs (see 'cont' variant for explanation) and increment
  # up positions accordingly
  leading_na <- which.min(is.na(breaks)) - 1
  if(!is.null(full_by_pos)) {full_by_pos <- full_by_pos + leading_na}

  # Now convert referenced dates into positions, adding them and removing duplicates
  if(!is.null(full_by_year)) {
    full_by_pos <- sort(
      unique(
        c(full_by_pos,
          match(full_by_year,breaks))))
  }

  # Add back full years for specified positions
  if(!is.null(full_by_pos)) {
    # Extract the name of the break in ggplot's named list of numbers, which is
    # the 4-digit year
    abbr[full_by_pos] <- names(breaks)[full_by_pos]
  }

  return(abbr)
}
