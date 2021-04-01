#'Axis handling helper functions
#'
#'`abbr_years_cont()` and `abbr_years_date()` are helper functions that allow
#'users to abbreviate year labels to their two-digit representation (e.g., 2008
#'to '08), but not abbreviating any specified breaks. They do so by creating a
#'new function that takes the breaks supplied by \code{ggplot} as their only
#'argument. These functions were modeled after the syntax and approach of the
#'labeling functions in the \code{scales::label_*} family.
#'
#'@importFrom stringr str_length
#'
#'@examples
#'\dontrun{
#' # Default implementation - this will abbreviate all labels except the first
#' # for both continuous and date scales, using their respective abbreviation
#' # function. Note the syntax - this function actually produces another
#' # function, and so you must include the opening and closing parentheses
#' # following the function's name (i.e., \code{abbr_years_cont()}).
#'
#' df2 <- dplyr::mutate(transit_ridership,year2 = as.Date(paste0(year,"-01-01")))
#' df1 <- filter(df2,year >= 2000)
#'
#' ggplot(df1,
#'        aes(x = year, y = ridership, color = system)) +
#'   geom_line() +
#'   scale_x_continuous(labels = abbr_years_cont())
#'
#' ggplot(df1,
#'        aes(x = year2, y = ridership, color = system)) +
#'   geom_line() +
#'   scale_x_date(labels = abbr_years_date())
#'
#' # If customizations are desired, users can use \code{full_by_pos} and/or
#' # \code{full_by_year} to maintain the full version of the specified labels.
#'
#' ggplot(df2,
#'        aes(x = year2, y = ridership, color = system)) +
#'   geom_line() +
#'   scale_x_date(labels = abbr_years_date(full_by_year = c(2000)))
#'
#' # You can also remove the default maintenance of the first label and only
#' # specify specific years.
#' ggplot(df2,
#'        aes(x = year, y = ridership, color = system)) +
#'   geom_line() +
#'   scale_x_continuous(labels = abbr_years_cont(full_by_pos = NULL,
#'                                               full_by_year = c(1990,2020)))
#'}
#'
#'@param full_by_pos Vector of integers, the position of breaks that should not
#'  be abbreviated. This defaults to \code{c(1)}, which retains the original
#'  first label and abbreviates subsequent ones. If all breaks should be
#'  abbreviated, this can be set to NULL.
#'@param full_by_year Vector of integers, the value of breaks that should not be
#'  abbreviated. Defaults to NULL.
#'
#'@describeIn abbr_years_cont For continuous scales
#'@export
abbr_years_cont <- function(full_by_pos = c(1),
                            full_by_year = NULL) {

  fxn <- function(breaks) {

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

  return(fxn)
}

#'@describeIn abbr_years_cont For date scales
#'
#'@export
abbr_years_date <- function(full_by_pos = c(1),
                            full_by_year = NULL) {
  fxn <- function(breaks) {

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

  return(fxn)
}

