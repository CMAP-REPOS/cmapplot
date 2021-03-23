#'Axis handling helper functions
#'
#'This file includes helper functions that improve axis handling.
#'
#'@importFrom stringr str_length


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

    # Stop if the breaks are not in a four-digit format
    if (!(min(lengths == 4) & max(lengths == 4))) {
      stop("Breaks are not in a four-digit format. Cannot abbreviate.")
    }

    # Stop if breaks cannot be coerced to a number
    tryCatch({as.integer(breaks)},
             warning = function(w){stop("Breaks cannot be coerced to a year. Cannot abbreviate.")})

    # Abbreviate all values
    abbr <- paste0("'",substr(breaks,3,4))

    # Add back any specified non-abbreviations
    if(!is.null(full_by_date)) {
      full_by_pos <- arrange(distinct(c(full_by_pos,match(breaks,full_by_date))))
    }

    if(!is.null(full_by_pos)) {
      abbr[full_by_pos] <- breaks[full_by_pos]
    }

    return(abbr)
  }
