# my attempt at building from scratch from https://ggplot2-book.org/spring1.html

filter_recessions <- function(min, max, xdata){
  # construct recessions dataset for this geom:

  # first, remove recessions outside of range
  recessions2 <- dplyr::filter(cmapplot::recessions, end_int > min & start_int < max)

  # second, if `min` or `max` fall in  middle of a recession, modify recession to
  # end at specified term. Then, rebuild dates from ints
  recessions2 <- dplyr::mutate(recessions2,
                               start_int = if_else(start_int < min, min, start_int),
                               end_int = if_else(end_int > max, max, end_int),
                               start_date = as.Date(lubridate::date_decimal(start_int)),
                               end_date = as.Date(lubridate::date_decimal(end_int))
  )

  # third, select the preferred data type (int or date format)
  recessions2 <- dplyr::select(recessions2, ends_with(xdata))

  # fourth, rename variables so they are easy to call below
  names(recessions2) <- c("start", "end")

  return(recessions2)
}


# this works
recession_test <- filter_recessions(1950,2010, "int")
ggplot(recession_test) + geom_rect(aes(xmin = start, xmax = end, ymin = 0, ymax = 1))

# but I got lost building the stat.
StatRecessions <- ggproto("StatRecessions", Stat,
                          setup_params = function(data, params){
                            # handle multiple constructions of `xdata`
                            if (xdata %in% c("i", "int", "integer", "integers")) {
                              xdata <- "int"
                            } else if (xdata %in% c("d", "date", "dates")) {
                              xdata <- "date"
                            } else {
                              stop("Incorrect `xdata` specified", call. = FALSE)
                            }

                            # handle labels
                            if(is.null(text)){
                              label <- FALSE
                            } else if (!is.logical(text)){
                              stop("text must be TRUE or FALSE", call. = FALSE)
                            }

                            # any handling for text_nudge_x, fill, or alpha ?

                            params
                          },
                          compute_panel = function(data, scales)

)
