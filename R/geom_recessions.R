#'Add recessions to time series graphs
#'
#'\code{geom_recessions} returns one or two ggplot geoms that add rectangles
#'representing recessions to a plot. It will either return only rectangles or,
#'by default, both rectangles and text identifying each recession.
#'
#'@param xformat Char, a string indicating whether the x axis of the primary
#'  data being graphed is in integer or date format. This argument will
#'  currently accept one of \code{c("numeric", "date")}.
#'@param text Logical, whether or not to include labels that identify each box
#'  as a recession.
#'@param label Char, the text to label each recession. Defaults to " Recession".
#'  (The space is a more consistent y axis buffer than text_nudge_y because it
#'  not relative to the scale of the y axis.)
#'@param ymin,ymax Numeric, The height of the recession rectangles. Defaults to
#'  -Inf and +Inf. Override to the top and bottom gridlines to implement ideal
#'  CMAP design standards.
#'@param fill Char, the fill color for the recession rectangles. Defaults to
#'  \code{#002d49} for compliance with CMAP design standards.
#'@param text_nudge_x,text_nudge_y Numeric, the amount to shift the labels along
#'  each axis. Defaults to 0.2 and 0, respectively. Note that these use the x
#'  and y scales so will need to be adjusted depending on what is being graphed.
#'  `text_nudge_y` only works when `ymax` is not set to `+Inf`, which is the
#'  default. Consider setting `ymax` equal to the top of your graph or top
#'  gridline as an additional argument in `geom_recessions()`.
#'@param show.legend Logical, whether to render the rectangles in the legend.
#'  Defaults to \code{FALSE}.
#'@param rect_aes,text_aes Named list, additional aesthetics to send to the
#'  rectangle and text geoms, respectively.
#'@param update_recessions Logical or data frame. \code{FALSE}, the default,
#'  relies on the package's built in recessions table, which was last updated in
#'  March 2021 and is loaded into the \code{sysdata.R} file located in the
#'  \code{R} directory. \code{TRUE} calls the function
#'  \code{update_recessions}, which attempts to fetch the current recessions
#'  table from the NBER website. A custom data table of recessions can also be
#'  passed to this argument, but it must be structured identically to the
#'  five-column data table described in the the documentation file for the
#'  function \code{update_recessions}.
#'@param show_ongoing Logical. \code{TRUE}, the default, will display an ongoing
#'  recession that does not yet have a defined end date. If an ongoing recession
#'  exists, it will be displayed as extending through the maximum extent of the
#'  graph's data (up to 2200). \code{FALSE} will remove the ongoing recession
#'  from the graph.
#'@param ... additional aesthetics to send to BOTH the rectangle and text geoms.
#'
#'@section Important notes: If \code{show.legend = TRUE} you must place any
#'  categorical aesthetics (e.g. color, size) specific to the primary data in
#'  the geom(s) used to display that data. Otherwise, the legend will inherit
#'  aesthetics from geom_recessions.
#'
#'  It is best to place this object before your primary geom (likely
#'  \code{geom_line()}) in your code, so that ggplot draws it behind the primary
#'  data being drawn.
#'
#'@section Default color: The CMAP color palette gray used for recessions is
#'  \code{#e3e8eb}. The rectangle geom has default fill and alpha values of
#'  \code{#002d49} and \code{0.11} built into the function. These replicate the
#'  palette color at the highest possible transparency. This is done because
#'  there is no known way to place the recession geom behind the graph's grid
#'  lines. The default therefore produces the approved CMAP color while altering
#'  the appearance of any overlapping grid lines as little as possible. These
#'  can be overridden, but separately. Override fill using the top-level
#'  argument, as in \code{fill = "red"}. Override alpha within rect_aes as in
#'  \code{rect_aes = list(alpha = 0.5)}. Color and alpha were calculated using
#'  the hints found here:
#'  \url{https://stackoverflow.com/questions/6672374/convert-rgb-to-rgba-over-white}.
#'
#'
#'@section Under the hood: This function calls two custom geoms, constructed
#'  with ggproto. The custom GeomRecessions and GeomRecessionsText are modified
#'  versions of GeomRect and GeomText, respectively. The only variations to each
#'  occur in \code{default_aes}, \code{required_aes}, and \code{setup_data}
#'  arguments. These variations allow the the primary dataframe (specified in
#'  \code{ggplot(data = XXX)}) to filter the recessions displayed.
#'
#' @examples
#' grp_goods <- dplyr::filter(grp_over_time, category == "Goods-Producing")
#' grp_goods <- dplyr::mutate(grp_goods, year2 = as.Date(lubridate::date_decimal(year)))
#'
#' # INTEGER X AXIS:
#' ggplot(grp_over_time, aes(x = year, y = realgrp, color = cluster)) +
#'   geom_recessions() +
#'   geom_line() +
#'   scale_x_continuous("Year") +
#'   theme_minimal()
#'
#' # DATE X AXIS:
#' ggplot(data = grp_goods,
#' mapping = aes(x = year2, y = realgrp, color = cluster)) +
#'   geom_recessions(xformat = "date") +
#'   geom_line() +
#'   scale_x_date("Year") +
#'   theme_minimal()
#'
#' # MODIFIED AESTHETICS:
#' ggplot(grp_over_time, aes(x = year, y = realgrp)) +
#'   geom_recessions(show.legend = TRUE, fill = "blue", text = FALSE,
#'                   rect_aes = list(alpha = 1, color = "red")) +
#'   geom_line(aes(color = cluster)) +
#'   scale_x_continuous("Year") +
#'   theme_minimal()
#'
#'
#' # BELOW EXAMPLES SHOW MORE THAN 1 RECESSION
#' df <- data.frame(year_dec=1950:1999, value=rnorm(100), var=c(rep("A", 50), rep("B", 50)))
#' df$year_date <- as.Date(lubridate::date_decimal(df$year_dec))
#'
#' # A plot with an integer-based x axis
#' ggplot(df, mapping = aes(x = year_dec, y = value)) +
#'   geom_recessions() +
#'   geom_line(aes(color = var)) +
#'   scale_x_continuous("Year") +
#'   theme_minimal()
#'
#' # A plot with a date-based x axis
#' ggplot(df, mapping = aes(x = year_date, y = value)) +
#'   geom_recessions(xformat = "date", show.legend = TRUE) +
#'   geom_line(aes(color = var)) +
#'   scale_x_date() +
#'   theme_minimal()
#'
#'@importFrom jsonlite fromJSON
#'
#'@seealso \itemize{ \item \url{https://ggplot2-book.org/extensions.html} \item
#'  \url{https://github.com/brodieG/ggbg/blob/development/inst/doc/extensions.html#stat-compute}
#'   \item \url{https://rpubs.com/hadley/97970} \item
#'  \url{https://ggplot2.tidyverse.org/articles/extending-ggplot2.html} }
#'
#'@export
geom_recessions <- function(xformat = "numeric",
                            text = TRUE,
                            label = " Recession",
                            ymin = -Inf,
                            ymax = Inf,
                            fill = "#002d49",
                            text_nudge_x = 0.2,
                            text_nudge_y = 0,
                            show.legend = FALSE,
                            rect_aes = NULL,
                            text_aes = NULL,
                            update_recessions = FALSE,
                            show_ongoing = TRUE,
                            ...) {

  # build recessions table for use in function, but hide it in a list
  # because of ggplot's requirement that parameters be of length 1
  recess_table <- list(build_recessions(update_recessions))

  # return a series of gg objects to ggplot
  list(
    layer(
      data = NULL,
      # An aesthetic mapping is required to generate a legend entry
      mapping = aes(fill = "Recession"),
      stat = "identity",
      geom = GeomRecessions,
      position = "identity",
      show.legend = if_else(show.legend, NA, FALSE),
      inherit.aes = TRUE,
      params = append(
        list(
          xformat = xformat,
          ymin = ymin,
          ymax = ymax,
          recess_table = recess_table,
          show_ongoing = show_ongoing,
          ...
        ),
        rect_aes
      )
    ),
    if (text) {
      layer(
        data = NULL,
        mapping = NULL,
        stat = "identity",
        geom = GeomRecessionsText,
        position = position_nudge(x = text_nudge_x, y = 0),
        show.legend = FALSE,
        inherit.aes = TRUE,
        params = append(
          list(
            xformat = xformat,
            label = label,
            y = ymax + text_nudge_y,
            # Because ymax is Inf by default, adjustments to this setting
            #  require manually setting `ymax` in the call to `geom_recessions`
            recess_table = recess_table,
            show_ongoing = show_ongoing,
            ...
          ),
          text_aes
        )
      )
    },
    # apply the fill color to values where fill="Recession". Push this to the legend if called for.
    do.call(
      scale_fill_manual,
      list(values = c("Recession" = fill), if (show.legend) {guide = "legend"})
    )
  )

}


# internal function used to define recessions table for use
build_recessions <- function(update_recessions){
  if(is.logical(update_recessions)){
    # if TRUE
    if(update_recessions){
      message("Trying to update recessions...")
      updated_recessions <- suppressWarnings(update_recessions(quietly = TRUE))

      # If updated_recessions is returned as NULL, use the default table
      if (is.null(updated_recessions)) {
        message("Could not update recessions. Using built-in recessions table...")
        return(recessions)
      }

      message("Successfully fetched from NBER")
      return(updated_recessions)
      # if FALSE
    } else {
      return(recessions)
    }
  # if DATAFRAME
  }else if(is.data.frame(update_recessions)){
    # confirm that table has correct structure
    if(!identical(update_recessions[NA,][1,], recessions[NA,][1,])){
      message("Recession table may not have correct format (See `?update_recessions`). Attempting anyway...")
    }
    return(update_recessions)
  # OTHERWISE
  }else{
    message("`update_recessions` must be TRUE, FALSE, or a data frame. Using built-in recessions table...")
    return(recessions)
  }
}

#' @importFrom lubridate decimal_date

# Internal function designed to filter the built-in recessions table
filter_recessions <- function(min, max, xformat, show_ongoing, recess_table){
  # Bind local variables to function
  end_num <- start_num <- end_date <- start_date <- end <- start <- ongoing <- NULL

  # unwrap recess_table from list
  recess_table <- recess_table[[1]]

  # Filtering out ongoing recessions if specified
  if (!show_ongoing) {recess_table <- dplyr::filter(recess_table, ongoing == F)}

  # use xformat to create correct "start" and "end" vars...
  if (xformat == "date") {
    # ... by renaming existing date fields (for date axis)
    recessions <- dplyr::rename(recess_table, start = start_date, end = end_date)
  } else {
    # ... or by creating decimal dates (for numeric axis)
    if (xformat != "numeric") {
      warning("geom_recessions currently only supports x axes in the numeric and date formats. Using numeric.")
    }
    recessions <- dplyr::mutate(
      recess_table,
      start = lubridate::decimal_date(start_date),
      end = lubridate::decimal_date(end_date)
      )
  }

  # Remove recessions outside of range
  recessions <- dplyr::filter(recessions, end > min & start < max)

  # If `min` or `max` fall in  middle of a recession, modify recession to end at specified term.
  recessions <- dplyr::transmute(
    recessions,
    start = if_else(start < min, min, as.numeric(start)),
    end = if_else(end > max, max, as.numeric(end)),
  )

  return(recessions)
}


#' @name customproto
NULL

#' @describeIn customproto Add recession bars to plot.
#' @format NULL
#' @usage NULL
#' @export
GeomRecessions <- ggproto(
  "GeomRecessions", Geom,
  default_aes = aes(colour = NA, alpha = 0.11, size = 0.5, linetype = 1, na.rm = TRUE),

  required_aes = c("xformat", "ymin", "ymax", "show_ongoing", "recess_table" ,"fill"),

  # replace `data` with `recessions`, filtered by `data`
  setup_data = function(data, params) {
    #filter recessions based on date parameters from `data` and return it. This overwrites `data`.
    data <- filter_recessions(min = min(data$x), max = max(data$x),
                              xformat = params$xformat,
                              show_ongoing = params$show_ongoing,
                              recess_table = params$recess_table)

    # set up data for GeomRect
    data <- dplyr::transmute(
      data,
      xmin = start,
      xmax = end,
      ymin = params$ymin,
      ymax = params$ymax,
      PANEL = 1,
      group = -1,
      # re-establish dummy aesthetic, needed for legend entry
      fill = "Recession"
    )

    return(data)
  },

  # remainder untouched from `geom_rect`:
  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {

    if (!coord$is_linear()) {
      aesthetics <- setdiff(
        names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
      )

      polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
        poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
        aes <- new_data_frame(row[aesthetics])[rep(1,5), ]

        GeomPolygon$draw_panel(cbind(poly, aes), panel_params, coord)
      })

      ggname("bar", do.call("grobTree", polys))
    } else {
      coords <- coord$transform(data, panel_params)
      ggname("geom_rect", rectGrob(
        coords$xmin, coords$ymax,
        width = coords$xmax - coords$xmin,
        height = coords$ymax - coords$ymin,
        default.units = "native",
        just = c("left", "top"),
        gp = gpar(
          col = coords$colour,
          fill = alpha(coords$fill, coords$alpha),
          lwd = coords$size * .pt,
          lty = coords$linetype,
          linejoin = linejoin,
          # `lineend` is a workaround for Windows and intentionally kept unexposed
          # as an argument. (c.f. https://github.com/tidyverse/ggplot2/issues/3037#issuecomment-457504667)
          lineend = if (identical(linejoin, "round")) "round" else "square"
        )
      ))
    }
  },

  draw_key = draw_key_polygon
)


#' @describeIn customproto Add recession bar labels to plot.
#' @format NULL
#' @usage NULL
#' @export
GeomRecessionsText <- ggproto(
  "GeomRecessionsText", Geom,

  required_aes = c("xformat", "label",  "show_ongoing", "recess_table", "y"),

  default_aes = aes(
    colour = "black", size = 3.88, alpha = NA, family = "", fontface = 1, lineheight = 1.2,
    xformat = NULL, angle = 270, parse = FALSE,
    check_overlap = FALSE, na.rm = TRUE,
    hjust = "left", vjust = "bottom"
  ),

  # replace `data` with `recessions`, filtered by `data`
  setup_data = function(data, params) {
    #filter recessions based on date parameters from `data` and return it. This overwrites `data`.
    data <- filter_recessions(min = min(data$x), max = max(data$x),
                              xformat = params$xformat,
                              show_ongoing = params$show_ongoing,
                              recess_table = params$recess_table)

    # set up data for GeomRect
    data <- dplyr::transmute(
      data,
      x = end,
      y = params$y,
      PANEL = 1,
      group = -1
    )

    return(data)
  },

  # remainder untouched from `geom_text`:
  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE) {
    lab <- data$label
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }

    data <- coord$transform(data, panel_params)
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }

    textGrob(
      lab,
      data$x, data$y, default.units = "native",
      hjust = data$hjust, vjust = data$vjust,
      rot = data$angle,
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        fontsize = data$size * .pt,
        fontfamily = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight
      ),
      check.overlap = check_overlap
    )
  },

  draw_key = draw_key_text
)


#'Update recessions table
#'
#'The cmapplot package contains an internal dataset \code{recessions} of all
#'recessions in American history as recorded by the National Bureau of Economic
#'Research (NBER). However, users may need to replace the built-in data, such as
#'in the event of new recessions and/or changes to the NBER consensus on
#'recession dates. This function fetches and interprets this data from the NBER
#'website.
#'
#'@param url Char, the web location of the NBER machine-readable CSV file. The
#'  default, \code{NULL}, uses the most recently identified URL known to the
#'  package development team, which appears to be the most stable location for
#'  updates over time.
#'@param quietly Logical, suppresses messages produced by
#'  \code{utils::download.file}.
#'
#'@return A data frame with the following variables: \itemize{ \item
#'  \code{start_char, end_char}: Chr. Easily readable labels for the beginning
#'  and end of the recession. \item \code{start_date, end_date}: Date. Dates
#'  expressed in R datetime format, using the first day of the specified month.
#'  \item \code{ongoing}: Logical. Whether or not the recession is ongoing as of
#'  the latest available NBER data. }
#'
#'@source \url{https://www.nber.org/sites/default/files/2023-03/BCDC_spreadsheet_for_website.xlsx}
#'
#' @examples
#' recessions <- update_recessions()
#'
#' # package maintainers can update the internal dataset from within
#' # package by running the following code:
#' \dontrun{
#'   recessions <- update_recessions()
#'   usethis::use_data(recessions, internal = TRUE, overwrite = TRUE)
#' }
#'
#'@export
update_recessions <- function(url = NULL, quietly = FALSE){

  # Use default URL if user does not override
  if (is_null(url) | missing(url)) {
    url <- "https://www.nber.org/sites/default/files/2023-03/BCDC_spreadsheet_for_website.xlsx"
  }

  # locally bind variable names
  start_char <- end_char <- start_date <- end_date <- ongoing <- index <- peak <- trough <- NULL

  return(
    # attempt to download and format recessions table
    tryCatch({
      recessions <- readxl::read_xlsx(url) %>%
        # drop first row trough
        dplyr::slice(-1) %>%
        # convert peaks and troughs...
        dplyr::mutate(
          # ...to R dates
          start_date = as.Date(peak),
          end_date = as.Date(trough),
          # ... and clean char strings
          start_char = format(start_date, "%b %Y"),
          end_char = format(end_date, "%b %Y")) %>%
        # confirm ascending and create row number
        dplyr::arrange(start_date) %>%
        mutate(index = row_number()) %>%
        mutate(
          # Flag unfinished recessions
          ongoing = case_when(
            is.na(end_date) & index == max(.$index) ~ T,
            TRUE ~ F),
          # set ongoing recession to arbitrary future date
          end_date = case_when(
            ongoing ~ as.Date("2200-01-01"),
            TRUE ~ end_date),
          # mark ongoing recession in char field
          end_char = case_when(
            ongoing ~ "Ongoing",
            TRUE ~ end_char)
          ) %>%
        # clean up
        select(start_char, end_char, start_date, end_date, ongoing)

      if (!quietly) {message("Successfully fetched from NBER")}

      # Return recessions
      recessions
    },
    error = function(cond){
      if (!quietly) message("WARNING: Fetch or processing failed. `NULL` returned.")
      return(NULL)
    }
    )
  )
}
