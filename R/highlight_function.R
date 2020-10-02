#' Highlight palette prep function
#'
#' Create named list to serve as palette input for following fill/color highlight functions
#'
#' @param variable group vector
#' @param highlight name of group of interest
#' @param color_main hexcode of highlighted color
#' @param color_other hexcode of non-highlighted color
#'
#' @noRd
make_highlight_palette <- function(variable, highlight, color_main, color_other){

  n <- length(unique(variable))

  palette <- rep.int(c(other = color_other), n)

  names(palette) <- levels(factor(variable))

  palette[[highlight]] <- color_main

  return(palette)

}

#' Highlight one discrete group in a comparison graph
#'
#' Pick the function depending on the aesthetic of your ggplot object (fill or color).
#' Specify the unique factor name of the group you're highlighting (as a case-sensitive string), and the vector it's found in.
#'
#' @param variable group vector, same as what is in your color/fill aesthetic. Include as 'data$variable' format
#' @param highlight name of group of interest, as character string
#' @param color_main (optional) Specify the highlighted color as a hexcode string. Default is #009ccc (blue)
#' @param color_other (optional) Specify non-highlighted color. Default is #9daab3 (gray)
#'
#' @examples
#' ggplot(dplyr::filter(transit_ridership, year=="2019"),
#'        aes(x = system, y = ridership, fill = system)) +
#'    geom_col() +
#'    cmap_fill_highlight(transit_ridership$system, "metra")
#'
#' @describeIn cmap_fill_highlight For fill aesthetic
#' @export
cmap_fill_highlight <- function(variable, highlight,
                                color_main = "#009ccc",
                                color_other = "#9daab3") {

  palette <- make_highlight_palette(variable, highlight, color_main, color_other)

  ggplot2::scale_fill_manual(values = palette)

}

#' @describeIn cmap_fill_highlight For color aesthetic
#' @export
cmap_color_highlight <- function(variable, highlight,
                                 color_main = "#009ccc",
                                 color_other = "#9daab3") {

  palette <- make_highlight_palette(variable, highlight, color_main, color_other)

  ggplot2::scale_color_manual(values = palette)

}

#' @describeIn cmap_fill_highlight For color aesthetic
#' @export
cmap_colour_highlight <- cmap_color_highlight
