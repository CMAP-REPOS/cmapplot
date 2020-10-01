#' Highlight palette prep function
#'
#' Create named list to serve as palette input for following fill/color highlight functions
#'
make_highlight_palette <- function(variable, highlight, color = "#009ccc"){

  n <- length(unique(variable))

  palette <- rep.int(c(other = "#9daab3"), n)

  names(palette) <- levels(factor(variable))

  palette[[highlight]] <- color

  return(palette)

}

#' Highlight one discrete group in a comparison graph
#'
#' Pick the function depending on the aesthetic of your ggplot object (fill or color).
#' Specify the unique factor name of the group you're highlighting (as a case-sensitive string), and the vector it's found in.
#'
#' @param variable group vector, same as what is in your color/fill aesthetic. Include as 'data$variable' format
#' @param highlight name of group of interest, as character string
#' @param color (optional) Specify the highlighted color as a hexcode string. Default is #009ccc (blue)
#'
#' @examples
#' ggplot(dplyr::filter(transit_ridership, year=="2019"),
#'        aes(x = system, y = ridership, fill = system)) +
#'    geom_col() +
#'    cmap_fill_highlight(transit_ridership$system, "metra")
#'
#' @export
cmap_fill_highlight <- function(variable, highlight, color = "#009ccc") {

  palette <- make_highlight_palette(variable, highlight, color)

  ggplot2::scale_fill_manual(values = palette)

}

#' @rdname cmap_fill_highlight
#' @export
cmap_color_highlight <- function(variable, highlight, color = "#009ccc") {

  palette <- make_highlight_palette(variable, highlight, color)

  ggplot2::scale_color_manual(values = palette)

}

#' @rdname cmap_fill_highlight
#' @export
cmap_colour_highlight <- cmap_color_highlight
