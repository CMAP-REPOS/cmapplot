#' Highlight one discrete group in a comparison graph
#'
#' Pick the function depending on the aesthetic of your ggplot object (fill or
#' color). Specify the unique factor name of the group you're highlighting (as a
#' case-sensitive string), and the vector it's found in.
#'
#' This function does not make any modifications to the legend, so legend
#' behavior is not perfect out-of-the-box. For example, if the plot aesthetic
#' differentiates between five unique values, all five values will appear in the
#' legend, even though four use the same color. You will likely want to hide the
#' legend altogether, or manufacture a new data field that contains only the
#' value(s) to highlight and some generic "Other" label.
#'
#' @param field character vector, the vector in which the value to highlight is
#'   found. Values need not be unique. Typically, pass the table column that
#'   defines the color/fill aesthetic as 'table$field'
#' @param value character string, the name of group to highlight
#' @param color_value Specify the highlighted color. Default is #009ccc (blue)
#' @param color_other Specify non-highlighted color. Default is #9daab3 (gray)
#'
#' @examples
#' ggplot(dplyr::filter(transit_ridership, year=="2019"),
#'        aes(x = system, y = ridership, fill = system)) +
#'    geom_col() +
#'    cmap_fill_highlight(transit_ridership$system, "metra")
#'
#' @describeIn cmap_fill_highlight For fill aesthetic
#' @export
cmap_fill_highlight <- function(field, value,
                                color_value = "#009ccc",
                                color_other = "#9daab3") {

  palette <- make_highlight_palette(field, value, color_value, color_other)

  ggplot2::scale_fill_manual(values = palette)

}

#' @describeIn cmap_fill_highlight For color aesthetic
#' @export
cmap_color_highlight <- function(field, value,
                                 color_value = "#009ccc",
                                 color_other = "#9daab3") {

  palette <- make_highlight_palette(field, value, color_value, color_other)

  ggplot2::scale_color_manual(values = palette)

}

#' @describeIn cmap_fill_highlight For color aesthetic, if you're British
#' @export
cmap_colour_highlight <- cmap_color_highlight



#' Highlight palette prep function
#'
#' Create named list to serve as palette input for following fill/color highlight functions
#'
#' @param field group vector
#' @param value name of group of interest
#' @param color_value hexcode of highlighted color
#' @param color_other hexcode of non-highlighted color
#'
#' @noRd
make_highlight_palette <- function(field, value, color_value, color_other){

  n <- length(unique(field))

  palette <- rep.int(c(other = color_other), n)

  names(palette) <- levels(factor(field))

  palette[[value]] <- color_value

  return(palette)

}
