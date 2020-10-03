#' Highlight one or more discrete groups in a comparison graph
#'
#' Pick the function depending on the aesthetic of your ggplot object (fill or
#' color). Specify the unique factor name(s) of the group(s) you're highlighting
#' (as a case-sensitive string), and the vector it's found in.
#'
#' You may specify multiple groups to highlight. If you do, you may specify a
#' single highlight color, or a vector of highlight colors of equal length to
#' the vector of highlight values.
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
#' @param value character string or vector, the name of group(s) to highlight
#' @param color_value Specify the highlight color(s). Default is #009ccc (blue)
#' @param color_other Specify non-highlighted color. Default is #9daab3 (gray)
#'
#' @examples
#' # one value, default colors
#' ggplot(dplyr::filter(transit_ridership, year=="2019"),
#'        aes(x = system, y = ridership, fill = system)) +
#'    geom_col() +
#'    cmap_fill_highlight(transit_ridership$system, "metra")
#'
#' # multiple values, default colors
#' ggplot(dplyr::filter(transit_ridership, year=="2019"),
#'        aes(x = system, y = ridership, fill = system)) +
#'    geom_col() +
#'    cmap_fill_highlight(transit_ridership$system, c("metra", "pace_ada"))
#'
#' # multiple values, multiple colors
#' ggplot(dplyr::filter(transit_ridership, year=="2019"),
#'        aes(x = system, y = ridership, fill = system)) +
#'    geom_col() +
#'    cmap_fill_highlight(transit_ridership$system,
#'    c("metra", "pace_ada"),
#'    c("red", "orange"))
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

  # check that value and color_value are the same length.
  # if color_value has a length of 1, it will be repeated
  if(length(value) > 1){
    if (length(color_value) == 1){
      color_value <- rep.int(color_value, length(value))
    } else if(length(value) != length(color_value)){
      stop("Length of `value` and `color_value` must be equal.", .call = FALSE)
    }
  }

  # identify palette length
  n <- length(unique(field))

  # construct initial palette
  palette <- rep.int(c(other = color_other), n)
  names(palette) <- levels(factor(field))

  # replace highlight value(s) with highlight color(s)
  for(i in seq_along(value)){
    palette[[value[i]]] <- color_value[i]
  }

  return(palette)

}
