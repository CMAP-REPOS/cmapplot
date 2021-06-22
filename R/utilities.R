#' Font visualization test
#'
#' This internal function uses base R graphics to display the five text variants
#' that should show up on a CMAP themed graphic - and what fonts the package is
#' planning to use to display them.
#'
#' @noRd
display_cmap_fonts <- function() {
  graphics::plot(c(0,2), c(0,6), type="n", xlab="", ylab="")

  draw.me <- function(name, font, size, placement){
    thisfont <- cmapplot_globals$font[[font]]
    thissize <- cmapplot_globals$fsize[[size]]

    graphics::par(family=thisfont$family,
                  font=ifelse(thisfont$face == "bold", 2, 1))
    graphics::text(1, placement,
                   paste(name,
                         paste(paste("font:", font), paste("size:", size), sep = ", "),
                         paste(thisfont$family, thisfont$face, thissize, sep = ", "),
                         sep = " | "),
                   cex=thissize/12, ps=12)
  }

  draw.me(name = "Title", font = "strong",  size = "L", 5)
  draw.me(name = "Main",  font = "regular", size = "M", 4)
  draw.me(name = "Axis",  font = "light",   size = "M", 3)
  draw.me(name = "Label", font = "strong",  size = "M", 2)
  draw.me(name = "Note",  font = "light",   size = "S", 1)
}



# Plot sizes and colors ---------------------------------------------------

#' Line width conversion
#'
#' The factor \code{.lwd} is used to calculate correct output sizes for line
#' widths. For line widths in \code{ggplot2}, the size in mm must be divided
#' by this factor for correct output. Because the user is likely to prefer
#' other units besides for mm, \code{gg_lwd_convert()} is provided as a
#' convenience function, converting from any unit all the way to ggplot units.
#'
#' \code{.lwd} is equal to \code{ggplot2::.stroke / ggplot2::.pt}. In
#' \code{ggplot2}, the size in mm is divided by \code{.lwd} to achieve the
#' correct output. In the \code{grid} package, however, the size in points
#' (\code{pts} (or maybe \code{bigpts}? Unclear.) must be divided by
#' \code{.lwd}. The user is unlikely to interact directly with \code{grid},
#' but this is how \code{finalize_plot()} does its work.
#'
#' This is closely related to \code{ggplot::.pt}, which is the factor that
#' font sizes (in \code{pts}) must be divided by for text geoms within
#' \code{ggplot2}. Confusingly, \code{.pt} is not required for \code{ggplot2}
#' font sizes outside the plot area: e.g. axis titles, etc.
#'
#' @seealso grid's \code{\link[grid]{unit}}, ggplot2's
#'   \code{\link[ggplot2]{.pt}}, and
#'   \url{https://stackoverflow.com/questions/17311917/ggplot2-the-unit-of-size}
#'
#' @examples
#' ggplot() + coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3)) +
#'
#'   # a green line 3 points wide
#'   geom_hline(yintercept = 1, color = "green", size = gg_lwd_convert(3)) +
#'
#'   # black text of size 24 points
#'   annotate("text", -2, 0, label = "text", size = 24/ggplot2::.pt)
#'
#'
#' # a blue line 6 points wide, drawn over the plot with  the `grid` package
#' grid::grid.lines(y = 0.4, gp = grid::gpar(col = "blue", lwd = 6 / .lwd))
#'
#' @export
.lwd <- ggplot2::.pt / ggplot2::.stroke


#' Helper function to calculate correct size for ggplot line widths.
#'
#' @param value Numeric, the value to be converted.
#' @param unit Char, the unit of the value to be converted. Can be any of the
#'   units accepted by \code{grid::unit()}, including "bigpts", "pt", "mm", and
#'   "in". Default is \code{bigpts}.
#'
#' @describeIn dot-lwd Function to convert from any unit directly to ggplot2's
#'   preferred millimeters.
#'
#' @export
gg_lwd_convert <- function(value, unit = "bigpts") {

  # convert input type to mm
  value_out <- grid::convertUnit(grid::unit(value, unit), "mm", valueOnly = TRUE)

  # return with conversion factor
  return(
    value_out / .lwd
  )
}



#' Identify correct font path based on filename
#'
#' Taking a list of font paths, search for a specific filename. If a perfect
#' match is found, return that path.
#'
#' @param filename the complete file name, less a .otf or .ttf extension.
#' @param path a vector of filepaths
#'
#' @noRd
find_path <- function(filename, paths){
  result <- grep(paste0("(\\\\|/)", filename, ".[ot]tf$"), paths, value = TRUE)

  if(length(result) >= 1){
    return(result[1])
  } else {
    stop(
      paste0("Font '", filename, "' not found."),
         call. = FALSE)
  }
}


#' Sub-fn to safely intepret grobHeight
#'
#' This returns the height of a grob in any real unit. If the value passed in is
#' null, it returns 0. It is used in various places in `finalize_plot`.
#'
#' @noRd
safe_grobHeight <- function(grob, unitTo = "bigpts", valueOnly = TRUE){

  if(is.null(grob)){
    if(valueOnly){
      return(0)
    } else {
      return(unit(0, unitTo))
    }
  }

  return(grid::convertHeight(grid::grobHeight(grob), unitTo, valueOnly))
}


#' Palette Fetcher
#'
#' @param which a vector of palette types to consider
#' @param return Value to return. "colors", the default, returns the palette as
#'   a vector of colors. "type" returns the palette's type. "Exists" returns
#'   TRUE or FALSE based on whether the name is found in the palettes table.
#'
#' @describeIn viz_palette Returns details about a palette
#'
#' @examples
#' # Identify the first two colors of the Prosperity Palette
#' fetch_pal("prosperity")[1:2]
#'
#' # Confirm that "reds" is a sequential palette
#' fetch_pal("reds", which = "sequential", return = "exists")
#'
#' @export
fetch_pal <- function(pal,
                      which = c("discrete", "sequential", "divergent"), #unique(cmapplot_globals$palettes$type),
                      return = c("colors", "type", "exists")){
  # basics
  name <- type <- NULL
  return <- match.arg(return)

  # filter palettes
  df <- dplyr::filter(
    cmapplot_globals$palettes,
    name == pal,
    type %in% which
  )

  # construct return
  if (return == "exists"){
    return(nrow(df)==1)
  } else if (nrow(df)==1){
    return(df[[return]][[1]])
  } else {
    return(NULL)
  }
}
