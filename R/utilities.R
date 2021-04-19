# Font spec visualization helper function ---------------------------------

#' Font visualization test
#'
#' This internal function uses base R graphics to display the five text variants
#' that should show up on a cmap themed graphic - and what fonts the package is
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


#' Check for necessary Whitney fonts in systemfonts registry
#'
#' Invisibly returns TRUE or FALSE based on whether the three used Whitney fonts
#' are available in the systemfonts registry.
#'
#' @param set_global sets cmapplot_globals$use_whitney based on results
#'
#' @noRd
check_for_whitney_core <- function(set_global = FALSE){

  registry_fonts <- systemfonts::registry_fonts()

  whitney_core_present <- nrow(
    registry_fonts[registry_fonts$family %in% cmapplot_globals$preferred_font,]
    ) >= 3

  if(set_global){
    assign("use_whitney",
           whitney_core_present,
           envir = cmapplot_globals)
  }

  invisible(whitney_core_present)
}
