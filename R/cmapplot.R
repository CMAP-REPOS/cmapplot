#' cmapplot
#'
#' This package contains extra palettes, themes and geoms for \pkg{ggplot2},
#' based on Chicago Metropolitan Agency for Planning (CMAP) design guidelines.
#'
#' Detailed documentation can be viewed at
#' \url{https://cmap-repos.github.io/cmapplot}.
#'
#' Please report issues and suggest improvements at
#' \url{https://github.com/CMAP-REPOS/cmapplot/issues}.
#'
#' @name cmapplot
#' @docType package
#' @import ggplot2 dplyr grid scales grDevices graphics rlang gridtext
#' @importFrom glue glue glue_collapse
#' @importFrom sysfonts font_files
#' @keywords internal
"_PACKAGE"

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

  draw.me(name = "Title", font = "strong",  size = "big", 5)
  draw.me(name = "Main",  font = "regular", size = "reg", 4)
  draw.me(name = "Axis",  font = "light",   size = "reg", 3)
  draw.me(name = "Label", font = "strong",  size = "reg", 2)
  draw.me(name = "Note",  font = "light",   size = "sml", 1)
}
#display_cmap_fonts()


# Plot sizes and colors ---------------------------------------------------

#' Helper function to calculate correct size for ggplot inputs. Takes two inputs:
#' a value (numeric) and a type (character). The type can be any of the units
#' accepted by `grid::unit()`, including "bigpt", "pt", "mm", and "in".
#'
#' @param value Numeric, the value to be converted.
#' @param type Char, the unit of the value to be converted.
#'
#' @return A unitless value in ggplot units
#'
#' @seealso <https://stackoverflow.com/questions/17311917/ggplot2-the-unit-of-size>
#' and [grid::unit()]
#'
#' @noRd
ggplot_size_conversion <- function(value, type = "bigpts") {
  # convert input type to bigpts (if not already)
  value_in_bigpts <- grid::convertUnit(grid::unit(value, type), "bigpts", valueOnly = TRUE)
  return(
    value_in_bigpts / 72 # Normalize from big points
      * 96               # Multiply by units for R pixels (per inch)
      / ggplot2::.pt     # Account for the ggplot2::.pt factor (=72.27/25.4)
  )
}
