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

#' cmapplot global variables
#'
#' Expose a list of predefined variables for use by the cmapplot package and its
#' users. List includes commonly used colors, font and font size specifications,
#' and a list of constants which aid in drawing cmap-themed plots.
#'
#'@section Plot Constants: \code{cmapplot_globals$consts} contains a list of
#'  default constants that set certain plot aesthetics. Units of all plot
#'  constants are "bigpts": 1/72 of an inch. Most plot constants are invoked
#'  (and can be overridden) in \code{\link{finalize_plot}}: these are marked
#'  \strong{F}. Some are used/can be overridden in \code{\link{theme_cmap}}:
#'  these are marked with \strong{T}.
#'
#'  \itemize{
#'    \item \code{lwd_originline}: This stronger-width line is drawn vertically or
#'    horizontally with the \code{hline, vline} args of \code{theme_cmap()}. \strong{(T)}
#'    \item \code{lwd_gridline}: This thinner-width line is drawn vertically or
#'    horizontally with the \code{gridlines, axislines} args of \code{theme_cmap()}. \strong{(T)}
#'    \item \code{lwd_plotline}: The width of any lines drawn by geoms in the plot
#'    (e.g. \code{geom_line}) but not explicitly sized by the geom's aesthetic. Due to
#'    limitations in ggplot2's construction, this does not apply to
#'    \code{finalize_plot(mode = "object")} outputs. \strong{(F)}
#'    \item \code{lwd_topline}: The width of the line above the plot and title. \strong{(F)}
#'    \item \code{margin_topline_t}: The margin between the top edge of the
#'    image and the top line. \strong{(F)}
#'    \item \code{margin_title_t}: The margin between the top line and the
#'    title. \strong{(F)}
#'    \item \code{margin_title_b}: The margin between the title and the caption. \strong{(F)}
#'    \item \code{margin_caption_b}: The margin between the bottom of the \strong{(F)}
#'    caption and the bottom edge of the image.
#'    \item \code{margin_legend_t}: The margin between the top line and the
#'    plot box (i.e., the top of the legend). \strong{(F)}
#'    \item \code{margin_legend_i}: The margin between legends (this only
#'    applies in plots with two or more legends and does not affect legend
#'    spacing on plots with single legends that have multiple rows). \strong{(T, F)}
#'    \item \code{margin_legend_b}: The margin between the bottom of the legend
#'    and the rest of the plot. \strong{(T, F)}
#'    \item \code{margin_plot_b}: The margin between the bottom of the plot and
#'    the bottom edge of the image. \strong{(F)}
#'    \item \code{margin_title_l}: The margin between the left edge of the image
#'    and the title. This also applies to the caption. Deducted from
#'    \code{title_width}. \strong{(F)}
#'    \item \code{margin_title_r}: The margin between the right edge of the
#'    image and the title. This also applies to the caption. Deducted from
#'    \code{title_width}. \strong{(F)}
#'    \item \code{margin_plot_r}: The margin between the right edge of the plot
#'    and the edge of the image. \strong{(F)}
#'    \item \code{margin_panel_r}: Padding between the plot and its right-hand drawing
#'    extent. Override this based on space needed for x axis labels. \strong{(T)}
#'    \item \code{leading_title}: Text leading for Title text.
#'    \item \code{leading_caption}: Text leading for Caption text.
#'  }
#'
#' @export
cmapplot_globals <- list(

  ## Colors
  colors = list(
    blackish = "#222222"
  ),

  ## Font sizes
  fsize = list(
    S = 11,
    M = 14,
    L = 17
  ),

  ## Base typefaces -- modified later by .onLoad()
  font = list(
    strong = list(family="Arial", face="bold"),
    regular = list(family="Arial", face="plain"),
    light = list(family="Arial", face="plain")
  ),
  use_whitney = FALSE,

  ## Establish plotting constants in bigpts (1/72 of inch)
  consts = list(
    lwd_originline = 1,
    lwd_gridline = 0.3,
    lwd_plotline = 3,
    lwd_topline = 2,
    margin_topline_t = 5,
    margin_title_t = 5,
    margin_title_b = 5,
    margin_caption_b = 5,
    margin_legend_t = 5,
    margin_legend_i = 8,
    margin_legend_b = 10,
    margin_plot_b = 5,
    margin_title_l = 2,
    margin_title_r = 10,
    margin_plot_r = 10,
    margin_panel_r = 20,
    leading_title = 1,
    leading_caption = 1
  )
)

## Use Whitney or Calibri if on Windows -- *must* be done with .onLoad()
.onLoad <- function(...) {
  if (.Platform$OS.type == "windows") {

    # Check for Whitney
    all_fonts <- sysfonts::font_files()
    whitney_fonts <- all_fonts[all_fonts$family %in% c("Whitney Medium", "Whitney Book", "Whitney Semibold") & all_fonts$face=="Regular", ]
    cmapplot_globals$use_whitney = length(whitney_fonts$family) == 3

    # Use Whitney if available
    if (cmapplot_globals$use_whitney) {
      # Add fonts to R
      grDevices::windowsFonts(
        `Whitney Medium` = grDevices::windowsFont("Whitney Medium"),
        `Whitney Book` = grDevices::windowsFont("Whitney Book"),
        `Whitney Semibold` = grDevices::windowsFont("Whitney Semibold")
      )

      # Update font variables
      cmapplot_globals$font <<- list(
        strong = list(family = "Whitney Semibold", face="plain"),
        regular = list(family="Whitney Medium", face="plain"),
        light = list(family="Whitney Book", face="plain")
      )

    } else {
      # Otherwise, use Calibri
      packageStartupMessage(
        "WARNING: Whitney is not installed on this PC, so CMAP theme will default to Calibri"
      )
      # Add fonts to R
      grDevices::windowsFonts(
        `Calibri` = grDevices::windowsFont("Calibri"),
        `Calibri Light` = grDevices::windowsFont("Calibri Light")
      )

      # Update font variables
      cmapplot_globals$font <<- list(
        strong = list(family="Calibri", face="bold"),
        regular = list(family="Calibri", face="plain"),
        light = list(family="Calibri Light", face="plain")
      )
    }

  } else {
    # If non-Windows machine, stick to Arial
    packageStartupMessage(
      "WARNING: CMAP theme will default to Arial on non-Windows platforms"
    )
  }
}




# Define an helper function to visualize the font specifications
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
ggplot_size_conversion <- function(value, type = "bigpts",isFont) {
  # convert input type to mm (if not already)
  value_in_mm <- grid::convertUnit(grid::unit(value, type), "mm", valueOnly = TRUE)

  return(
    value_in_mm
      * ggplot2::.stroke # Multiply by units for R pixels per mm (=96/25.4)
      / ggplot2::.pt     # Account for the ggplot2::.pt factor (=72.27/25.4)
  )
}


