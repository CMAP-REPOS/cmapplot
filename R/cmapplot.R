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

#'cmapplot global variables
#'
#'A list of predefined variables for use by the cmapplot package and its users.
#'It includes commonly used colors, font and font size specifications, and a
#'list of constants which aid in drawing cmap-themed plots.
#'
#'@section Plot Constants: The only portion of these global variables of
#'  interest to the user is \code{cmapplot_globals$consts}, a list of default
#'  constants that set certain plot aesthetics. Units of all plot constants are
#'  "bigpts": 1/72 of an inch. Most plot constants are invoked (and can be
#'  overridden) in \code{\link{finalize_plot}}: these are marked below with an
#'  \strong{F}. Some are used/can be overridden in \code{\link{theme_cmap}}:
#'  these are marked with \strong{T}.
#'
#'  \itemize{ \item \code{lwd_strongline}: This stronger-width line is drawn
#'  vertically or horizontally with the \code{hline, vline} args of
#'  \code{theme_cmap()}. \strong{(T)} \item \code{lwd_gridline}: This
#'  thinner-width line is drawn vertically or horizontally with the
#'  \code{gridlines, axislines} args of \code{theme_cmap()}. \strong{(T)} \item
#'  \code{lwd_plotline}: The width of any lines drawn by geoms in the plot (e.g.
#'  \code{geom_line}) but not explicitly sized by the geom's aesthetic.
#'  Implemented by \code{finalize_plot} or by \code{apply_cmap_default_aes} but
#'  not overridable in either context. (Modify by setting the size explicitly in
#'  the geom, but see \code{gg_lwd_convert} first.) \item \code{lwd_topline}:
#'  The width of the line above the plot. \strong{(F)} \item
#'  \code{length_ticks}: The length of the axis ticks (if shown). \strong{(T)}
#'  \item \code{margin_topline_t}: The margin between the top edge of the image
#'  and the top line. \strong{(F)} \item \code{margin_title_t}: The margin
#'  between the top line and the title. \strong{(F)} \item
#'  \code{margin_title_b}: The margin between the title and the caption when
#'  both are drawn in the sidebar. \strong{(F)} \item \code{margin_caption_b}:
#'  The margin between the bottom of the caption and the bottom edge of the
#'  image. \strong{(F)} \item \code{margin_legend_t}: The margin between the top
#'  line and the plot box (i.e., the top of the legend). \strong{(F)} \item
#'  \code{margin_legend_i}: The margin between legends (this only applies in
#'  plots with two or more legends and does not affect legend spacing on plots
#'  with single legends that have multiple rows). \strong{(T, F)} \item
#'  \code{margin_legend_b}: The margin between the bottom of the legend and the
#'  rest of the plot. \strong{(T, F)} \item \code{margin_plot_b}: The margin
#'  between the bottom of the plot and the bottom edge of the image (or top of
#'  caption). \strong{(F)} \item \code{margin_sidebar_l}: The margin between the
#'  left edge of the image and the title and caption, when the sidebar exists.
#'  Deducted from \code{title_width}. \strong{(F)} \item \code{margin_plot_l}:
#'  The margin between the left edge of the plot and the sidebar. \strong{(F)}
#'  \item \code{margin_plot_r}: The margin between the right edge of the plot
#'  and the edge of the image. \strong{(F)} \item \code{margin_panel_r}: Padding
#'  between the plot and its right-hand drawing extent. Override this based on
#'  space needed for x axis labels. \strong{(T)} \item \code{leading_title}:
#'  Text leading for Title text. \strong{(F)} \item \code{leading_caption}: Text
#'  leading for Caption text. \strong{(F)} }
#'
#'@export
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
    strong = list(family = "Arial", face = "bold"),
    regular = list(family = "Arial", face = "plain"),
    light = list(family = "Arial", face = "plain")
  ),
  use_whitney = FALSE,

  ## Establish plotting constants in bigpts (1/72 of inch)
  consts = list(
    lwd_gridline = 0.3,
    lwd_strongline = 1,
    lwd_plotline = 3,
    lwd_topline = 2,
    length_ticks = 7,
    margin_topline_t = 5,
    margin_title_t = 5,
    margin_title_b = 5,
    margin_caption_b = 5,
    margin_legend_t = 5,
    margin_legend_i = 8,
    margin_legend_b = 10,
    margin_plot_b = 5,
    margin_sidebar_l = 2,
    margin_plot_l = 10,
    margin_plot_r = 10,
    margin_panel_r = 10,
    leading_title = 1,
    leading_caption = 1
  ),

  # list of geoms whose aesthetics will be customized
  geoms_that_change = c(
    "Label",
    "Line",
    "Text",
    "TextLast",
    "PointLast",
    "RecessionsText"
  ),

  # empty location for loading in preferred aesthetics during `.onLoad`
  default_aes_cmap = NULL,

  # empty location for caching existing aesthetics during `.onLoad`
  default_aes_cached = NULL

)


## Update fonts based on system -- *must* be done with .onLoad()
.onLoad <- function(...) {

  # Check for Whitney
  all_fonts <- sysfonts::font_files()
  whitney_fonts <- all_fonts[all_fonts$family %in% c("Whitney Medium", "Whitney Book", "Whitney Semibold") & all_fonts$face=="Regular", ]
  cmapplot_globals$use_whitney <<- length(whitney_fonts$family) >= 3

  # Font handling for Windows users
  if (.Platform$OS.type == "windows") {

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
        strong = list(family = "Whitney Semibold", face = "plain"),
        regular = list(family = "Whitney Medium", face = "plain"),
        light = list(family = "Whitney Book", face = "plain")
      )

    # Otherwise, use Calibri
    } else {
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
        strong = list(family = "Calibri", face = "bold"),
        regular = list(family = "Calibri", face = "plain"),
        light = list(family = "Calibri Light", face = "plain")
      )
    }

  # Font handling for macOS/Linux/Unix
  } else {

    # Use Whitney if available
    if (cmapplot_globals$use_whitney) {
      # Add fonts to R
      grDevices::X11Fonts(
        `Whitney Medium` = grDevices::X11Font("-*-whitney-medium-%s-*-*-%d-*-*-*-*-*-*-*"),
        `Whitney Book` = grDevices::X11Font("-*-whitney-book-%s-*-*-%d-*-*-*-*-*-*-*"),
        `Whitney Semibold` = grDevices::X11Font("-*-whitney-semibold-%s-*-*-%d-*-*-*-*-*-*-*")
      )

      # Update font variables
      cmapplot_globals$font <<- list(
        strong = list(family = "Whitney Semibold", face = "plain"),
        regular = list(family = "Whitney Medium", face = "plain"),
        light = list(family = "Whitney Book", face = "plain")
      )

    # Otherwise, stick to Arial (set prior to .onLoad())
    } else {
      packageStartupMessage(
        "WARNING: Whitney is not installed on this system, so CMAP theme will default to Arial"
      )
    }
  }

  # Load CMAP preferred default.aes (can't be done until fonts are specified)
  cmapplot_globals$default_aes_cmap <<- init_cmap_default_aes()

  # Cache existing default.aes
  cmapplot_globals$default_aes_cached <<- fetch_current_default_aes()
}


# Font spec visualization helper function ---------------------------------

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
#' grid::grid.lines(y = 0.4,
#'                  gp = grid::gpar(col = "blue", lwd = 6 / .lwd))
#'
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
