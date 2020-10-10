#' cmapplot
#'
#' This package contains extra palettes, themes and geoms for \pkg{ggplot2}.
#'
#' Please see the help pages listed below:
#'
#' \itemize{
#'   \item \code{\link{geom_text_lastonly}}
#'   \item \code{\link{theme_cmap}}
#' }
#'
#' Please report issues and suggest improvements at Github:
#'
#' \url{https://github.com/CMAP-REPOS/cmapplot}
#'
#' @name cmapplot
#' @docType package
#' @import ggplot2 dplyr grid scales grDevices graphics rlang gridtext
#' @importFrom gridExtra arrangeGrob
#' @importFrom ggpubr get_legend
#' @importFrom glue glue glue_collapse
#' @importFrom lubridate date_decimal
#' @importFrom sysfonts font_files
#' @keywords internal
"_PACKAGE"

cmapplot_globals <- new.env(parent = emptyenv())  # An environment for storing any global variables


# Default font handling ---------------------------------------------------

if (.Platform$OS.type == "windows") {

  # Set CMAP fonts: use Whitney if installed, Calibri otherwise
  all_fonts <- sysfonts::font_files()
  whitney_fonts <- dplyr::filter(all_fonts,
    family %in% c("Whitney Medium", "Whitney Book", "Whitney Semibold") & face == "Regular"
  )
  cmapplot_globals$use_whitney = length(whitney_fonts$family) == 3
  rm(all_fonts)
  rm(whitney_fonts)

  if (cmapplot_globals$use_whitney) {
    grDevices::windowsFonts(
      `Whitney Medium` = grDevices::windowsFont("Whitney Medium"),
      `Whitney Book` = grDevices::windowsFont("Whitney Book"),
      `Whitney Semibold` = grDevices::windowsFont("Whitney Semibold")
    )
    cmapplot_globals$font = list(
      main = list(family="Whitney Medium", face="plain", size=14),
      note = list(family="Whitney Book", face="plain", size=11),
      title = list(family="Whitney Semibold", face="plain", size=17),
      label = list(family="Whitney Semibold", face="plain", size=14)
    )
  } else {
    message("WARNING: Whitney is not installed on this PC, so CMAP theme will default to Calibri")
    grDevices::windowsFonts(
      `Calibri` = grDevices::windowsFont("Calibri"),
      `Calibri Light` = grDevices::windowsFont("Calibri Light")
    )
    cmapplot_globals$font = list(
      main = list(family="Calibri", face="plain", size=14),
      note = list(family="Calibri Light", face="plain", size=11),
      title = list(family="Calibri", face="bold", size=17),
      label = list(family="Calibri", face="bold", size=14)
    )
  }

} else {

  # Assume no Whitney or Calibri on non-Windows (i.e. non-work) computers
  message("WARNING: CMAP theme will default to Arial on non-Windows platforms")
  cmapplot_globals$use_whitney = FALSE
  cmapplot_globals$font = list(
    main = list(family="Arial", face="plain", size=14),
    note = list(family="Arial", face="plain", size=11),
    title = list(family="Arial", face="bold", size=17),
    label = list(family="Arial", face="bold", size=14)
  )

}

check_cmap_fonts <- function() {
  graphics::plot(c(0,2), c(0,5), type="n", xlab="", ylab="")
  graphics::par(family=cmapplot_globals$font$note$family,
                font=ifelse(cmapplot_globals$font$note$face == "bold", 2, 1))
  graphics::text(1, 4, paste("Note:", cmapplot_globals$font$note$family), cex=4)
  graphics::par(family=cmapplot_globals$font$main$family,
                font=ifelse(cmapplot_globals$font$main$face == "bold", 2, 1))
  graphics::text(1, 3, paste("Main:", cmapplot_globals$font$main$family), cex=4)
  graphics::par(family=cmapplot_globals$font$title$family,
                font=ifelse(cmapplot_globals$font$title$face == "bold", 2, 1))
  graphics::text(1, 2, paste("Title:", cmapplot_globals$font$title$family), cex=4)
  graphics::par(family=cmapplot_globals$font$label$family,
                font=ifelse(cmapplot_globals$font$label$face == "bold", 2, 1))
  graphics::text(1, 1, paste("Label:", cmapplot_globals$font$label$family), cex=4)
}
#check_cmap_fonts()


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

# Establish plotting constants in bigpts (1/72 of inch)
# [t] and [f] in comments indicate whether the constant is called in
# [t]heme_cmap() and/or [f]inalize_plot().
cmapplot_globals$consts <- list(
  lwd_originline = 1.6, # [t, ] Width of origin lines. This appears to be the
                        #        minimum for variation between origin lines and
                        #        other background lines
  lwd_gridline = 0.3,   # [t, ] Width of grid lines (non-origin)
  lwd_plotline = 3,     # [ ,f] Width of line graph lines
  lwd_topline = 2,      # [ ,f] Width of top line
  margin_topline_t = 5, # [ ,f] Margin between top edge of image and top line
  margin_title_t = 5,   # [ ,f] Margin between top line and title
  margin_title_b = 5,   # [ ,f] Margin between title and caption if
                        #        `caption_valign = "top"`
  margin_caption_b = 5, # [ ,f] Margin between caption and bottom edge of image
  margin_legend_t = 5,  # [ ,f] Margin between top line and plotbox
  margin_legend_i = 5,  # [t,f] Vertical margin between legends (only applies
                        #        to multilegend plots)
  margin_legend_b = 10, # [t,f] Margin between legend and plot (within plotbox)
  margin_plot_b = 0,    # [ ,f] Margin between plotbox and bottom edge of image
  margin_title_l = 2,   # [ ,f] Margin between left edge of image and title/caption
  margin_title_r = 10,  # [ ,f] Margin between title/caption and legend/plot
                        #        (Both title_l and title_r are deducted from `titlewidth`)
  margin_plot_r = 10,   # [ ,f] Margin between plotbox and right edge of image
  padding_plot = c(0,10,0,0),
                        # [t,f] Padding around the entire plot
  padding_legend = c(0,1,0,-5),
                        # [t,f] Padding for the legend element. The -9 adjustment is
                        #        necessary to left align legend key with plot elements
  leading_title = 0.93, # [ ,f] Text leading for Title text
  leading_caption = 1  #  [ ,f] Text leading for Caption text
)

# Define common colors
cmapplot_globals$colors <- list(
  blackish = "#222222"
)
