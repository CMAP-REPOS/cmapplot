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
#' users.
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
  ## [t] and [f] in comments indicate whether the constant is called in
  ## [t]heme_cmap() and/or [f]inalize_plot().
  consts = list(
    lwd_originline = 1.6, # [t, ] Width of origin lines. This appears to be the
                          #        minimum for variation between origin lines and
                          #        other background lines
    lwd_gridline = 0.3,   # [t, ] Width of most lines in plot theme, including any gridlines
    lwd_plotline = 3,     # [ ,f] Width of line graph lines
    lwd_topline = 2,      # [ ,f] Width of top line
    margin_topline_t = 5, # [ ,f] Margin between top edge of image and top line
    margin_title_t = 5,   # [ ,f] Margin between top line and title
    margin_title_b = 5,   # [ ,f] Margin between title and caption if
                          #        `caption_valign = "top"`
    margin_caption_b = 5, # [ ,f] Margin between caption and bottom edge of image
    margin_legend_t = 5,  # [ ,f] Margin between top line and plotbox
    margin_legend_i = 8,  # [t,f] Vertical margin between legends (only applies
                          #        to multilegend plots)
    margin_legend_b = 10, # [t,f] Margin between legend and plot (within plotbox)
    margin_plot_b = 5,    # [ ,f] Margin between plotbox and bottom edge of image
    margin_title_l = 2,   # [ ,f] Margin between left edge of image and title/caption
    margin_title_r = 10,  # [ ,f] Margin between title/caption and legend/plot
                          #        (Both title_l and title_r are deducted from `titlewidth`)
    margin_plot_r = 10,# [ ,f] Margin between plotbox and right edge of image
    margin_panel_r = 20,   # [t,f] Margin between the right side of the plot and plotbox edge
    padding_legend = c(0,0,0,0),
                          # [t,f]  Padding around the legend.
    leading_title = 1,    # [ ,f] Text leading for Title text
    leading_caption = 1   # [ ,f] Text leading for Caption text
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
        strong = list(family="Calibri", face="bold"),
        regular = list(family="Calibri", face="plain"),
        light = list(family="Calibri Light", face="plain")
      )
    }

    # If non-Windows machine, stick to Arial
  } else {
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
ggplot_size_conversion <- function(value, type = "bigpts") {
  # convert input type to bigpts (if not already)
  value_in_bigpts <- grid::convertUnit(grid::unit(value, type), "bigpts", valueOnly = TRUE)
  return(
    value_in_bigpts / 72 # Normalize from big points
      * 96               # Multiply by units for R pixels (per inch)
      / ggplot2::.pt     # Account for the ggplot2::.pt factor (=72.27/25.4)
  )
}





# temp plots ---------------------------------------------------
# some plots without themes for internal testing

econ_plot <- ggplot(data = cmapplot::cluster_jobchange,
                    mapping = aes(
                      x = reorder(name, jobchange),
                      y = jobchange,
                      fill = category,
                      alpha = assessment)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)
#
# transit_plot <- cmapplot::transit_ridership %>%
#   mutate(system = case_when(
#     system == "cta_bus" ~ "CTA (Bus)",
#     system == "cta_rail" ~ "CTA (Rail)",
#     system == "metra" ~ "Metra",
#     system == "pace" ~ "Pace",
#     system == "pace_ada" ~ "Paratransit"
#   )) %>%
#   ggplot(aes(x = year, y = ridership, color = system)) +
#   geom_line()
