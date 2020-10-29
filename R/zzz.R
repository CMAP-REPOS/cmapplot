.onLoad <- function(...) {
  ## Create cmapplot_globals environment
  cmapplot_globals <- new.env(parent = emptyenv())  # An environment for storing any global variables


  ## Colors
  cmapplot_globals$colors <- list(
    blackish = "#222222"
  )


  ## Font sizes
  cmapplot_globals$fsize <- list(
    big = 17,
    reg = 14,
    sml = 11
  )


  ## Typefaces
  if (.Platform$OS.type == "windows") {

    # Check for Whitney
    all_fonts <- sysfonts::font_files()
    whitney_fonts <- dplyr::filter(
      all_fonts,
      family %in% c("Whitney Medium", "Whitney Book", "Whitney Semibold") & face == "Regular"
    )
    cmapplot_globals$use_whitney = length(whitney_fonts$family) == 3

    # Use Whitney if available
    if (cmapplot_globals$use_whitney) {
      # Add fonts to R
      grDevices::windowsFonts(
        `Whitney Medium` = grDevices::windowsFont("Whitney Medium"),
        `Whitney Book` = grDevices::windowsFont("Whitney Book"),
        `Whitney Semibold` = grDevices::windowsFont("Whitney Semibold")
      )

      # establish font variables
      cmapplot_globals$font = list(
        strong = list(family = "Whitney Semibold", face="plain"),
        regular = list(family="Whitney Medium", face="plain"),
        light = list(family="Whitney Book", face="plain")
      )

    # Otherwise, use Calibri
    } else {
      message("WARNING: Whitney is not installed on this PC, so CMAP theme will default to Calibri")
      # Add fonts to R
      grDevices::windowsFonts(
        `Calibri` = grDevices::windowsFont("Calibri"),
        `Calibri Light` = grDevices::windowsFont("Calibri Light")
      )

      # establish font variables
      cmapplot_globals$font = list(
        strong = list(family="Calibri", face="bold"),
        regular = list(family="Calibri", face="plain"),
        light = list(family="Calibri Light", face="plain")
      )
    }

  # If non-Windows machine, use Arial
  } else {
    message("WARNING: CMAP theme will default to Arial on non-Windows platforms")
    cmapplot_globals$use_whitney = FALSE

    # establish font variables
    cmapplot_globals$font = list(
      strong = list(family="Arial", face="bold"),
      regular = list(family="Arial", face="plain"),
      light = list(family="Arial", face="plain")
    )
  }


  ## Establish plotting constants in bigpts (1/72 of inch)
  ## [t] and [f] in comments indicate whether the constant is called in
  ## [t]heme_cmap() and/or [f]inalize_plot().
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
    margin_legend_i = 8,  # [t,f] Vertical margin between legends (only applies
    #        to multilegend plots)
    margin_legend_b = 10, # [t,f] Margin between legend and plot (within plotbox)
    margin_plot_b = 5,    # [ ,f] Margin between plotbox and bottom edge of image
    margin_title_l = 2,   # [ ,f] Margin between left edge of image and title/caption
    margin_title_r = 10,  # [ ,f] Margin between title/caption and legend/plot
    #        (Both title_l and title_r are deducted from `titlewidth`)
    margin_plot_r = 10,   # [ ,f] Margin between plotbox and right edge of image
    padding_plot = c(0,10,0,0),
    # [t,f] Padding around the entire plot
    padding_legend = c(0,0,0,0),
    # [t,f] Padding for the legend element. The -9.5
    #        adjustment is necessary to left align legend
    #        key with plot elements. Additional adjustments
    #        may be required based on system configuration,
    #        which can be done easily using `legend_bump`
    legend_key_size = 14, # [t,f] Size of the legend key item.
    leading_title = 1, # [ ,f] Text leading for Title text
    leading_caption = 1   # [ ,f] Text leading for Caption text
  )

}
