#'Arrange and save CMAP ggplot chart
#'
#'\code{finalize_plot} will place a ggplot into a frame defined by CMAP design
#'standards. It will align your title and caption to the left, add a horizontal
#'line on top, and make other adjustments. It can show you the final plot and/or
#'export it as a raster or vector file. This function will not apply CMAP design
#'standards to the plot itself: use with \code{theme_cmap()} for that.
#'
#'Exports from this function use Cairo graphics drivers, while drawing within R
#'is done with default (Windows) drivers. \code{mode = "object"} also returns a
#'gTree object that can be stored and drawn later with \code{grid::grid.draw()}.
#'Lines drawn by any \code{geom_line()} geoms without line widths explicitly
#'specified are assigned a thicker width (specifically,
#'\code{cmapplot_globals$plot_constants$lwd_plotline}) in all outputs except for
#'when exporting as an object.
#'
#'@param plot ggplot object, the variable name of the plot you have created that
#'  you want to finalize. If null (the default), the most recent plot will be
#'  retrieved via \code{ggplot2::last_plot()}.
#'@param title,caption Char, the text you want to appear in the title and
#'  caption blocks. If empty, any non-Null values from \code{plot} will be
#'  retrieved. These blocks take html formatting, so manual text breaks can be
#'  created with \code{<br>} and formatting can be changed with \code{<span>}.
#'@param width,height Numeric, the dimensions for the output image, including
#'  the title. Units in inches, which interacts with \code{ppi} to define the
#'  pixel dimensions of raster outputs. Default is 9.31 inches wide (670/72) and
#'  5.56 inches tall (400/72), to match Comms specification of 670px by 400px at
#'  72ppi.
#'@param title_width Numeric, the width in inches for the title. If unspecified,
#'  use 25 percent of the total output width (per Comms guidance).
#'@param ppi Numeric, the resolution of exported images (pixels per inch).
#'  Default = 300.
#'@param mode Vector, the action(s) to be taken with the plot. Save using any of
#'  the following: \code{png}, \code{tiff}, \code{jpeg}, \code{bmp}, \code{svg},
#'  \code{pdf}, \code{ps}. View in R with: \code{plot}, \code{window} (`window`
#'  currently works on computers running Windows). Return an object with
#'  \code{object}.
#'@param filename Char, the file path and name you want the plot to be saved to.
#'  You may specify an extension to use. If you don't, the correct extension
#'  will be added for you.
#'@param caption_valign Char, align the caption text at the top or the bottom of
#'  the available space between the title and bottom of image. This argument
#'  accepts abbreviations, too: \code{c("top", "t", "bottom", "b")}.
#'@param fill_bg,fill_canvas Char, strings that represent colors R can
#'  interpret. They are used to fill behind and around the finished plot,
#'  respectively.
#'@param overrides Named list, overrides the default drawing attributes defined
#'  in \code{cmapplot_globals$plot_constants} which are drawn by
#'  \code{finalize_plot()} (this is most of them). Units are in bigpts (1/72 of
#'  an inch).
#'@param debug Bool, TRUE enables outlines around components of finalized plot.
#'  Default = FALSE.
#'@param legend_build Char, how the function attempts to build the legend.
#'  \code{"adjust"}, the default, attempts to align the legend all the way left
#'  (on top of the y axis labels) per CMAP design standards. \code{"safe"}
#'  aligns the legend left, but not over the y axis labels: less ideal, but less
#'  buggy. \code{"none"} removes the legend entirely.
#'@param ... pass additional arguments to \code{ggplot2::theme()} to override any
#'  elements of the default CMAP theme.
#'
#'@return If and only if \code{"object"} is one of the modes specified, a gTree
#'  object is returned. gTree is an assembly of grobs, or graphical objects,
#'  that can be drawn using the grid package.
#'
#'@importFrom utils modifyList
#'@importFrom generics intersect
#'
#'@examples
#' \dontrun{
#' econ_plot <- ggplot(data = cluster_jobchange,
#'                     mapping = aes(
#'                       x = reorder(name, jobchange),
#'                       y = jobchange,
#'                       fill = category)) +
#'   geom_col() +
#'   coord_flip() +
#'   theme_cmap(gridlines = "v", hline = 0) +
#'   scale_y_continuous(labels = scales::comma)
#'
#' finalize_plot(econ_plot,
#'                "Cluster-level employment changes in the Chicago MSA, 2001-17",
#'                "Source: Chicago Metropolitan Agency for Planning analysis",
#'                mode = "window",
#'                height = 6,
#'                width = 8,
#'                title_width = 2.5,
#'                overrides = list(margin_h3 = 30))
#'
#' transit_plot <- transit_ridership %>%
#'   mutate(system = case_when(
#'     system == "cta_bus" ~ "CTA (Bus)",
#'     system == "cta_rail" ~ "CTA (Rail)",
#'     system == "metra" ~ "Metra",
#'     system == "pace" ~ "Pace",
#'     system == "pace_ada" ~ "Paratransit"
#'   )) %>%
#'   ggplot(aes(x = year, y = ridership, color = system)) +
#'   geom_line() +
#'   theme_cmap(legend.max.columns = 3)
#'
#' finalize_plot(transit_plot,
#'               "Transit ridership in the RTA region over time, 1980-2019
#'               (in millions).",
#'               "Source: Chicago Metropolitan Agency for Planning
#'               analysis of data from the Regional Transportation Authority.",
#'               mode=c("plot", "pdf"),
#'               filename = "foo")
#'}
#'@export
finalize_plot <- function(plot = NULL,
                          title = "",
                          caption = "",
                          width = 670/72, # comms spec: 670px @ 72ppi
                          height = 400/72, # comms spec: 400px @ 72ppi
                          title_width = NULL, # if unspecified, default to width/4
                          ppi = 300,
                          mode = c("plot"),
                          filename = "",
                          caption_valign = c("top", "bottom"),
                          fill_bg = "white",
                          fill_canvas = "gray90",
                          overrides = list(),
                          debug = FALSE,
                          legend_build = c("adjust", "safe", "none"),
                          ...
                          ){

  # Validation and initialization -----------------------------

  # Seek last plot if user did not specify one
  if (is.null(plot)) {
    plot <- ggplot2::last_plot()
  }

  # Set title_width to 25% of total width if unspecified
  if (is.null(title_width)) {
    title_width <- width / 4
  }

  # check args with default vectors
  caption_valign <- match.arg(caption_valign)
  legend_build <- match.arg(legend_build)

  # check mode argument and validate filename
  savetypes_raster <- c("png","tiff","jpeg","bmp")
  savetypes_vector <- c("svg","ps","pdf")
  savetypes_print <- c("plot", "window")

  mode <- match.arg(arg = mode,
                    choices = c(savetypes_raster,
                                savetypes_vector,
                                savetypes_print,
                                "object"),
                    several.ok = TRUE)

  # if any save modes specified, check for filename
  if (length(generics::intersect(mode, c(savetypes_raster, savetypes_vector))) > 0) {
    if (filename == "") { stop("You must specify a filename if saving", call. = FALSE) }
  }

  # create list of plot constants, from globals unless overridden by user
  plot_constants <- utils::modifyList(cmapplot_globals$plot_constants, overrides)

  # convert function arguments (given in inches) to bigpoints
  plot_constants <- append(
    plot_constants,
    list(
      height = convertUnit(unit(height, "in"), "bigpts", valueOnly = TRUE),
      width = convertUnit(unit(width, "in"), "bigpts", valueOnly = TRUE),
      title_width = convertUnit(unit(title_width, "in"), "bigpts", valueOnly = TRUE),
      margin_v1_v2 = plot_constants$margin_v1 + plot_constants$margin_v2
    )
  )

  # calculate the size of the plot box
  plot_constants <- append(
    plot_constants,
    list(
      plotbox_height = plot_constants$height - plot_constants$margin_v1 -
                         plot_constants$margin_v4 - plot_constants$margin_v7,
      plotbox_width =  plot_constants$width - plot_constants$title_width - plot_constants$margin_h3
    )
  )

  # If title/caption unspecified, try to extract from plot
  input_title <- plot$labels$title
  if (title == "") {
    if(!is.null(input_title)) {
      title <- input_title
    } else {
      title <- "This plot needs a title"
    }
  }

  input_caption <- plot$labels$caption
  if (caption == "" & !is.null(input_caption)) {
    caption <- input_caption
  }

  # set outline color to black if debugging or transparent otherwise.
  if(debug){ debug_color = "red" } else { debug_color = "transparent" }

  # Size conversion for line widths in line graphs
  default_lwd <- ggplot2::GeomLine$default_aes$size
  ggplot2::update_geom_defaults(
    geom = "line",
    new = list(size = ggplot_size_conversion(plot_constants$lwd_plotline))
    )

  # preformat plot
  plot <- plot + ggplot2::theme(
    # **FONT SIZE ADJUSTMENT IS NECESSARY BUT NOT UNDERSTOOD**
    text = ggplot2::element_text(size = cmapplot_globals$font$main$size * 1.25),
    # remove any in-plot titles
    plot.title = element_blank(),
    plot.caption = element_blank(),
    # remove the above-legend spacing applied in theme_cmap()
    # (this is now applied outside of the viewport)
    legend.box.margin = margin(t = 0),
    # re-apply plot and legend margins, so they can be adjusted in
    # `overrides` argument of this function
    plot.margin = grid::unit(plot_constants$padding_plot,"bigpts"),
    legend.margin = margin(t = plot_constants$padding_legend[1],
                           r = plot_constants$padding_legend[2],
                           b = plot_constants$padding_legend[3],
                           l = plot_constants$padding_legend[4] + plot_constants$legend_indent,
                           "bigpts"),
    # apply any extra `ggplot2::theme()` args
    ...
  )

  # draw boxes around plot elements in debug mode
  if(debug){
    plot <- plot + ggplot2::theme(
      legend.background = element_rect(color = debug_color, fill = "transparent"),
      legend.box.background = element_rect(color = debug_color, fill = "transparent"),
      plot.background = element_rect(color = debug_color, fill = "transparent")
    )
  }


  # Build necessary viewports -----------------------------------------------------

  # create a parent viewport for centering the final_plot when drawing within R
  vp.centerframe <- grid::viewport(
    name = "vp.centerframe",
    default.units = "bigpts",
    width = plot_constants$width,
    height = plot_constants$height,
    clip = "on"
  )

  # create plotbox viewport
  vp.plotbox <- grid::viewport(
    name = "vp.plotbox",
    x = plot_constants$title_width,
    y = plot_constants$margin_v7,
    just = c(0,0),
    default.units = "bigpts",
    height = plot_constants$plotbox_height,
    width = plot_constants$plotbox_width,
    clip = "on"
  )

  # Build necessary grobs -----------------------------------------------------

  # grob to fill canvas (ROOT vp)
  grob_canvas <- grid::grid.rect(
    name = "canvas",
    gp = grid::gpar(fill = fill_canvas,
                    col = fill_canvas)
  )

  # grob to fill behind output (ROOT vp)
  grob_background <- grid::grid.rect(
    name = "background",
    gp = grid::gpar(fill = fill_bg,
                    col = fill_bg)
  )

  #  top line (ROOT vp)
  grob_topline <- grid::linesGrob(
    name = "topline",
    default.units = "bigpts",
    x = c(0, plot_constants$width),
    y = plot_constants$height - plot_constants$margin_v1,
    gp = grid::gpar(col = cmapplot_globals$colors$blackish,
                    lineend = "butt",
                    lwd = plot_constants$lwd_topline)
  )

  # title textbox (ROOT vp)
  grob_title <- gridtext::textbox_grob(
    name = "title",
    text = title,
    default.units = "bigpts",
    # set location down from top left corner
    x = 0,
    y = plot_constants$height - plot_constants$margin_v1_v2,
    hjust = 0,
    vjust = 1,
    # set dimensions
    width = plot_constants$title_width,
    maxheight = plot_constants$height - plot_constants$margin_v1_v2 - plot_constants$margin_v3,
    # set margins around textbox
    margin = grid::unit(c(0,                        # top
                          plot_constants$margin_h2, # right
                          0,                        # bottom
                          plot_constants$margin_h1),# left
                        "bigpts"),
    # set font aesthetic variables
    gp = grid::gpar(fontsize=cmapplot_globals$font$title$size,
                    fontfamily=cmapplot_globals$font$title$family,
                    fontface=cmapplot_globals$font$title$face,
                    lineheight=plot_constants$leading_title,
                    col=cmapplot_globals$colors$blackish),
    box_gp = grid::gpar(col = debug_color,
                        fill = "transparent")
  )

  # set caption textbox alignment options
  if(caption_valign == "top"){
    captionvars <- list(
      y = grid::unit(plot_constants$height - plot_constants$margin_v1_v2, "bigpts") - grid::grobHeight(grob_title),
      vjust = 1,
      maxheight = grid::unit(plot_constants$height - plot_constants$margin_v1_v2 - plot_constants$margin_v3, "bigpts") - grid::grobHeight(grob_title),
      padding_top = plot_constants$margin_v3,
      padding_bottom = 0
    )
  } else {
    captionvars <- list(
      y = 0,
      vjust = 0,
      maxheight = plot_constants$height - plot_constants$margin_v1_v2,
      padding_top = 0,
      padding_bottom = plot_constants$margin_v6
    )
  }

  # caption textbox (ROOT vp)
  grob_caption <- gridtext::textbox_grob(
    name = "caption",
    text = caption,
    default.units = "bigpts",
    # set location down from top left corner
    x = 0,
    y = captionvars$y,
    hjust = 0,
    vjust = captionvars$vjust,
    # set dimensions
    width = plot_constants$title_width,
    maxheight = captionvars$maxheight,
    # set margins within textbox
    margin = grid::unit(c(captionvars$padding_top,   # top
                          plot_constants$margin_h2,  # right
                          captionvars$padding_bottom,# bottom
                          plot_constants$margin_h1), # left
                        "bigpts"),
    # set aesthetic variables
    gp = grid::gpar(fontsize = cmapplot_globals$font$note$size,
                    fontfamily = cmapplot_globals$font$note$family,
                    fontface = cmapplot_globals$font$note$face,
                    lineheight = plot_constants$leading_caption,
                    col = cmapplot_globals$colors$blackish),
    box_gp = grid::gpar(col = debug_color,
                        fill = "transparent")
  )

  # ggplot as grob (vp.plotbox)
  grob_plot <- grid::grobTree(
    # Use helper function to develop full stack of legend, buffer, and plot
    buildChart(plot = plot,
               plot_constants = plot_constants,
               legend_build = legend_build),
    vp = vp.plotbox,
    name = "plot"
  )


  # Assemble final plot -----------------------------------------------------

  final_plot <- grid::grobTree(
    grob_background, grob_topline, grob_title, grob_caption, grob_plot,
    name = "final_plot"
  )

  # Output the figure based on mode selected -----------------------------------

  for(this_mode in mode){

    # if filename does not contain correct extension, add it
    # (in print modes this functions but is meaningless)
    if (!(grepl(paste0("\\.", this_mode, "$"), filename))) {
      this_filename <- paste0(filename, ".", this_mode)
    } else {
      this_filename <- filename
    }

    # export as raster
    if (this_mode %in% savetypes_raster) {

      # Open the device
      do.call(this_mode,
              list(filename = this_filename,
                   type = "cairo",
                   width = width,
                   height = height,
                   units = "in",
                   res = ppi))

      # draw the plot and close the device
      grid::grid.draw(final_plot)
      dev.off()

      message(paste("Export successful:", this_mode))

    # OR export as vector
    } else if (this_mode %in% savetypes_vector) {

      # add required cairo prefix for non-svg files
      mode_modified <- if (this_mode != "svg") { paste0("cairo_" , this_mode) } else { this_mode }

      # open the device
      do.call(mode_modified,
              list(filename = this_filename,
                   width = width,
                   height = height))

      # draw the plot and close the device
      grid::grid.draw(final_plot)
      dev.off()

      message(paste("Export successful:", this_mode))

    # OR display the grob in the plot window
    } else if (this_mode == "plot") {

      # set up blank canvas
      grid::grid.newpage()
      grid::grid.draw(grob_canvas)

      # enter centerframe, draw plot, exit centerframe
      grid::pushViewport(vp.centerframe)
      grid::grid.draw(final_plot)
      grid::popViewport()

    } else if (this_mode == "window"){

      if(.Platform$OS.type == "windows"){
        # open new device (window)
        grDevices::dev.new(width = width * 1.02,
                           height = height * 1.02,
                           noRStudioGD = TRUE)

        # set up blank canvas
        grid::grid.newpage()
        grid::grid.draw(grob_canvas)

        # enter centerframe, draw plot, exit centerframe
        grid::pushViewport(vp.centerframe)
        grid::grid.draw(final_plot)
        grid::popViewport()

        # reset device to default without closing window
        grDevices::dev.next()

      } else {

        message("`Window` mode not available on non-Windows platforms")
      }
    }
  }

  # return geom defaults as before
  ggplot2::update_geom_defaults("line",list(size = default_lwd))

  # if user wants an object, return it
  if("object" %in% mode){
    return(final_plot)
  }
}



#' @noRd
# Function to create plot object with left aligned legend on top
buildChart <- function(plot,
                       plot_constants,
                       legend_build) {

  # in safe mode, don't extract legend
  if(legend_build == "safe"){
    return(ggplotGrob(plot))
  }

  # in no legend mode, remove legend altogether
  if(legend_build == "none"){
    output_plot <- plot + theme(legend.position = "none")
    return(ggplotGrob(output_plot))
  }

  # In "adjust" mode...

  # Extract the legend
  legend <- ggpubr::get_legend(plot)

  # count the total number of "heights" in the legend (5 is standard for one
  # legend, with each additional legend adding two additional height elements to
  # the total)

  legend_total <- length(legend$heights)

  # extract height of legend object within ggplot plot. For plots with only one
  # legend, this returns the 3rd element, which is the height of the legend
  # component. For plots with two or more, it returns the sum of the heights of
  # every element from the third element to the third-to-last element, which
  # includes both text/key heights and buffers between legends)
  legend_height <- grid::convertUnit(sum(legend$heights[3:(legend_total - 2)]),
                                     "bigpts",
                                     valueOnly = TRUE)

  # calculate the height remaining for the plot
  plot_height <- plot_constants$plotbox_height - legend_height - plot_constants$margin_v5

  # Assemble a combined grob
  built <- gridExtra::arrangeGrob(
    legend,
    grid::rectGrob(gp = grid::gpar(col = "transparent", fill = "transparent")),
    ggplotGrob(plot + ggplot2::theme(legend.position = "none")),
    nrow = 3,
    heights = grid::unit(c(legend_height,
                           plot_constants$margin_v5,
                           plot_height),
                         "bigpts")
  )

  # return the combined grob
  return(built)
}
