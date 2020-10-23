#'Arrange and save CMAP ggplot chart
#'
#'\code{finalize_plot} will place a ggplot into a frame defined by CMAP design
#'standards. It will align your title and caption to the left, add a horizontal
#'line on top, and make other adjustments. It can show you the final plot and/or
#'export it as a raster or vector file. This function will not apply CMAP design
#'standards to the plot itself: use with \code{theme_cmap()} for that.
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
#'  currently works only on computers running Windows). Return an object with
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
#'  in \code{cmapplot_globals$consts} which are drawn by
#'  \code{finalize_plot()} (this is most of them). Units are in bigpts (1/72 of
#'  an inch).
#'@param debug Bool, TRUE enables outlines around components of finalized plot.
#'  Default = FALSE.
#'@param legend_build Char, how the function attempts to build the legend.
#'  \code{"adjust"}, the default, attempts to align the legend all the way left
#'  (on top of the y axis labels) per CMAP design standards. \code{"safe"}
#'  maintains the alignment used in the original plot.
#'@param legend_bump Numeric, shift the legend left (positive) or right
#'  (negative) this amount. Depending on system configuration, it may be
#'  necessary to use this parameter to achieve exact left alignment (this can
#'  most easily be tested using \code{debug = TRUE}). Expressed in bigpts.
#'@param ... pass additional arguments to \code{ggplot2::theme()} to override any
#'  elements of the default CMAP theme.
#'
#'@return Exports from this function use Cairo graphics drivers, while drawing
#'  within R is done with default (Windows) drivers. \code{mode = "object"} also
#'  returns a gTree object that can be stored and drawn later with
#'  \code{grid::grid.draw()}. Lines drawn by any \code{geom_line()} geoms
#'  without line widths explicitly specified are assigned a thicker width
#'  (specifically, \code{cmapplot_globals$consts$lwd_plotline}) in all outputs
#'  except for when exporting as an object.
#'
#'@section Overrides: In the \code{overrides} argument, the user can modify
#'  certain default constants that define certain plot aesthetics. Units of all
#'  plot constants are "bigpts": 1/72 of an inch. Most plot constants (stored in
#'  \code{cmapplot_globals$consts}) are used in this function, while the few
#'  are used in \code{theme_cmap()}. For constants used in both functions, any
#'  overrides specified in \code{theme_cmap()} must be specified again here.
#'
#'  \itemize{
#'    \item \code{lwd_plotline}: The width of line graph lines.
#'    \item \code{lwd_topline}: The width of the line above the plot and title.
#'    \item \code{margin_topline_t}: The margin between the top edge of the
#'    image and the top line.
#'    \item \code{margin_title_t}: The margin between the top line and the
#'    title.
#'    \item \code{margin_title_b}: The margin between the title and the caption.
#'    \item \code{margin_caption_b}: The margin between the bottom of the
#'    caption and the bottom edge of the image.
#'    \item \code{margin_legend_t}: The margin between the top line and the
#'    plot box (i.e., the top of the legend).
#'    \item \code{margin_legend_i}: The margin between legends (this only
#'    applies in plots with two or more legends and does not affect legend
#'    spacing on plots with single legends that have multiple rows).
#'    \item \code{margin_legend_b}: The margin between the bottom of the legend
#'    and the rest of the plot.
#'    \item \code{margin_plot_b}: The margin between the bottom of the plot and
#'    the bottom edge of the image.
#'    \item \code{margin_title_l}: The margin between the left edge of the image
#'    and the title. This also applies to the caption. Deducted from
#'    \code{title_width}.
#'    \item \code{margin_title_r}: The margin between the right edge of the
#'    image and the title. This also applies to the caption. Deducted from
#'    \code{title_width}.
#'    \item \code{margin_plot_r}: The margin between the right edge of the plot
#'    and the edge of the image.
#'    \item \code{padding_plot}: A numeric vector of length 4 (top, right,
#'    bottom, left) that creates padding between the plot and its drawing
#'    extent.
#'    \item \code{padding_legend}: A numeric vector of length 4 (top, right,
#'    bottom, left) that creates padding around the margin. These numbers can be
#'    negative to reduce space around the legend.
#'    \item \code{legend_key_size}: The size of legend key elements.
#'    \item \code{leading_title}: Text leading for Title text.
#'    \item \code{leading_caption}: Text leading for Caption text.
#'  }
#'
#'@importFrom utils modifyList
#'@importFrom generics intersect
#'@importFrom gridExtra arrangeGrob
#'@importFrom ggpubr get_legend
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
#'                overrides = list(margin_plot_r = 30))
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
#'               (in millions)",
#'               "Source: Chicago Metropolitan Agency for Planning
#'               analysis of data from the Regional Transportation Authority",
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
                          legend_bump = 0,
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

  # if function will be drawing to the default plotting device (the plot window),
  # trigger a new page in that window now. This is needed to prevent the creation
  # of an unnecessary blank plot by other `grid` functions in cases where the
  # default device is not already active.
  if("plot" %in% mode){ grid::grid.newpage() }

  # create list of plot constants, from globals unless overridden by user
  consts <- utils::modifyList(cmapplot_globals$consts, overrides)

  # add various arguments to constants, with conversions where necessary
  consts <- append(
    consts,
    list(
      height = grid::convertUnit(unit(height, "in"), "bigpts", valueOnly = TRUE),
      width = grid::convertUnit(unit(width, "in"), "bigpts", valueOnly = TRUE),
      title_width = grid::convertUnit(unit(title_width, "in"), "bigpts", valueOnly = TRUE),
      legend_bump = legend_bump,
      margin_title_to_top = consts$margin_topline_t + consts$margin_title_t
    )
  )

  # calculate the size of the plot box
  consts <- append(
    consts,
    list(
      plotbox_height = consts$height - consts$margin_topline_t -
                         consts$margin_legend_t - consts$margin_plot_b,
      plotbox_width =  consts$width - consts$title_width - consts$margin_plot_r
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
  if(debug){ debug_color = "red" } else { debug_color = NA }

  # Size conversion for line widths in line graphs
  default_lwd <- ggplot2::GeomLine$default_aes$size
  ggplot2::update_geom_defaults(
    geom = "line",
    new = list(size = ggplot_size_conversion(consts$lwd_plotline))
    )

  # Font defaults for annotations and labels on the graph
  default_fontfamily <- ggplot2::theme_get()$text$family
  default_fontface <- ggplot2::theme_get()$text$face
  default_fontcolor <- ggplot2::theme_get()$text$colour
  default_fontsize <- ggplot2::theme_get()$text$size

  ggplot2::update_geom_defaults(
    geom = "text",
    new = list(family = cmapplot_globals$font$main$family,
               face = cmapplot_globals$font$main$face,
               size = 5, # For unknown reasons, this corresponds to size 14 when exported
               colour = cmapplot_globals$colors$blackish)
  )

  # preformat plot
  plot <- plot + ggplot2::theme(
    # **FONT SIZE ADJUSTMENT IS NECESSARY BUT NOT UNDERSTOOD**
    text = ggplot2::element_text(size = cmapplot_globals$font$main$size * 1.25),
    # remove any in-plot titles
    plot.title = element_blank(),
    plot.caption = element_blank(),
    # re-apply plot and legend margins, so they can be adjusted in
    # `overrides` argument of this function
    plot.margin = grid::unit(consts$padding_plot,"bigpts"),
    legend.margin = margin(t = consts$padding_legend[1],
                           r = consts$padding_legend[2],
                           b = consts$padding_legend[3],
                           l = consts$padding_legend[4] + consts$legend_bump,
                           "bigpts"),
    # re-apply legend key size for `overrides`
    legend.key.size = grid::unit(consts$legend_key_size,"bigpts"),
    # apply any extra `ggplot2::theme()` args
    ...
  )

  # draw boxes around plot elements in debug mode
  if(debug){
    plot <- plot + ggplot2::theme(
      legend.background = element_rect(color = debug_color, fill = NA),
      legend.box.background = element_rect(color = debug_color, fill = NA),
      plot.background = element_rect(color = debug_color, fill = NA)
    )
  }


  # Build necessary viewports -----------------------------------------------------

  # create a parent viewport for centering the final_plot when drawing within R
  vp.centerframe <- grid::viewport(
    name = "vp.centerframe",
    default.units = "bigpts",
    width = consts$width,
    height = consts$height,
    clip = "on"
  )

  # create plotbox viewport
  vp.plotbox <- grid::viewport(
    name = "vp.plotbox",
    x = consts$title_width,
    y = consts$margin_plot_b,
    just = c(0,0),
    default.units = "bigpts",
    height = consts$plotbox_height,
    width = consts$plotbox_width,
    clip = "on"
  )

  # Build necessary grobs -----------------------------------------------------

  # grob to fill canvas (ROOT vp)
  grob_canvas <- grid::rectGrob(
    name = "canvas",
    gp = grid::gpar(fill = fill_canvas,
                    col = fill_canvas)
  )

  # grob to fill behind output (ROOT vp)
  grob_background <- grid::rectGrob(
    name = "background",
    gp = grid::gpar(fill = fill_bg,
                    col = fill_bg)
  )

  #  top line (ROOT vp)
  grob_topline <- grid::linesGrob(
    name = "topline",
    default.units = "bigpts",
    x = c(0, consts$width),
    y = consts$height - consts$margin_topline_t,
    gp = grid::gpar(col = cmapplot_globals$colors$blackish,
                    lineend = "butt",
                    lwd = consts$lwd_topline)
  )

  # title textbox (ROOT vp)
  grob_title <- gridtext::textbox_grob(
    name = "title",
    text = title,
    default.units = "bigpts",
    # set location down from top left corner
    x = 0,
    y = consts$height - consts$margin_title_to_top,
    hjust = 0,
    vjust = 1,
    # set dimensions
    width = consts$title_width,
    maxheight = consts$height - consts$margin_title_to_top - consts$margin_title_b,
    # retract texbox size on left and right
    margin = grid::unit(c(0,                     # top
                          consts$margin_title_r, # right
                          0,                     # bottom
                          consts$margin_title_l),# left
                          "bigpts"),
    # set font aesthetic variables
    gp = grid::gpar(fontsize=cmapplot_globals$font$title$size,
                    fontfamily=cmapplot_globals$font$title$family,
                    fontface=cmapplot_globals$font$title$face,
                    lineheight=consts$leading_title,
                    col=cmapplot_globals$colors$blackish),
    box_gp = grid::gpar(col = debug_color,
                        fill = NA)
  )

  # caption textbox (ROOT vp)
  grob_caption <- gridtext::textbox_grob(
    name = "caption",
    text = caption,
    default.units = "bigpts",
    # set location
    x = 0,
    y = 0,
    hjust = 0,
    vjust = 0,
    # set dimensions
    width = consts$title_width,
    height = grid::unit(consts$height - consts$margin_title_to_top, "bigpts") - grid::grobHeight(grob_title),
    # retract texbox size on each side
    margin = grid::unit(c(consts$margin_title_b,  # top
                          consts$margin_title_r,  # right
                          consts$margin_caption_b,# bottom
                          consts$margin_title_l), # left
                        "bigpts"),
    # set aesthetic variables
    valign = if(caption_valign == "top"){ 1 } else { 0 },
    gp = grid::gpar(fontsize = cmapplot_globals$font$note$size,
                    fontfamily = cmapplot_globals$font$note$family,
                    fontface = cmapplot_globals$font$note$face,
                    lineheight = consts$leading_caption,
                    col = cmapplot_globals$colors$blackish),
    box_gp = grid::gpar(col = debug_color,
                        fill = NA)
  )

  # ggplot as grob (vp.plotbox)
  grob_plot <- grid::grobTree(
    # Use helper function to develop full stack of legend, buffer, and plot
    buildChart(plot = plot,
               consts = consts,
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
  ggplot2::update_geom_defaults(
    geom = "text",
    new = list(family = default_fontfamily,
               face = default_fontface,
               size = default_fontsize,
               colour = default_fontcolor)
  )

  # if user wants an object, return it
  if("object" %in% mode){
    return(final_plot)
  }
}



#' @noRd
# Function to create plot object with left aligned legend on top
buildChart <- function(plot,
                       consts,
                       legend_build) {

  # in safe mode, don't extract legend
  if(legend_build == "safe"){
    output_plot <- plot + theme(
      legend.spacing.y = grid::unit(consts$margin_legend_i, "bigpts"))
    return(ggplotGrob(output_plot))
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
  # the total). Use this to determine the number of legends in the plot.

  number_of_legends <- (length(legend$heights) - 3) / 2

  # If multilegend plot (i.e., if legend_total >= 7), replace interior margins
  # with overrides if called. For a plot with n legends, there are n - 1
  # interior margins. These are the 4th legend height element and every second
  # one beyond that, up to the 4th-to-last legend height element.

  # determine if multilegend plot
  if (number_of_legends > 1) {
    # if multilegend plot, establish loop to change margins
    for (i in 1:(number_of_legends-1)) {
      margin_index <- 2*(i+1) # e.g., for a 2-legend item, this modifies element 4
      legend$heights[[margin_index]] <- grid::unit(consts$margin_legend_i,"bigpts")
    }
  }

  # extract height of legend object within ggplot plot. For plots with only one
  # legend, this returns the 3rd element, which is the height of the legend
  # component. For plots with two or more, it returns the sum of the heights of
  # every element from the third element to the third-to-last element, which
  # includes both text/key heights and buffers between legends)
  legend_height <- grid::convertUnit(sum(legend$heights[3:(3 + number_of_legends * 2)]),
                                     "bigpts",
                                     valueOnly = TRUE)

  # calculate the height remaining for the plot
  plot_height <- consts$plotbox_height - legend_height - consts$margin_legend_b

  # Assemble a combined grob
  built <- gridExtra::arrangeGrob(
    legend,
    grid::rectGrob(gp = grid::gpar(col = NA, fill = NA)),
    ggplotGrob(plot + ggplot2::theme(legend.position = "none")),
    nrow = 3,
    heights = grid::unit(c(legend_height,
                           consts$margin_legend_b,
                           plot_height),
                         "bigpts")
  )

  # return the combined grob
  return(built)
}
