#'Arrange and save CMAP ggplot chart
#'
#'Place a ggplot into a frame defined by CMAP design standards. It will align
#'your title and caption to the left, add a horizontal line on top, and make
#'other adjustments. It can show you the final plot and/or export it as a raster
#'or vector file. This function will not apply CMAP design standards to the plot
#'itself: use with \code{theme_cmap()} for that. Exports from this function use
#'Cairo graphics drivers, while drawing within R is done with default (Windows)
#'drivers.
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
#'  5.56 inches tall (400/72), to match Comms specification for web graphics.
#'@param title_width Numeric, the width in inches for the title. If unspecified,
#'  use 25 percent of the total output width (per Comms guidance).
#'@param caption_valign Char, align the caption text at the top or the bottom of
#'  the available space between the title and bottom of image. This argument
#'  accepts abbreviations, too: \code{c("bottom", "b", "top", "t")}.
#'@param mode Vector, the action(s) to be taken with the plot. Save using any of
#'  the following: \code{png}, \code{tiff}, \code{jpeg}, \code{bmp}, \code{svg},
#'  \code{pdf}, \code{ps}. View in R with: \code{plot}, \code{window}
#'  (\code{window} = \code{plot} on computers not running Windows OS).
#'@param filename Char, the file path and name you want the plot to be saved to.
#'  You may specify an extension to use. If you don't, the correct extension
#'  will be added for you.
#'@param overwrite Bool, set to \code{TRUE} if you would like the function to
#'  overwrite existing files by the same name. The default is \code{FALSE}.
#'@param ppi Numeric, the resolution of exported images (pixels per inch).
#'  Default = 300.
#'@param fill_bg,fill_canvas Char, strings that represent colors R can
#'  interpret. They are used to fill behind and around the finished plot,
#'  respectively.
#'@param overrides Named list, overrides the default drawing attributes defined
#'  in \code{cmapplot_globals$consts} which are drawn by
#'  \code{\link{finalize_plot}}. Units are in bigpts (1/72 of an inch).
#'@param legend_shift Bool, \code{TRUE}, the default, attempts to align the
#'  legend all the way left (on top of the y axis labels) per CMAP design
#'  standards. \code{FALSE} maintains the alignment used in the original plot.
#'@param debug Bool, TRUE enables outlines around components of finalized plot.
#'  Default = FALSE.
#'@param use_cmap_aes Bool, \code{TRUE}, the default, temporarily implements
#'  CMAP default aesthetic settings for geoms (see
#'  \code{\link{apply_cmap_default_aes}}) for the present plot.
#'@param ... pass additional arguments to ggplot2's \code{\link[ggplot2]{theme}}
#'  function to override any elements of the default CMAP theme.
#'
#'@return This function invisibly returns a gTree object. If stored (e.g. with
#'  \code{g <- finalize_plot(...)}), the gTree can be drawn later with
#'  \code{grid::grid.draw()}.
#'
#'@importFrom utils modifyList
#'@importFrom generics intersect
#'@importFrom gridExtra arrangeGrob
#'@importFrom ggpubr get_legend
#'@importFrom purrr compact
#'
#'@examples
#' \dontrun{
#' econ_plot <- ggplot(data = cluster_jobchange,
#'                     mapping = aes(
#'                       y = reorder(name, jobchange),
#'                       x = jobchange,
#'                       fill = category)) +
#'   geom_col() +
#'   theme_cmap(gridlines = "v", vline = 0) +
#'   scale_x_continuous(labels = scales::comma)
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
                          caption_valign = c("bottom", "top"),
                          mode = c("plot"),
                          filename = "",
                          overwrite = FALSE,
                          ppi = 300,
                          fill_bg = "white",
                          fill_canvas = "gray90",
                          overrides = list(),
                          legend_shift = TRUE,
                          debug = FALSE,
                          use_cmap_aes = TRUE,
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

  # check mode argument
  savetypes_raster <- c("png","tiff","jpeg","bmp")
  savetypes_vector <- c("svg","ps","pdf")
  savetypes_print <- c("plot", "window")

  mode <- match.arg(arg = mode,
                    choices = c(savetypes_raster,
                                savetypes_vector,
                                savetypes_print),
                    several.ok = TRUE)

  # if any save modes specified, check for filename
  if (length(generics::intersect(mode, c(savetypes_raster, savetypes_vector))) > 0) {
    if (filename == "") { stop("You must specify a filename if saving", call. = FALSE) }
  }

  # if function will be drawing to the default plotting device (the plot window),
  # trigger a new page in that window now. This is needed to prevent the creation
  # of an unnecessary blank plot by other `grid` functions in cases where the
  # default device is not already active.
  if ("plot" %in% mode) { grid::grid.newpage() }

  # create list of plot constants, from globals unless overridden by user
  consts <- utils::modifyList(cmapplot_globals$consts, overrides)

  # add various arguments to constants, with conversions where necessary
  consts <- append(
    consts,
    list(
      height = grid::convertUnit(unit(height, "in"), "bigpts", valueOnly = TRUE),
      width = grid::convertUnit(unit(width, "in"), "bigpts", valueOnly = TRUE),
      title_width = grid::convertUnit(unit(title_width, "in"), "bigpts", valueOnly = TRUE),
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
    if (!is.null(input_title)) {
      title <- input_title
    } else {
      title <- "This plot needs a title"
    }
  }

  input_caption <- plot$labels$caption
  if (caption == "" & !is.null(input_caption)) {
    caption <- input_caption
  }


  # Prepare ggplot -------------------------------------------------------------

  # cache then temporary override geom defaults
  if (use_cmap_aes) {
    geom_defaults <- fetch_current_default_aes()
    set_default_aes(cmapplot_globals$default_aes_cmap)
  }

  # Build full stack of legend, buffer, and plot, and debug rects as a grob
  plot <- buildChart(plot = plot,
                     consts = consts,
                     overrides = overrides,
                     legend_shift = legend_shift,
                     debug = debug,
                     ...)

  # reset geom defaults
  if (use_cmap_aes) {
    set_default_aes(geom_defaults)
  }


  # Assemble final plot --------------------------------------------------------

  final_plot <- construct_plot(plot = plot,
                               consts = consts,
                               fill_bg = fill_bg,
                               title = title,
                               caption = caption,
                               caption_valign = caption_valign,
                               debug = debug)


  # Output the figure based on mode selected -----------------------------------

  # first, do in-R drawing
  for(this_mode in intersect(mode, savetypes_print)){
    draw_plot(final_plot = final_plot,
              consts = consts,
              fill_canvas = fill_canvas,
              mode = this_mode)
  }

  # second, export vectors
  for(this_mode in intersect(mode, savetypes_vector)){

    # construct arglist for drawing device
    arglist <- list(filename = filename,
                    width = width,
                    height = height)

    # export the plot
    save_plot(final_plot = final_plot,
              mode = this_mode,
              arglist = arglist,
              overwrite = overwrite)
  }

  # third, export rasters
  for(this_mode in intersect(mode, savetypes_raster)){

    # construct arglist for drawing device
    arglist <- list(filename = filename,
                    type = "cairo",
                    width = width,
                    height = height,
                    units = "in",
                    res = ppi)

    # export the plot
    save_plot(final_plot = final_plot,
              mode = this_mode,
              arglist = arglist,
              overwrite = overwrite)
  }

  # finally, return plot as grob
  invisible(final_plot)
}


#' Sub-fn to create plot grob, including legend-realignment
#' @noRd
buildChart <- function(plot,
                       consts,
                       overrides,
                       legend_shift,
                       debug,
                       ...) {

  # preformat plot ---------------------------------------------

  # the basics
  plot <- plot + ggplot2::theme(
    # remove any in-plot titles
    plot.title = element_blank(),
    plot.caption = element_blank(),
    # apply any extra `ggplot2::theme()` args
    ...
  )

  # add debug rect around plot if in debug mode
  if (debug) {
    plot <- plot + ggplot2::theme(
      plot.background = element_rect(color = "red")
    )
  }

  # return plot as grob if no legend shift ---------------------
  if (!legend_shift | is.null(ggpubr::get_legend(plot))) {
    return(ggplotGrob(plot))
  }

  # Shift legend -----------------------------------------------

  # add debug rects around legend if in debug mode
  if (debug) {
    plot <- plot + ggplot2::theme(
      legend.background = element_rect(color = "red"),
      legend.box.background = element_rect(color = "red")
    )
  }

  # Determine correct legend margins to use. Use from plot (set
  # via theme_cmap(), with possible overrides) unless user has
  # set override in finalize
  margin_legend_i <- ifelse(
    # if not overridden in finalize, use value from ggplot
    is_null(overrides$margin_legend_i),
    dplyr::first(purrr::compact(list( # Call first non-NA value in this list:
      # If plot has a theme:
      plot$theme$legend.spacing.y, # If available, use specific y-legend spacing
      plot$theme$legend.spacing, # If not, default to general legend spacing
      # If plot has no theme, use globally applied theme
      ggplot2::theme_get()$legend.spacing.y, # If available, use y-legend spacing
      ggplot2::theme_get()$legend.spacing # If not, default to general legend spacing
    ))),
    # otherwise, use override value
    overrides$margin_legend_i
  )

  margin_legend_b <- ifelse(
    # if not overridden in finalize, use value from ggplot
    is_null(overrides$margin_legend_b),
    # Repeat logic from margin_legend_i
    convertUnit(dplyr::first(purrr::compact(list(
      plot$theme$legend.box.spacing,
      ggplot2::theme_get()$legend.box.spacing
      ))), unitTo = "bigpts", valueOnly = TRUE),
    # otherwise, use override value
    overrides$margin_legend_b
  )

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
      # e.g., for a 2-legend item, this modifies element 4
      margin_index <- 2*(i+1)
      # apply correct legend spacing
      legend$heights[[margin_index]] <- grid::unit(margin_legend_i,"bigpts")
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
  plot_height <- consts$plotbox_height - legend_height - margin_legend_b

  # Assemble a combined grob
  built <- gridExtra::arrangeGrob(
    legend,
    grid::rectGrob(gp = grid::gpar(col = NA, fill = NA)),
    ggplotGrob(plot + ggplot2::theme(legend.position = "none")),
    nrow = 3,
    heights = grid::unit(c(legend_height,
                           margin_legend_b,
                           plot_height),
                         "bigpts")
  )

  # return the combined grob
  return(built)
}


#' Sub-fn to create final plot layout, and return as grobTree
#' @noRd
construct_plot <- function(plot,
                           consts,
                           fill_bg,
                           title,
                           caption,
                           caption_valign,
                           debug) {

  # Build necessary viewports -----------------------------------------------------

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
                    lwd = consts$lwd_topline / .lwd)
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
    gp = grid::gpar(fontsize=cmapplot_globals$fsize$L,
                    fontfamily=cmapplot_globals$font$strong$family,
                    fontface=cmapplot_globals$font$strong$face,
                    lineheight=consts$leading_title,
                    col=cmapplot_globals$colors$blackish),
    box_gp = grid::gpar(col = ifelse(debug, "red", NA),
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
    valign = ifelse(caption_valign == "top", 1, 0),
    gp = grid::gpar(fontsize = cmapplot_globals$fsize$S,
                    fontfamily = cmapplot_globals$font$light$family,
                    fontface = cmapplot_globals$font$light$face,
                    lineheight = consts$leading_caption,
                    col = cmapplot_globals$colors$blackish),
    box_gp = grid::gpar(col = ifelse(debug, "red", NA),
                        fill = NA)
  )

  # ggplot as grob (vp.plotbox)
  grob_plot <- grid::grobTree(
    plot,
    vp = vp.plotbox,
    name = "plot"
  )


  # Assemble final plot -----------------------------------------------------

  final_plot <- grid::grobTree(
    grob_background, grob_topline, grob_title, grob_caption, grob_plot,
    name = "final_plot"
  )

  return(final_plot)
}



#' Sub-fn to draw plot within R
#' @noRd
draw_plot <- function(final_plot,
                      consts,
                      fill_canvas,
                      mode){

  # create a parent viewport for centering the final_plot when drawing within R
  vp.centerframe <- grid::viewport(
    name = "vp.centerframe",
    default.units = "bigpts",
    width = consts$width,
    height = consts$height,
    clip = "on"
  )

  # in window mode, open new drawing device
  if (mode == "window" & .Platform$OS.type == "windows") {
    grDevices::dev.new(width = consts$width * 72 * 1.02,
                       height = consts$height * 72 * 1.02,
                       noRStudioGD = TRUE)
  }

  # draw blank canvas
  grid::grid.rect(gp = grid::gpar(fill = fill_canvas,
                                  col = fill_canvas)
  )

  # enter centerframe, draw plot, exit centerframe
  grid::pushViewport(vp.centerframe)
  grid::grid.draw(final_plot)
  grid::popViewport()

  # in window mode, reset device to default without closing window
  if (mode == "window" & .Platform$OS.type == "windows") {
    grDevices::dev.next()
  }
}


#' Sub-fn to save plot using various device functions
#' @noRd
save_plot <- function(final_plot,
                      mode,
                      arglist,
                      overwrite){

  # if filename does not contain correct extension, add it
  if (!(grepl(paste0("\\.", mode, "$"), arglist$filename))) {
    arglist$filename <- paste0(arglist$filename, ".", mode)
  }

  # if file exists and overwrite == FALSE, do not write
  if(file.exists(arglist$filename) & !overwrite){
    message(paste(arglist$filename, "already exists. Skipped. Try `overwrite = TRUE`?"))
    return()
  }

  # add required cairo prefix to function name for pdf and ps (see `?cairo`)
  mode <- ifelse (mode == "pdf" | mode == "ps", paste0("cairo_" , mode), mode)

  # open the device
  do.call(mode, arglist)

  # draw the plot and close the device
  grid::grid.draw(final_plot)
  dev.off()

  message(paste("Export successful:", mode))
}
