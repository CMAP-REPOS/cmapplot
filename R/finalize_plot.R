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
#'  use 25 percent of the total output width (per Comms guidance). If set to 0,
#'  the title column is eliminated and the caption, if present, is moved to
#'  below the plot.
#'@param caption_align Numeric, alignment of the caption text. When the caption
#'  is in the title column (when \code{title_width > 0}), 0 (the default) aligns
#'  text to bottom; 1 aligns top. When the caption is located below the plot, 0
#'  aligns left and 1 aligns right. 0.5 aligns center.
#'@param mode Vector, the action(s) to be taken with the plot. View in R with
#'  \code{plot}, the default, or \code{window} (\code{window} only works on
#'  computers running Windows). Save using any of the following: \code{png},
#'  \code{tiff}, \code{jpeg}, \code{bmp}, \code{svg}, \code{pdf}, \code{ps}. Run
#'  multiple simultaneous outputs with a vector, e.g. \code{c("plot", "png",
#'  "pdf")}.
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
#'@param debug Bool, \code{TRUE} enables outlines around components of finalized
#'  plot. Defaults to \code{FALSE}.
#'@param use_cmap_aes Bool, \code{TRUE}, the default, temporarily implements
#'  CMAP default aesthetic settings for geoms (see
#'  \code{\link{apply_cmap_default_aes}}) for the present plot.
#'@param caption_valign This is deprecated as of cmapplot 1.1.0 and will be
#'  removed in future releases. Replace with \code{caption_align} argument.
#'@param ... Pass additional arguments to ggplot2's \code{\link[ggplot2]{theme}}
#'  function to override any elements of the plot's theme when drawing.
#'
#'@return This function invisibly returns the finished graphic as a gTree
#'  object. If stored (e.g. \code{g <- finalize_plot(...)}), the gTree can be
#'  drawn later with \code{grid} (e.g. \code{grid::grid.draw(g)}).
#'
#'@importFrom utils modifyList
#'@importFrom generics intersect
#'@importFrom gridExtra arrangeGrob
#'@importFrom ggpubr get_legend
#'@importFrom purrr compact
#'@importFrom stringr str_replace
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
                          caption_align = 0,
                          mode = c("plot"),
                          filename = NULL,
                          overwrite = FALSE,
                          ppi = 300,
                          fill_bg = "white",
                          fill_canvas = "gray90",
                          overrides = list(),
                          legend_shift = TRUE,
                          debug = FALSE,
                          use_cmap_aes = TRUE,
                          caption_valign,
                          ...
                          ){

  # Validation and initialization -----------------------------

  # Seek last plot if user did not specify one
  if (is.null(plot)) {
    plot <- ggplot2::last_plot()
  }

  # Set title_width to 25% of total width if unspecified.
  # Also force values to be between 0 and width/2.
  if (is.null(title_width)) {
    title_width <- width / 4
  } else if (title_width < 0) {
    message("`title_width` cannot be negative. Using 0 instead.")
    title_width <- 0
  } else if (title_width > width / 2) {
    message("`title_width` exceeds 50% of `width`. Using `width/2` instead.")
    title_width <- width / 2
  }

  # Create boolean for alternative "vertical" mode with no title and a bottom
  # caption, used when title_width is 0.
  vert_mode <- ifelse(title_width > 0, FALSE, TRUE)

  # Check deprecated variable `caption_valign`
  if (!missing(caption_valign)) {
    warning(paste(
      "The argument `caption_valign` is deprecated and will be removed in a future release.",
      "Please update your code to use `caption_align` (with a value between 0-1) instead.",
      sep = "\n  "))
    caption_valign <- match.arg(caption_valign, c("bottom", "top"))
    if (caption_valign == "top" & !vert_mode) {
      caption_align <- 1
    }
  }

  # Force caption_align to be between 0-1
  if (caption_align < 0) {
    message("`caption_align` must be between 0-1. Using 0 instead.")
    caption_align <- 0
  } else if (caption_align > 1) {
    message("`caption_align` must be between 0-1. Using 1 instead.")
    caption_align <- 1
  }

  # Remove any `window` mode specified if OS is not Windows
  if ("window" %in% mode & .Platform$OS.type != "windows"){
    mode <- stringr::str_replace(mode, "^window$", "plot")
    message("`mode='window'` is not supported on non-Windows systems. Switching to `mode='plot'` instead.")
  }

  # Check mode argument
  savetypes_raster <- c("png", "tiff", "jpeg", "bmp")
  savetypes_vector <- c("svg", "ps", "pdf")
  savetypes_print <- c("plot", "window")

  mode <- match.arg(arg = unique(mode),
                    choices = c(savetypes_print,
                                savetypes_vector,
                                savetypes_raster),
                    several.ok = TRUE)


  # If any save modes specified, check for filename
  if (length(generics::intersect(mode, c(savetypes_raster, savetypes_vector))) > 0) {
    if (is.null(filename)) { stop("You must specify a filename if saving", call. = FALSE) }
  }

  # If function will be drawing to the default plotting device (the plot window),
  # trigger a new page in that window now. This is needed to prevent the creation
  # of an unnecessary blank plot by other `grid` functions in cases where the
  # default device is not already active.
  if ("plot" %in% mode) { grid::grid.newpage() }

  # Create list of plot constants, from globals unless overridden by user
  consts <- utils::modifyList(cmapplot_globals$consts, overrides)

  # Add various arguments to constants, with conversions where necessary
  consts <- append(
    consts,
    list(
      height = grid::convertUnit(unit(height, "in"), "bigpts", valueOnly = TRUE),
      width = grid::convertUnit(unit(width, "in"), "bigpts", valueOnly = TRUE),
      title_width = grid::convertUnit(unit(title_width, "in"), "bigpts", valueOnly = TRUE),
      margin_title_to_top = consts$margin_topline_t + consts$margin_title_t
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


  # Build necessary grobs -----------------------------------------------------
  grobs <- list()

  # grob to fill behind output
  grobs$background <- grid::rectGrob(
    name = "background",
    gp = grid::gpar(fill = fill_bg,
                    col = fill_bg)
  )

  #  top line
  grobs$topline <- grid::linesGrob(
    name = "topline",
    default.units = "bigpts",
    x = c(0, consts$width),
    y = consts$height - consts$margin_topline_t,
    gp = grid::gpar(col = cmapplot_globals$colors$blackish,
                    lineend = "butt",
                    lwd = consts$lwd_topline / .lwd)
  )

  # in traditional (horizontal) mode, create title and left-side caption boxes
  if(!vert_mode){

    # title textbox
    grobs$title <- gridtext::textbox_grob(
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
      maxheight = consts$height - consts$margin_title_to_top,
      # retract texbox size on left
      margin = grid::unit(c(0, 0, # top, right
                            consts$margin_title_b, # bottom
                            consts$margin_title_l), # left
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

    # caption textbox
    grobs$caption <- gridtext::textbox_grob(
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
      height = consts$height - consts$margin_title_to_top - safe_grobHeight(grobs$title),
      # retract texbox size on each side
      margin = grid::unit(c(0, 0,  # top, right
                            consts$margin_caption_b,# bottom
                            consts$margin_title_l), # left
                          "bigpts"),
      # set aesthetic variables
      valign = caption_align,
      gp = grid::gpar(fontsize = cmapplot_globals$fsize$S,
                      fontfamily = cmapplot_globals$font$light$family,
                      fontface = cmapplot_globals$font$light$face,
                      lineheight = consts$leading_caption,
                      col = cmapplot_globals$colors$blackish),
      box_gp = grid::gpar(col = ifelse(debug, "red", NA),
                          fill = NA)
    )
  }

  # in vertical mode, and if caption exists, create bottom caption box (but no title)
  if(vert_mode & caption != "") {
    grobs$caption_bottom <- gridtext::textbox_grob(
      name = "caption_bottom",
      text = caption,
      default.units = "bigpts",
      # set location
      x = consts$title_width,
      y = 0,
      hjust = 0,
      vjust = 0,
      # set dimensions
      width = consts$width - consts$title_width,
      # retract texbox size on each side
      margin = grid::unit(c(0,                      # top
                            consts$margin_plot_r,   # right
                            consts$margin_caption_b,# bottom
                            consts$margin_plot_l),  # left
                          "bigpts"),
      # set aesthetic variables
      halign = caption_align,
      gp = grid::gpar(fontsize = cmapplot_globals$fsize$S,
                      fontfamily = cmapplot_globals$font$light$family,
                      fontface = cmapplot_globals$font$light$face,
                      lineheight = consts$leading_caption,
                      col = cmapplot_globals$colors$blackish),
      box_gp = grid::gpar(col = ifelse(debug, "red", NA),
                          fill = NA)
    )
  }

  # calculate the height of the plotbox (area for legend and plot)
  consts$plotbox_height <- consts$height - consts$margin_topline_t -
                           consts$margin_legend_t - consts$margin_plot_b -
                           safe_grobHeight(grobs$caption_bottom)


  # Build plotbox viewport
  vp.plotbox <- grid::viewport(
    name = "vp.plotbox",
    x = consts$title_width + consts$margin_plot_l,
    y = safe_grobHeight(grobs$caption_bottom) + consts$margin_plot_b,
    just = c(0,0),
    default.units = "bigpts",
    height = consts$plotbox_height,
    width = consts$width - consts$title_width - consts$margin_plot_r - consts$margin_plot_l,
    clip = "on"
  )

  # plot as grob (drawn into vp.plotbox)
  grobs$plot <- grid::grobTree(
    # use subfn to prepare plot for final plotting
    prepare_plot(plot = plot,
                 consts = consts,
                 overrides = overrides,
                 legend_shift = legend_shift,
                 debug = debug,
                 use_cmap_aes = use_cmap_aes,
                 ...),
    vp = vp.plotbox,
    name = "plot"
  )

  # Assemble finished graphic -----------------------------------------------------

  # this do.call combines the list of grobs with the `name` argument and passes
  # that list on to the grobTree function.
  finished_graphic <- do.call(grobTree, c(grobs, name = "finished_graphic"))

  # Output the figure based on mode selected -----------------------------------

  # first, do in-R drawing
  for (this_mode in generics::intersect(mode, savetypes_print)) {
    draw_plot(finished_graphic = finished_graphic,
              width = width,
              height = height,
              fill_canvas = fill_canvas,
              mode = this_mode)
  }

  # second, export vectors
  for (this_mode in generics::intersect(mode, savetypes_vector)) {

    # construct arglist for drawing device
    arglist <- list(filename = filename,
                    width = width,
                    height = height)

    # export the plot
    save_plot(finished_graphic = finished_graphic,
              mode = this_mode,
              arglist = arglist,
              overwrite = overwrite)
  }

  # third, export rasters
  for (this_mode in generics::intersect(mode, savetypes_raster)) {

    # construct arglist for drawing device
    arglist <- list(filename = filename,
                    type = "cairo",
                    width = width,
                    height = height,
                    units = "in",
                    res = ppi)

    # export the plot
    save_plot(finished_graphic = finished_graphic,
              mode = this_mode,
              arglist = arglist,
              overwrite = overwrite)
  }

  # finally, return plot as grob
  invisible(finished_graphic)
}


#' Sub-fn to create plot grob, including legend-realignment
#' @noRd
prepare_plot <- function(plot,
                         consts,
                         overrides,
                         legend_shift,
                         debug,
                         use_cmap_aes,
                         ...) {

  # override geom defaults -------------------------------------

  if (use_cmap_aes) {
    # cache current defaults
    geom_defaults <- fetch_current_default_aes()
    # set cmap custom defaults
    set_default_aes(cmapplot_globals$default_aes_cmap)
    # when this function exits, whether or not due to error, reset geom defaults
    on.exit(set_default_aes(geom_defaults))
  }

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


#' Sub-fn to draw plot within R
#' @noRd
draw_plot <- function(finished_graphic,
                      width,
                      height,
                      fill_canvas,
                      mode){

  # in window mode, open new drawing device
  if (mode == "window") {
    grDevices::dev.new(width = width * 1.02,
                       height = height * 1.02,
                       noRStudioGD = TRUE)
  }

  # draw blank canvas
  grid::grid.rect(gp = grid::gpar(fill = fill_canvas,
                                  col = fill_canvas)
  )

  # create and enter a viewport for drawing the finished_graphic.
  # this creates a drawing space in the center of the active device
  # that is the user-specified dimensions.
  grid::pushViewport(
    grid::viewport(
      width = width,
      height = height,
      default.units = "in",
      clip = "on"
    )
  )

  # draw plot, exit centerframe
  grid::grid.draw(finished_graphic)
  grid::popViewport()

  # in window mode, reset device to default without closing window
  if (mode == "window") {
    grDevices::dev.next()
  }
}


#' Sub-fn to save plot using various device functions
#' @importFrom stringr str_trunc
#' @noRd
save_plot <- function(finished_graphic,
                      mode,
                      arglist,
                      overwrite){

  # Prepare some things -----------------------------------------------

  # if filename does not contain correct extension, add it
  if (!(grepl(paste0("\\.", mode, "$"), arglist$filename))) {
    arglist$filename <- paste0(arglist$filename, ".", mode)
  }

  # construct pretty filename for messages
  fname <- stringr::str_trunc(arglist$filename, 50, "left")


  # add required cairo prefix to function name for pdf and ps (see `?cairo`)
  mode <- ifelse (mode == "pdf" | mode == "ps", paste0("cairo_" , mode), mode)

  # if file exists and overwrite == FALSE, do not write
  if (file.exists(arglist$filename) & !overwrite) {
    message(paste0(fname, ": SKIPPED (try `overwrite = TRUE`?)"))
    return()
  }

  # Write to device -----------------------------------------------
  tryCatch(
    {
      # open the device, draw the plot, close the device
      suppressWarnings(do.call(mode, arglist))
      grid::grid.draw(finished_graphic)
      dev.off()

      # notify
      message(paste0(fname, ": Export successful"))

      # return nothing
      NULL
    },
    error = function(cond) {
      # Or safely error
      message(paste0(fname, ": FAILED (is file open?)"))
    }
  )

}


#' Sub-fn to safely intepret grobHeight
#'
#' This returns the height of Grob in any real unit.
#' If the value passed in is null, it returns 0.
#'
#' @noRd
safe_grobHeight <- function(grob, unitTo = "bigpts", valueOnly = TRUE){

  if(is.null(grob)){
    if(valueOnly){
      return(0)
    } else {
      return(unit(0, unitTo))
    }
  }

  return(grid::convertHeight(grid::grobHeight(grob), unitTo, valueOnly))
}
