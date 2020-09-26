#'Arrange and save CMAP ggplot chart
#'
#'\code{finalize_plot2} will place a ggplot into a frame defined by CMAP design
#'standards. It will align your title and caption to the left, add a horizontal
#'line on top, and make other adjustments. It can show you the final plot and/or
#'export it as a raster or vector file. This function will not apply CMAP design
#'standards to the plot itself: use with \code{theme_cmap()} for that.
#'
#'Additional details can be placed here if needed.
#'
#'@usage finalize_plot2(input_plot = NULL, title = "", caption = "", mode =
#'  c("plot"), width = 6.7, height = 4, title_width = 2, resolution = 300,
#'  filepath = "", plot_margin_top = cmapplot_globals$margins$plot_top,
#'  plot_margin_right = cmapplot_globals$margins$plot_right, plot_margin_bottom
#'  = cmapplot_globals$margins$plot_bottom, plot_margin_left =
#'  cmapplot_globals$margins$plot_left, topline = cmapplot_globals$lwds$topline,
#'  topline_margin = cmapplot_globals$margins$topline_above, text_margin_left =
#'  cmapplot_globals$margins$title_left, text_margin_top =
#'  cmapplot_globals$margins$title_top, text_margin_mid =
#'  cmapplot_globals$margins$title_bottom, fill_bg = "white", fill_canvas =
#'  "gray90")
#'
#'@param input_plot ggplot object, the variable name of the plot you have
#'  created that you want to finalize. If null (the default), the most recent
#'  plot will be retrieved via \code{ggplot2::last_plot()}.
#'@param title,caption Char, the text you want to appear in the title and
#'  caption blocks. If empty, any non-Null values from \code{input_plot} will be
#'  retrieved. These blocks take html formatting, so manual text breaks can be
#'  created with \code{<br>} and formatting can be changed with \code{<span>}.
#'@param mode Vector, the action(s) to be taken with the plot. Save using any of
#'  the following: \code{png}, \code{tiff}, \code{jpeg}, \code{bmp}, \code{svg},
#'  \code{pdf}, \code{ps}. View in R with: \code{plot}, \code{window}. Return an
#'  object with \code{object}.
#'@param filepath Char, the filepath you want the plot to be saved to. You may
#'  specify an extension to use. If you don't, the correct extension will be
#'  added for you.
#'@param width Numeric, the width in inches for the image, including the title.
#'  Default = 7.
#'@param height Numeric, the height in inches for the image. Default = 4.
#'@param title_width Numeric, the width in inches for the title. Default = 2.
#'@param resolution, Numeric, the resolution of exported images (in dpi).
#'  Default = 300.
#'@param plot_margin_top Numeric, the margin between the top line and the plot
#'  (in big points). Default = 5.
#'@param plot_margin_right Numeric, the margin between the the plot and the
#'  right edge (in big points). Default = 2.
#'@param plot_margin_bottom Numeric, the margin between the the plot and the
#'  bottom edge (in big points). Default = 2.
#'@param plot_margin_left Numeric, the margin between the the plot and the
#'  title/caption (in big points). Default = 11.5.
#'@param topline Numeric, the width of the line above the title and graph  (in
#'  "big points"). Default = 3.
#'@param topline_margin Numeric, the margin between the top line and the top of
#'  the exported image (in "big points"). Default = 5.
#'@param text_margin_left Numeric, the margin to left of title and caption text
#'  (in "big points"). Default = 2.
#'@param text_margin_top Numeric, the margin between the top line and title text
#'  (in "big points"). Default = 5.
#'@param text_margin_mid Numeric, the margin between the title and caption text
#'  (in "big points"). Default = 10.
#'@param fill_bg,fill_canvas Char, strings that represent colors R can
#'  interpret. They are used to fill behind and around the finished plot,
#'  respectively.
#'
#'@return If and only if \code{"object"} is one of the modes specified, a gTree
#'  object is returned. gTree is an assembly of grobs, or graphical objects,
#'  that can be drawn using the grid package.
#'
#'@examples
#'\dontrun{
#' econ_plot <-
#'   cluster_jobchange %>%
#'   ggplot(aes(x = reorder(name, jobchange), y = jobchange, fill = category)) +
#'   geom_col() +
#'   coord_flip() +
#'   theme_cmap(gridlines = "v", hline = 0) +
#'   scale_y_continuous(labels = scales::comma)
#'
#' finalize_plot2(econ_plot,
#'                "Cluster-level employment changes in the Chicago MSA, 2001-17.",
#'                "Source: Chicago Metropolitan Agency for Planning analysis.",
#'                mode = "plot",
#'                height = 6,
#'                width = 8,
#'                title_width = 2.5,
#'                plot_margin_right = 30)
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
#'   theme_cmap(max_columns = 3)
#'
#' finalize_plot2(transit_plot,
#'                "Transit ridership in the RTA region over time, 1980-2019
#'                (in millions).",
#'                "Source: Chicago Metropolitan Agency for Planning
#'                analysis of data from the Regional Transportation Authority.",
#'                mode="window",
#'                filepath = "foo",
#'                plot_margin_right = 10)
#'}
#'@export
finalize_plot2 <- function(input_plot = NULL,
                           title = "",
                           caption = "",
                           mode = c("plot"),
                           width = 6.7,
                           height = 4,
                           title_width = 2,
                           resolution = 300,
                           filepath = "",
                           fill_bg = "white",
                           fill_canvas = "gray90",
                           overrides = list()
                           ){

  # Validation and initialization -----------------------------

  # Seek last plot if user did not specify one
  if(is.null(input_plot)){
    input_plot <- ggplot2::last_plot()
  }

  # check mode argument and validate filepath
  savetypes_raster <- c("png","tiff","jpeg","bmp")
  savetypes_vector <- c("svg","ps","pdf")
  savetypes_print <- c("plot", "window")

  mode <- match.arg(arg = mode,
                    choices = c(savetypes_raster,
                                savetypes_vector,
                                savetypes_print,
                                "object"),
                    several.ok = TRUE)

  # if any save modes specified, check for filepath
  if (length(intersect(mode, c(savetypes_raster, savetypes_vector))) > 0) {
    if (filepath == "") { stop("You must specify a filepath if saving") }
  }

  # create list of plot constants, from globals unless overridden by user
  plot_constants <- modifyList(cmapplot_globals$plot_constants, overrides)

  # for brevity, add a sum of margin_v1 and margin_v2 to list
  plot_constants <- append(plot_constants,
                           list(margin_v1_v2 =
                             plot_constants$margin_v1 + plot_constants$margin_v2),
                           after = 6
                           )

  # If title/caption unspecified, try to extract from plot
  input_title <- input_plot$labels$title
  if (title == "") {
    if(!is.null(input_title)) {
      title <- input_title
    } else {
      title <- "This plot needs a title"
    }
  }

  input_caption <- input_plot$labels$caption
  if (caption == "" & !is.null(input_caption)) {
    caption <- input_caption
  }

  # Size conversion for widths in line graphs
  default_lwd <- ggplot2::GeomLine$default_aes$size
  ggplot2::update_geom_defaults("line", list(size = plot_constants$lwd_plotline))

  # Build necessary viewports -----------------------------------------------------

  # create a parent viewport for centering the plot when drawing within R
  vp.centerframe <- viewport(
    name = "vp.centerframe",
    width = grid::unit(width, "in"),
    height = grid::unit(height, "in"),
    clip = "on"
  )

  # create viewport for non-plot content
  vp.frame <- viewport(
    name = "vp.frame",
    # origin at 0,0 of parent
    x = 0,
    y = 0,
    just = c(0,0),
    # with a size determined by user
    width = width,
    height = height,
    default.units = "in",
    clip = "on"
  )

  # create viewport for plot
  vp.plot <- viewport(
    name = "vp.plot",
    # origin shifted over `title_width` from 0,0 of parent
    x = grid::unit(title_width, "in"),
    y = 0,
    just = c(0,0),
    # size is function of remaining size available
    width = grid::unit(width - title_width, "in"),
    height = grid::unit(height, "in") - grid::unit(plot_constants$margin_v1_v2, "bigpts"),
    clip = "on"
  )

  # Build necessary grobs -----------------------------------------------------

  # grob to fill canvas (ROOT vp)
  grob_canvas <- grid::grid.rect(
    name = "canvas",
    gp = grid::gpar(fill = fill_canvas,
                    col = fill_canvas)
  )

  # grob to fill behind output (vp.frame)
  grob_background <- grid::grid.rect(
    name = "background",
    vp = vp.frame,
    gp = grid::gpar(fill = fill_bg,
                    col = fill_bg)
  )

  #  top line (vp.frame)
  grob_topline <- grid::linesGrob(
    name = "topline",
    vp = vp.frame,
    y = grid::unit(1, "npc") - grid::unit(plot_constants$margin_v1, "bigpts"),
    gp = grid::gpar(col = cmapplot_globals$colors$blackish,
                    lineend = "butt",
                    lwd = plot_constants$lwd_topline)
  )

  # title textbox (vp.frame)
  grob_title <- gridtext::textbox_grob(
    name = "title",
    text = title,
    # set location down from top left corner of vp.frame
    vp = vp.frame,
    x = grid::unit(0, "npc"),
    y = grid::unit(1, "npc") - grid::unit(plot_constants$margin_v1_v2, "bigpts"),
    hjust = 0,
    vjust = 1,
    # set dimensions
    width = grid::unit(title_width,"in"),
    maxheight = grid::unit(1, "npc") - grid::unit(plot_constants$margin_v1_v2, "bigpts"),
    # set margins within textbox
    padding = grid::unit(c(0,                        # top
                           plot_constants$margin_h2, # right
                           plot_constants$margin_v3, # bottom
                           plot_constants$margin_h1),# left
                         "bigpts"),
    # set font aesthetic variables
    gp = grid::gpar(fontsize=cmapplot_globals$font$title$size,
                    fontfamily=cmapplot_globals$font$title$family,
                    fontface=cmapplot_globals$font$title$face,
                    lineheight=plot_constants$leading_title,
                    col=cmapplot_globals$colors$blackish)
  )

  # caption textbox (vp.frame)
  grob_caption <- gridtext::textbox_grob(
    name = "caption",
    text = caption,
    # set location down from top left corner of vp.frame
    vp = vp.frame,
    x = grid::unit(0, "npc"),
    y = grid::unit(1, "npc") - grid::unit(plot_constants$margin_v1_v2, "bigpts") - grid::grobHeight(grob_title),
    hjust = 0,
    vjust = 1,
    # set dimensions
    width = grid::unit(title_width,"in"),
    maxheight = grid::unit(1,"npc") - grid::unit(plot_constants$margin_v1_v2, "bigpts") - grid::grobHeight(grob_title),
    # set margins within textbox
    padding = grid::unit(c(0,                        # top
                           plot_constants$margin_h2, # right
                           0,                        # bottom
                           plot_constants$margin_h1),# left
                         "bigpts"),
    # set aesthetic variables
    gp = grid::gpar(fontsize = cmapplot_globals$font$note$size,
                    fontfamily = cmapplot_globals$font$note$family,
                    fontface = cmapplot_globals$font$note$face,
                    lineheight = plot_constants$leading_caption,
                    col = cmapplot_globals$colors$blackish)
  )

  # ggplot as grob (vp.plot)
  grob_plot <- grid::grobTree(
    ggplotGrob(
      input_plot + ggplot2::theme(
        # make sure the plot has no title or caption
        plot.title = ggplot2::element_blank(),
        plot.caption = ggplot2::element_blank(),
        # add margins and modify text sizing  *** SEE NOTE BELOW
        plot.margin = unit(plot_constants$padding_plot, "bigpts"),
        text = ggplot2::element_text(size = cmapplot_globals$font$main$size * 1.25)
      )
    ),
    vp = vp.plot,
    name = "plot"
  )

  # NOTE: The plot appears to be resizing when exported via save (but not in plot or
  # newwindow). To have correct font sizes for export, it appears that chart
  # text must be specified at 17.5 (which is 1.25 x 14) appears to be accurately
  # rendered at 14. This could be an artifact of the points vs. pixels size
  # problem.

  # Assemble final plot -----------------------------------------------------

  final_plot <- grid::grobTree(
    grob_background, grob_topline, grob_title, grob_caption, grob_plot,
    name = "final_plot"
  )

  # Output the figure based on mode selected -----------------------------------

  for(this_mode in mode){

    # if filename does not contain correct extension, add it
    # (in print modes this functions but is meaningless)
    if (!(grepl(paste0("\\.", this_mode, "$"), filepath))) {
      this_filepath <- paste0(filepath, ".", this_mode)
    } else {
      this_filepath <- filepath
    }

    # export as raster
    if (this_mode %in% savetypes_raster) {

      # Open the device
      do.call(this_mode,
              list(filename = this_filepath,
                   type = "cairo",
                   width = width,
                   height = height,
                   units = "in",
                   res = resolution))

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
              list(filename = this_filepath,
                   width = width,
                   height = height))

      # draw the plot and close the device
      grid::grid.draw(final_plot)
      dev.off()

      message(paste("Export successful:", this_mode))

    # OR print the grob
    } else if (this_mode %in% savetypes_print) {

      # In window mode, open a new window
      if (this_mode == "window") {
        grDevices::dev.new(width = width * 1.02,
                           height = height * 1.02,
                           noRStudioGD = TRUE)
      }

      # set up blank canvas
      grid::grid.newpage()
      grid::grid.draw(grob_canvas)

      # enter centerframe, draw plot, exit centerframe
      grid::pushViewport(vp.centerframe)
      grid::grid.draw(final_plot)
      grid::popViewport()

      # In window mode, deactivate the new window
      if (this_mode == "window") {
        grDevices::dev.next()
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





####### ARCHIVE - original version of finalize_plot
# Arrange and save CMAP ggplot chart
#
# Running this function will save your plot with CMAP infographic standards.
# It will align your title and subtitle to the left, add a horizontal line on top, and save it to your specified location.
# title <- "Change in labor force size per 1,000 residents, by age, Chicago and select Metropolitan Statistical Areas, 2006-10 to 2013-17"
# subtitle <- "Source: Chicago Metropolitan Agency for Planning analysis of National Highway Traffic Safety Administration Corporate Average Fuel Economy Fact Sheets; Illinois Department of Transportation data; 2009 National Household Travel Survey data."
#
# myplot <- ggplot(economy_basic, aes(x = interaction(year, variable), y = value, fill = sector)) +
#   geom_col(position = "fill") +
#   scale_y_continuous(labels = scales::percent) + theme_cmap()
#
#
# finalize_plot(myplot, title, subtitle, action='save', type='web', titlewraplen = 14, subtitleheight = 0.55, save_filepath = '/Users/sarahbuchhorn/Desktop/test7.png')
#
#
# save_plot <- function (plot_grid, save_filepath, type, height=NA) {
#   # use 72 px/in conversion
#   width_pixels = 670
#
#   if (!type %in% c("web","word",'ppt')) {
#     stop("Type must be 'web', 'word', or 'ppt'")
#   }
#
#   if (type == 'web'){
#     if (grepl('.svg',save_filepath) == FALSE) {
#       paste(save_filepath, '.svg', sep='')
#     }
#   }
#
#   if (type == 'word'){
#     if (grepl('.tiff',save_filepath) == FALSE) {
#       paste(save_filepath, '.tiff', sep='')
#     }
#     if (height > 400){
#       warning("max height is 400 px for this type")
#     }
#   }
#
#   if (type == 'ppt'){
#     if (grepl('.jpeg',save_filepath) == FALSE) {
#       paste(save_filepath, '.jpeg', sep='')
#     }
#     if (height > 400){
#       warning("max height is 400 px for this type")
#     }
#   }
#
#   grid::grid.draw(plot_grid)
#
#   #save it
#   ggplot2::ggsave(filename = save_filepath,
#                   plot=plot_grid,
#                   width=(width_pixels/72), height=(height/72),
#                   bg="white")
# }
#
#
# create_title_block <- function (title, subtitle, sheight) {
#   title_block <- grid::grobTree(
#     grid::textGrob(title,
#                    x = 0.1, hjust = 0, y = 1, vjust = 1,
#                    gp = grid::gpar(fontsize=cmapplot_globals$font$title$size,
#                                    fontfamily=cmapplot_globals$font$title$family,
#                                    fontface=cmapplot_globals$font$title$face,
#                                    lineheight=0.93)),
#     grid::textGrob(subtitle,
#                    x = 0.1, hjust = 0, vjust = 1, y = sheight,
#                    gp = grid::gpar(fontsize=cmapplot_globals$font$note$size,
#                                    fontfamily=cmapplot_globals$font$note$family,
#                                    fontface=cmapplot_globals$font$note$face,
#                                    lineheight=1))
#   )
#
#   return(title_block)
#
# }
#
# finalize_plot <- function(plot,
#                           title,
#                           subtitle,
#                           save_filepath,
#                           height_pixels=400,
#                           type,
#                           sidepercent=23,
#                           autowrap=TRUE,
#                           titlewraplen=22,
#                           subtitlewraplen=28,
#                           subtitleheight=0.72,
#                           action=view,
#                           newwindow=FALSE) {
#
#   # word wrap
#   if (autowrap == TRUE){
#     tpieces <- stringi::stri_wrap(title, titlewraplen, cost_exponent=2, whitespace_only=TRUE)
#     titlebreak <- stringi::stri_paste_list(list(tpieces), sep="\n")
#     stpieces <- stringi::stri_wrap(subtitle, subtitlewraplen, cost_exponent=2, whitespace_only=TRUE)
#     subtitlebreak <- stringi::stri_paste_list(list(stpieces), sep="\n")
#
#     side <- create_title_block(titlebreak, subtitlebreak, subtitleheight)
#   }
#
#   if (autowrap == FALSE){
#     side <- create_title_block(title, subtitle, subtitleheight)
#
#   }
#
#   line <- grid::linesGrob(x = grid::unit(c(0,1), "npc"),
#                           y = grid::unit(c(0.92,0.92), "npc"),
#                           gp=grid::gpar(col='black',
#                                         lwd=3))
#
#   space <- grid::rectGrob(width = 1, gp=grid::gpar(fill='white',col='white',lwd=0))
#
#   bottom <- ggpubr::ggarrange(side, space, plot,
#                               ncol=3, nrow=1,
#                               widths = c(1, (2/sidepercent), ((98/sidepercent) - 1)))
#
#   plot_grid <- ggpubr::ggarrange(line, bottom,
#                                  ncol = 1, nrow = 2,
#                                  heights = c(0.03,1))
#
#   if (action=='view') {
#     width_pixels = 670
#     if (newwindow==TRUE) {
#       if (.Platform$OS.type == "windows") {
#         windows(width=(width_pixels/72),
#                 height=(height_pixels/72))
#         return(plot_grid)
#       } else {
#         dev.new(width=(width_pixels/72),
#                 height=(height_pixels/72),
#                 noRStudioGD = TRUE)
#         return(plot_grid)
#       }
#     }
#     if (newwindow==FALSE) {
#       return(plot_grid)
#     }
#   }
#
#   if (action=='save') {
#     save_plot(plot_grid, save_filepath, type=type, height=height_pixels)
#   }
# }





