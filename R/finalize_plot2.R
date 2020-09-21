#'Arrange and save CMAP ggplot chart
#'
#'\code{finalize_plot2} will save a ggplot into a frame defined by  CMAP design
#'standards. It will align your title and subtitle to the left, add a horizontal
#'line on top, and make other adjustments. If instructed to do so, it will also
#'save the plot in one of four file formats (png, tiff, svg, and pdf) to a
#'specified location. This function will not apply CMAP design standards to the
#'plot itself: use with \code{theme_cmap()} for that.
#'
#'@param plot ggplot object, the variable name of the plot you have created that
#'  you want to finalize. The efault is \code{ggplot2::last_plot()}, so if
#'  unspecified, the most recent plot will be used.
#'@param title Char, the text you want to appear in the title block.
#'@param subtitle Char, the text you want to appear in the subtitle block.
#'@param mode Char, the action to be taken with the plot. Default is to view in
#'  the window (using "plot"). Other options include "newwindow" (displays in a
#'  new window), "object" (returns a grob object, but does not print), or one of
#'  four file extensions that can be saved (png, tiff, svg, and pdf).
#'@param filepath Char, the filepath you want the plot to be saved to.  You can
#'  specify an extension to use, or an extension will be added if you specify it
#'  in "mode".
#'@param width Numeric, the width in pixels for the image, including the title.
#'  Default = 670.
#'@param height Numeric, the height in pixels for the image. Default = 400.
#'@param title_width Numeric, the width in pixels for the title. Default = 150.
#'@param plot_margins Vector of units, the margins around the elements of the
#'  plot within the plot object. This requires a vector of 4 "unit" elements,
#'  defining the margins clockwise starting from the top. The default is 5, 5,
#'  5, and 10 big points  (1/72 of an inch) on the top, right, bottom, and left,
#'  respectively. Inputs should be formatted as c(`top`,`right`,`bottom`,`left`)
#'@param top_bar Numeric, the width of the line above the title and graph in
#'  big points. Default is 3 "big points."
#'@param text_margin_left Unit, the margin to left of title and subtitle text.
#'  Default is 2 "big points."
#'@param text_margin_top Unit, the margin between the top line and title text.
#'  Default is 5 "big points."
#'@param text_margin_mid Unit, the margin between the title and subtitle text.
#'  Default is 10 "big points."
#'
#'@importFrom grid gpar unit unit.c grobTree
#'@importFrom utils installed.packages
#'
#' @examples
#' \dontrun{
#' econ_plot <-
#'   cluster_jobchange %>%
#'   ggplot(aes(x = reorder(name, jobchange), y = jobchange, fill = category)) +
#'   geom_col() +
#'   coord_flip() +
#'   theme_cmap(gridlines = "v", hline = 0) +
#'   scale_y_continuous(labels = scales::comma)
#'
#' finalize_plot2(econ_plot,
#'                "Change in employment in specified clusters in the Chicago
#'                Metropolitan Statistical Area, 2001-17.",
#'                "Source: Chicago Metropolitan Agency for Planning analysis
#'                of traded clusters.",
#'                mode = "plot",
#'                filepath = "foo",
#'                plot_margins = c(5,20,5,10))
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
finalize_plot2(transit_plot,
               "Transit ridership in the RTA region over time, 1980-2019
               (in millions)",
               "Source: Chicago Metropolitan Agency for Planning
               analysis of data from the Regional Transportation Authority.",
               mode="pdf",
               filepath = "foo",
               height = 300,
               title_width = 200,
               width = 800)
#' }
#'@export
finalize_plot2 <- function(plot = ggplot2::last_plot(),
                           title = "Title here",
                           subtitle = "Subtitle here",
                           mode = c("plot", "newwindow", "object", "svg", "png", "tiff","pdf"),
                           width = 670,
                           height = 400,
                           title_width = 150,
                           filepath = NULL,
                           plot_margins = c(5,5,5,10),
                           top_bar = 2,
                           text_margin_left = 2,
                           text_margin_top = 5,
                           text_margin_mid = 10,
                           max_columns = NA
                           ){

  # Validation and initialization -----------------------------

  # check mode argument
  mode <- match.arg(mode)

  # check plot margins argument
  if (length(plot_margins) != 4){stop("Plot margins must be a vector of length four")}

  # validate output details
  savetypes <- c("svg", "png", "tiff","pdf")
  if(mode %in% savetypes){
    # check for filepath
    if(is.null(filepath)){stop("You must specify a filepath in save mode")}

    # if extension does not contain correct extension, add it
    if(!(grepl(paste0("\\.",mode, "$"), filepath))){
      filepath <- paste0(filepath, ".", mode)
    }

    # if mode is SVG, confirm svglite package.
    if(mode == "svg"){
      if (!("svglite" %in% rownames(utils::installed.packages()))) {
        stop("To export as SVG, package `svglite` is required.")
      }
    }
  }

  # validate height and width
  if(width != 670){warning("Width should typically be 670 px exactly.")}
  if(height > 400){warning("Height should typically be 400 px or less.")}

  # validate top bar
  if(top_bar != 2){warning("Top bar should typically be 2 big points exactly")}

  # validate margins
  if(text_margin_left != 2){warning("Margin between left edge and title text should typically be 2 big points exactly")}
  if(text_margin_top != 5){warning("Margin between top bar and title should typically be 5 big points exactly")}
  if(text_margin_mid != 10){warning("Margin between title and subtitle should typically be 10 big points exactly")}


  # convert top bar and margins to big points
  top_bar <- grid::unit(top_bar_lwd,"bigpts")
  text_margin_left <- grid::unit(text_margin_left,"bigpts")
  text_margin_top <- grid::unit(text_margin_top,"bigpts")
  text_margin_mid <- grid::unit(text_margin_mid,"bigpts")
  plot_margins <- grid::unit(plot_margins,"bigpts")



  # FLAG FOR REVIEW AND FURTHER THOUGHT - modify the plot a bit to fix margins
  # and text size on plot
  plot <- plot + theme(plot.margin = plot_margins,
                       plot.title = ggplot2::element_blank(),
                       text = ggplot2::element_text(size = cmapplot_globals$font_sizes$main * 1.25))
                       # The plot appears to be resizing when exported via save
                       # (but not in plot or newwindow). To have correct font
                       # sizes for export, it appears that chart text must be
                       # specified at 17.5 (which is 1.25 x 14, or 96/72 x 14)
                       # appears to be accurately rendered at 14. This could be
                       # an artifact of the points vs. pixels size problem.

  # Size conversion for widths in line graphs (this is ignored in calls that
  # return a grob object, as it is not yet drawn)
  if (mode != "object") {
  default_lwd <- GeomLine$default_aes$size
  update_geom_defaults("line", list(size = cmapplot_globals$lwd_layout))
  }

  # Build necessary grobs -----------------------------------------------------

  # rectangle grob for top line
  grob_topline <- grid::rectGrob(gp=grid::gpar(fill = cmapplot_globals$colors$blackish,
                                               col = "white"))

  # title grob
  grob_title <- gridtext::textbox_grob(text = title,
                                       # set top left location of grob
                                       x = grid::unit(0, "npc"),
                                       y = grid::unit(1, "npc"),
                                       hjust = 0,
                                       vjust = 1,
                                       # set margins within textbox
                                       padding = grid::unit.c(text_margin_top, # top
                                                              grid::unit(1, "bigpts"), # right
                                                              grid::unit(1, "bigpts"), # bottom
                                                              text_margin_left),# left
                                       # set font aesthetic variables
                                       gp = grid::gpar(fontsize=cmapplot_globals$font_sizes$title,
                                                       fontfamily=cmapplot_globals$font_title,
                                                       fontface=cmapplot_globals$font_title_face,
                                                       lineheight=0.93,
                                                       col=cmapplot_globals$colors$blackish)
                                       # # for debug, draw box around grob
                                       # , box_gp = gpar(col = "blue", fill = "cornsilk")
                                       )

  # subtitle grob
                                           # note that color is defined using
                                           # html-style "span" tags
  grob_subtitle <-  gridtext::textbox_grob(text = subtitle,
                                           # set top left location of grob
                                           x = grid::unit(0, "npc"),
                                           y = grid::unit(1, "npc") - grid::grobHeight(grob_title),
                                           hjust = 0,
                                           vjust = 1,
                                           # set margins within textbox
                                           padding = grid::unit.c(text_margin_mid, # top
                                                                  grid::unit(1, "bigpts"), # right
                                                                  grid::unit(1, "bigpts"), # bottom
                                                                  text_margin_left),# left
                                           # set aesthetic variables
                                           gp = grid::gpar(fontsize=cmapplot_globals$font_sizes$note,
                                                           fontfamily=cmapplot_globals$font_note,
                                                           fontface=cmapplot_globals$font_note_face,
                                                           lineheight=0.93,
                                                           col=cmapplot_globals$colors$blackish)
                                           # # for debug, draw box around grob
                                           # , box_gp = gpar(col = "blue", fill = "lavenderblush")
  )


  # Assemble final plot -----------------------------------------------------

  # establish matrix shape for three viewports
  layout <- rbind(c(3,3),
                 c(2,1))

  # stitch together the final plot
  output <- gridExtra::arrangeGrob(grobs = list(plot,
                                                grid::grobTree(grob_title, grob_subtitle),
                                                grob_topline),
                                   layout_matrix = layout,
                                   # establish specific dimensions for each column and row
                                   heights = grid::unit.c(top_bar,
                                                          unit(height, "bigpts") - top_bar),
                                   widths = grid::unit(c(title_width,
                                                         width - title_width),
                                                         "bigpts"))


  # # as debug tool, print viewport layouts to plot window
  # gtable::gtable_show_layout(output)

  # output the figure based on user setting -----------------------------------

  if (mode == "object") {
    # return output as TableGrob object
    return(output)
  } else if (mode %in% savetypes) {
    # OR send to ggsave
    ggplot2::ggsave(filename = filepath,
                    plot = output,
                    width = (width/72),
                    height = (height/72),
                    dpi = 72,
                    bg = "white",
                    # If PDF, switch device to "cairo" for better PDF handling
                    device = if (mode == "pdf") {cairo_pdf} else {mode}
                    )
    message("Export successful")
  } else if (mode == "plot") {
    # OR print the grob to the plots window
    grid::grid.newpage()
    grid::grid.draw(output)
  } else if (mode == "newwindow") {
    # TO CONFIRM - does this work on Mac?
    dev.new(width = (width/72),
            height = (height/72),
            noRStudioGD = TRUE)
    grid::grid.newpage()
    grid::grid.draw(output)
    # Reset device to original (for future plots, if needed)
    dev.next()
  }

  # return geom defaults as before
  ggplot2::update_geom_defaults("line",list(size = default_lwd))
}



# # visualisation experiment
# line <- grid::linesGrob(x = grid::unit(c(0,1), "npc"),
#                         y = grid::unit(c(0.92,0.92), "npc"),
#                         gp=grid::gpar(col='black',
#                                       lwd=3))
#
# line2 <- grid::linesGrob(x = grid::unit(c(0,1), "npc"),
#                         y = grid::unit(c(1,1), "inches"),
#                         gp=grid::gpar(col='black',
#                                       lwd=3))
#
# line3 <- grid::linesGrob(x = grid::unit(c(0,1), "npc"),
#                          y = grid::unit(c(3,3), "points"),
#                          gp=grid::gpar(col='black',
#                                        lwd=3))
#
#
# grid.arrange(line, line2, line3, nrow = 1)

