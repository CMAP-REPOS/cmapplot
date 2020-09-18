#' @importFrom grid gpar unit unit.c

# for documentation - title and subtitle text takes HTML. Use <br> to force line breaks.

# # SAMPLE CODE
# myplot <- ggplot(economy_basic, aes(x = interaction(year, variable), y = value, fill = sector)) +
#   geom_col(position = "fill") +
#   scale_y_continuous(labels = scales::percent) + theme_cmap()
# #
# finalize_plot2(myplot, "title is<br>long so it might take many lines", "subtitle keeps going, and going, and going!", mode = "svg", filepath = "foo.svg")
#
#
#
# # # testing testing testing
# title <- "Change in labor force size per 1,000 residents, by age, Chicago and select Metropolitan Statistical Areas, 2006-10 to 2013-17"
# subtitle <- "Source: Chicago Metropolitan Agency for Planning analysis of National Highway Traffic Safety Administration Corporate Average Fuel Economy Fact Sheets; Illinois Department of Transportation data; 2009 National Household Travel Survey data."
#
#
# finalize_plot2(myplot,title,subtitle,filepath = "foo",mode = "pdf", height = 400)
#
#
# myplot2 <- transit_ridership %>%
#   ggplot(aes(x = year, y = ridership, color = system)) +
#   geom_line() +
#   theme_cmap()
#
# title2 <- "Transit ridership in the RTA region over time, 1980-2019 (in millions)"
# subtitle2 <- "Source: Chicago Metropolitan Agency for Planning analysis of data from the Regional Transit Authority"
#
# finalize_plot2(myplot2,title2,subtitle2,filepath = "too", mode="png")

### Actual code below


#' @export
finalize_plot2 <- function(plot = ggplot2::last_plot(),
                           title = "Title here",
                           subtitle = "Subtitle here",
                           mode = c("plot", "newwindow", "object", "svg", "png", "tiff","pdf"),
                           width = 670,
                           height = 400,
                           title_width = 150,
                           filepath = NULL
                           ){

  # Validation and initialization -----------------------------

  # check mode argument
  mode <- match.arg(mode)

  # validate output details
  savetypes <- c("svg", "png", "tiff","pdf")
  if(mode %in% savetypes){
    # check for filepath
    if(is.null(filepath)){stop("You must specify a filepath in save mode")}

    # if extension does not contain correct extension, add it.
    if(!grepl(paste0("//.", mode, "$"), filepath)){
      filepath <- paste0(filepath, ".", mode)
    }

    # if mode is SVG, confirm svglite package.
    if(mode == "svg"){
      if (!("svglite" %in% rownames(installed.packages()))) {
        stop("To export as SVG, package `svglite` is required.")
      }
    }
  }

  # validate height and width
  if(width != 670){warning("Width should typically be 670 px exactly.")}
  if(height > 400){warning("Height should typically be 400 px or less.")}

  # function constants
  # (***should these be function arguments?***)
  c_topbar_lwd <- grid::unit(3,"bigpts") # width of top line
  c_textmargin_left <- grid::unit(2,"bigpts") # margin to left of title and subtitle text
  c_textmargin_top <- grid::unit(5,"bigpts") # margin between top line and title
  c_textmargin_mid <- grid::unit(10,"bigpts") # margin between title and subtitle
  c_plotmargins <- grid::unit(c(5,5,5,10),"bigpts") #margins around plot, clockwise from top

  # modify the plot a bit to fix margins
  plot <- plot + theme(plot.margin = c_plotmargins,
                       plot.title = ggplot2::element_blank())

  # FLAG FOR REVIEW BY NOEL - Fix discrepancy in fonts for exporting to svg and pdf
  if (.Platform$OS.type == "windows") {
    plot <- plot + theme(text = element_text(family = windowsFonts(cmapplot_globals$font_main)))
  } else {
    plot <- plot + theme(text = element_text(family = X11Fonts(cmapplot_globals$font_main)))
  }

  # Size conversion for widths in line graphs
  old_lwd <- GeomLine$default_aes$size
  update_geom_defaults("line", list(size = cmapplot_globals$lwd_layout))

  # Build necessary grobs -----------------------------------------------------

  # rectangle grob for top line
  grob_topline <- grid::rectGrob(gp=grid::gpar(fill = cmapplot_globals$colors$blackish,
                                         col = "white",
                                         lwd=0))

  # title grob
  grob_title <- gridtext::textbox_grob(text = title,
                                       # set top left location of grob
                                       x = grid::unit(0, "npc"),
                                       y = grid::unit(1, "npc"),
                                       hjust = 0,
                                       vjust = 1,
                                       # set margins within textbox
                                       padding = grid::unit.c(c_textmargin_top, # top
                                                              grid::unit(1, "bigpts"), # right
                                                              grid::unit(1, "bigpts"), # bottom
                                                              c_textmargin_left),# left
                                       # set aesthetic variables
                                       gp = grid::gpar(fontsize=cmapplot_globals$font_sizes$title,
                                                       # FLAG FOR REVIEW BY NOEL - fix for font family that was causing problems in SVG and PDF exports
                                                       fontfamily= if (.Platform$OS.type == "windows") {
                                                         windowsFonts(cmapplot_globals$font_title)} else {
                                                            X11Fonts(cmapplot_globals$font_title)},
                                                       fontface=cmapplot_globals$font_title_face,
                                                       lineheight=0.93)
                                       # # for debug, draw box around grob
                                       # , box_gp = gpar(col = "blue", fill = "cornsilk")
                                       )

  # subtitle grob
  grob_subtitle <-  gridtext::textbox_grob(text = subtitle,
                                           # set top left location of grob
                                           x = grid::unit(0, "npc"),
                                           y = grid::unit(1, "npc") - grid::grobHeight(grob_title),
                                           hjust = 0,
                                           vjust = 1,
                                           # set margins within textbox
                                           padding = grid::unit.c(c_textmargin_mid, # top
                                                                  grid::unit(1, "bigpts"), # right
                                                                  grid::unit(1, "bigpts"), # bottom
                                                                  c_textmargin_left),# left
                                           # set aesthetic variables
                                           gp = grid::gpar(fontsize=cmapplot_globals$font_sizes$note,
                                                           # FLAG FOR REVIEW BY NOEL - fix for font family that was causing problems in SVG and PDF exports
                                                           fontfamily= if (.Platform$OS.type == "windows") {
                                                             windowsFonts(cmapplot_globals$font_main)} else {
                                                               X11Fonts(cmapplot_globals$font_main)},
                                                           fontface=cmapplot_globals$font_main_face,
                                                           lineheight=0.93)
                                           # # for debug, draw box around grob
                                           # , box_gp = gpar(col = "blue", fill = "lavenderblush")
  )


  # Assemble final plot -----------------------------------------------------

  # establish matrix shape for three viewports
  layout = rbind(c(1,1),
                 c(2,3))


  # stitch together the final plot
  output <- gridExtra::arrangeGrob(grobs = list(grob_topline,
                                                grobTree(grob_title, grob_subtitle),
                                                plot),
                                   layout_matrix = layout,
                                   heights = grid::unit.c(c_topbar_lwd,
                                                          unit(height, "bigpts") - c_topbar_lwd),
                                   widths = grid::unit(c(title_width, width - title_width),
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
                    height=(height/72),
                    dpi = 72,
                    bg="white",
                    # If PDF, switch device to "cairo" for better PDF handling
                    device = if (mode == "pdf") {cairo_pdf} else {mode}
                    )
    message("Export successful")
  } else if (mode == "plot") {
    # OR print the grob to the plots window
    grid::grid.newpage()
    grid::grid.draw(output)
  } else if (mode == "newwindow") {
    # OR print the grob to a new graphics window
    if (.Platform$OS.type == "windows") {
      windows(width=(width/72),
              height=(height/72))
      grid::grid.newpage()
      grid::grid.draw(output)
    } else {
      dev.new(width=(width/72),
              height=(height/72),
              noRStudioGD = TRUE)
      return(output)
    }
  }

  # return geom defaults as before
  update_geom_defaults("line",list(size = old_lwd))
}



# # old title and subtitle grobs
# grob_title <-  grid::textGrob(label = title,
#                               # set top left location of grob
#                               x = c_textmargin_left,
#                               y = unit(1, "npc") - c_textmargin_top,
#                               just = c("left", "top"),
#                               # set aesthetic variables
#                               gp = gpar(fontsize=cmapplot_globals$font_sizes$title,
#                                         fontfamily=cmapplot_globals$font_title,
#                                         fontface=cmapplot_globals$font_title_face,
#                                         lineheight=0.93))
#
# grob_subtitle <-  grid::textGrob(label = subtitle,
#                                  # set top left location of grob
#                                  x = c_textmargin_left,
#                                  y = unit(1, "npc") - grid::grobHeight(grob_title) - c_textmargin_top - c_textmargin_mid,
#                                  just = c("left", "top"),
#                                  # set aesthetic variables
#                                  gp = gpar(fontsize=cmapplot_globals$font_sizes$note,
#                                            fontfamily=cmapplot_globals$font_reg,
#                                            fontface=cmapplot_globals$font_reg_face,
#                                            lineheight=0.93))

# # # ALTERNATIVE, MORE COMPLICATED LAYOUT
# # establish layout of viewports to draw into
# layout = rbind(c(1,1),
#                c(2,4),
#                c(3,4),
#                c(NA,4))
#
# # stitch together the final plot
# output <- gridExtra::arrangeGrob(grobs = list(grob_rect, grob_title, grob_subtitle, myplot),
#                         layout_matrix = layout,
#                         heights = unit(c(3, 10, 5, 150), "points"),
#                         widths = unit(c(80, 220), "points"))

# grid.newpage()
# grobTree(title, subtitle) %>%
#   grid.draw()

#how to use grobHeight to automatically adjust location of subtitle?

# this attempt has the title and subtitle in separate matrix locations



# this attempt has the title and subtitle in a single matrix

# # note arrangeGrob is same as grid.arrange but does not draw
# layout2 = rbind(c(1,1),
#                 c(2,3))
#
# grid.arrange(grobs = list(rect, grobTree(title, subtitle), myplot),
#              layout_matrix = layout2,
#              heights = unit(c(3, 200), "points"),
#              widths = unit(c(80, 220), "points"))


# help
# https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
# https://stackoverflow.com/questions/59012782/ggplot2-how-to-align-grobs-to-sides-using-arrangegrob


### OLD


# # problem is title placement features are abandoned here. How to place top left in grid?
# title_block <- arrangeGrob(title, title, ncol = 1,
#              heights = unit(c(3, 3), "lines")) # How many lines of height?




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

