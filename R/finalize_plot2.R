#' @importFrom grid gpar unit unit.c

# for documentation - title and subtitle text takes HTML. Use <br> to force line breaks.

# # SAMPLE CODE
# myplot <- ggplot(economy_basic, aes(x = interaction(year, variable), y = value, fill = sector)) +
#   geom_col(position = "fill") +
#   scale_y_continuous(labels = scales::percent) + theme_cmap()
#
# finalize_plot2(myplot, "title is<br>long so it might take many lines", "subtitle keeps going and going!")



# testing testing testing
title <- "Change in labor force size per 1,000 residents, by age, Chicago and select Metropolitan Statistical Areas, 2006-10 to 2013-17"
subtitle <- "Source: Chicago Metropolitan Agency for Planning analysis of National Highway Traffic Safety Administration Corporate Average Fuel Economy Fact Sheets; Illinois Department of Transportation data; 2009 National Household Travel Survey data."


finalize_plot2(myplot,title,subtitle,mode="save",save_filepath = "foo.png",type = "ppt", height = 400)




### Actual code below

save_plot <- function (plot_grid, save_filepath, type, height=NA) {
  # use 72 px/in conversion
  width_pixels = 670

  if (!type %in% c("web","word",'ppt')) {
    stop("Type must be 'web', 'word', or 'ppt'")
  }

  if (type == 'web'){
    if (grepl('.svg',save_filepath) == FALSE) {
      paste(save_filepath, '.svg', sep='')
    }
  }

  if (type == 'word'){
    if (grepl('.tiff',save_filepath) == FALSE) {
      paste(save_filepath, '.tiff', sep='')
    }
    if (height > 400){
      warning("max height is 400 px for this type")
    }
  }

  if (type == 'ppt'){
    if (grepl('.png',save_filepath) == FALSE) {
      paste(save_filepath, '.png', sep='')
    }
    if (height > 400){
      warning("max height is 400 px for this type")
    }
  }

  grid::grid.draw(plot_grid)

  #save it
  ggplot2::ggsave(filename = save_filepath,
                  plot=plot_grid,
                  width=(width_pixels/72), height=(height/72),
                  dpi = 72,
                  bg="white")
}


#' @export
finalize_plot2 <- function(plot = ggplot2::last_plot(),
                           title = "Title here",
                           subtitle = "Subtitle here",
                           mode = c("plot", "newwindow", "save"), # expand to include save formats (e.g. svg, pdf, etc)?
                           width = 670,
                           height = 400,
                           title_width = 150,
                           save_filepath,
                           type = c("ppt","web","word")){

  mode <- match.arg(mode)

  # TEMPORARY convert pixel sizes into something manageable
  # width <- width / 2
  # height <- height / 2
  # title_width <- title_width / 2

  # function constants
  # (***should these be function arguments?***)
  c_topbar_lwd <- unit(3,"bigpts") # width of top line
  c_textmargin_left <- unit(2,"bigpts") # margin to left of title and subtitle text
  c_textmargin_top <- unit(5,"bigpts") # margin between top line and title
  c_textmargin_mid <- unit(10,"bigpts") # margin between title and subtitle

  # establish rectangle grob for top line
  grob_topline <- grid::rectGrob(gp=gpar(fill = cmapplot_globals$colors$blackish,
                                         col = "white",
                                         lwd=0))

  # establish title grob
  grob_title <- gridtext::textbox_grob(text = title,
                                       # set top left location of grob
                                       x = unit(0, "npc"),
                                       y = unit(1, "npc"),
                                       hjust = 0,
                                       vjust = 1,
                                       # set margins within textbox
                                       padding = unit.c(c_textmargin_top, # top
                                                        unit(1, "bigpts"), # right
                                                        unit(1, "bigpts"), # bottom
                                                        c_textmargin_left),# left
                                       # set aesthetic variables
                                       gp = gpar(fontsize=cmapplot_globals$font_sizes$title,
                                                 fontfamily=cmapplot_globals$font_title,
                                                 fontface=cmapplot_globals$font_title_face,
                                                 lineheight=0.93)
                                       # # for debug, draw box around grob
                                       # , box_gp = gpar(col = "blue", fill = "cornsilk")
                                       )

  # establish subtitle grob
  grob_subtitle <-  gridtext::textbox_grob(text = subtitle,
                                           # set top left location of grob
                                           x = unit(0, "npc"),
                                           y = unit(1, "npc") - grid::grobHeight(grob_title),
                                           hjust = 0,
                                           vjust = 1,
                                           # set margins within textbox
                                           padding = unit.c(c_textmargin_mid, # top
                                                            unit(1, "bigpts"), # right
                                                            unit(1, "bigpts"), # bottom
                                                            c_textmargin_left),# left
                                           # set aesthetic variables
                                           gp = gpar(fontsize=cmapplot_globals$font_sizes$note,
                                                     fontfamily=cmapplot_globals$font_reg,
                                                     fontface=cmapplot_globals$font_reg_face,
                                                     lineheight=0.93)
                                           # # for debug, draw box around grob
                                           # , box_gp = gpar(col = "blue", fill = "lavenderblush")
  )

  # TO BE ADDED : Size conversion for widths in line graphs

  # add margin between plot elements and titles
  final_plot <- myplot +
    theme(plot.margin = unit(c(5,5,5,10),"pt"))

  # establish matrix shape for three viewports
  layout = rbind(c(1,1),
                 c(2,3))


  # stitch together the final plot
  output <- gridExtra::arrangeGrob(grobs = list(grob_topline,
                                                grobTree(grob_title, grob_subtitle),
                                                final_plot),
                                   layout_matrix = layout,
                                   heights = unit.c(c_topbar_lwd, unit(height, "bigpts") - c_topbar_lwd),
                                   widths = unit(c(title_width, width - title_width), "bigpts"))


  # # as debug tool, print viewport layouts to plot window
  # gtable::gtable_show_layout(output)


  # output the figure based on user setting
  if (mode == "object") {
    # just return the output as a TableGrob
    return(output)
  } else if (mode == "save") {
    save_plot(output,save_filepath,type,height)
  }
  else if (mode == "plot") {
    # print the grob to the plots window
    grid::grid.newpage()
    grid::grid.draw(output)
  } else if (mode == "newwindow") {
    # print the grob to a new graphics window
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

