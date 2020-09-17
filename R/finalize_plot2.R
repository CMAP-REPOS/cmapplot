#library(ggpubr)
#library(cowplot)
#library(gtable)

#' @importFrom grid gpar unit unit.c
#'

# for documentation - title and subtitle text takes HTML. Use <br> to force line breaks.

# SAMPLE CODE
# myplot <- ggplot(economy_basic, aes(x = interaction(year, variable), y = value, fill = sector)) +
#   geom_col(position = "fill") +
#   scale_y_continuous(labels = scales::percent) + theme_cmap() +
#   theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
#         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
#         legend.background = element_rect(fill = "transparent"), # get rid of legend bg
#         )
# finalize_plot2(myplot, "title is<br>long so it might take two lines", "subtitle keeps going and going!")

#' @export
finalize_plot2 <- function(plot = ggplot2::last_plot(),
                           title = "Title here",
                           subtitle = "Subtitle here",
                           mode = c("plot", "newwindow", "output"), # expand to include output formats (e.g. svg, pdf, etc)?
                           width = 670,
                           height = 300,
                           title_width = 200){

  mode <- match.arg(mode)

  # TEMPORARY convert pixel sizes into something managable
  width <- width / 2
  height <- height / 2
  title_width <- title_width / 2

  # function constants
  c_topbar_lwd <- unit(3, "points") # width (points) of top line
  c_textmargin_left <- unit(2, "points") # margin (points) to left of title and subtitle text
  c_textmargin_top <- unit(5, "points") # margin (points) between top line and title
  c_textmargin_mid <- unit(10, "points") # margin (points) between title and subtitle

  # establish rectangle grob for top line
  grob_rect <- grid::rectGrob(gp=gpar(fill = cmapplot_globals$colors$blackish, lwd=0))

  # establish title grob
  grob_title <- gridtext::textbox_grob(text = title,
                                       # set top left location of grob
                                       x = unit(0, "npc"),
                                       y = unit(1, "npc"),
                                       hjust = 0,
                                       vjust = 1,
                                       # set margins within textbox
                                       padding = unit.c(c_textmargin_top, # top
                                                        unit(0, "points"), # right
                                                        unit(0, "points"), # bottom
                                                        c_textmargin_left),# left
                                       # set aesthetic variables
                                       gp = gpar(fontsize=17,
                                                 fontfamily=cmapplot_globals$font_title,
                                                 fontface=cmapplot_globals$font_title_face,
                                                 lineheight=0.93)
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
                                                            unit(0, "points"), # right
                                                            unit(0, "points"), # bottom
                                                            c_textmargin_left),# left
                                           # set aesthetic variables
                                           gp = gpar(fontsize=11,
                                                     fontfamily=cmapplot_globals$font_reg,
                                                     fontface=cmapplot_globals$font_reg_face,
                                                     lineheight=0.93)
                                           )

  # establish matrix shape for three viewports
  layout = rbind(c(1,1),
                  c(2,3))

  # stitch together the final plot
  output <- gridExtra::arrangeGrob(grobs = list(grob_rect, grobTree(grob_title, grob_subtitle), myplot),
                                   layout_matrix = layout,
                                   heights = unit.c(c_topbar_lwd, unit(height, "points") - c_topbar_lwd),
                                   widths = unit(c(title_width, width - title_width), "points"))


  # as debug tool, print viewport layouts to plot window
  gtable::gtable_show_layout(output)


  # output the figure based on user setting
  if (mode == "object") {
    # just return the output as a TableGrob
    return(output)
  } else if (mode == "plot") {
    # print the grob to the plots window
    grid::grid.newpage()
    grid::grid.draw(output)
  } else if (mode == "newwindow") {
    # print the grob to a new graphics window
    if (.Platform$OS.type == "windows") {
      windows(width=(width*2/72),
              height=(height*2/72))
      grid::grid.newpage()
      grid::grid.draw(output)
    } else {
      dev.new(width=(width*2/72),
              height=(height*2/72),
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
#                               gp = gpar(fontsize=17,
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
#                                  gp = gpar(fontsize=11,
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

