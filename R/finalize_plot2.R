#library(ggpubr)
#library(cowplot)
#library(gtable)

#' @importFrom grid gpar unit
#'

# # SAMPLE CODE
# myplot <- ggplot(economy_basic, aes(x = interaction(year, variable), y = value, fill = sector)) +
#   geom_col(position = "fill") +
#   scale_y_continuous(labels = scales::percent) + theme_cmap() +
#   theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
#         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
#         legend.background = element_rect(fill = "transparent"), # get rid of legend bg
#         )
# finalize_plot2(myplot, "title \ntwo lines", "subtitle \nkeeps going \nand going!")


finalize_plot2 <- function(plot = ggplot2::last_plot(),
                           title = "Title here",
                           subtitle = "Subtitle here",
                           # below are not implemented yet in the function
                           width = 670,
                           height = 300,
                           title_width = 200){

  # implement text wrapping for title and subtitle here


  # establish rectangle grob for top line
  grob_rect <- grid::rectGrob(gp=gpar(fill = cmapplot_globals$colors$blackish, lwd=0))

  # establish title grob
  grob_title <-  grid::textGrob(label = title,
                                x = unit(2, "points"), # places the title x point in from left margin
                                y = unit(1, "npc") - unit(10, "points"), # places title at top of area, less a x point margin
                                just = c("left", "top"), # x and y specify the left and top corner of the Grob
                                gp = gpar(fontsize=17,
                                          fontfamily=cmapplot_globals$font_title,
                                          fontface=cmapplot_globals$font_title_face,
                                          lineheight=0.93))

  # establish subtitle grob
  grob_subtitle <-  grid::textGrob(label = subtitle,
                                   x = unit(2, "points"), # places the title x point in from left margin
                                   y = unit(1, "npc") - unit(20, "points") - grid::grobHeight(grob_title), # places subtitle at top of area, less title height, less a x point margin
                                   just = c("left", "top"), # x and y specify the left and top corner of the Grob
                                   gp = gpar(fontsize=11,
                                             fontfamily=cmapplot_globals$font_reg,
                                             fontface=cmapplot_globals$font_reg_face,
                                             lineheight=0.93))

  # establish matrix shape for three viewports
  layout = rbind(c(1,1),
                  c(2,3))

  # stitch together the final plot
  output <- gridExtra::arrangeGrob(grobs = list(grob_rect, grobTree(grob_title, grob_subtitle), myplot),
                                   layout_matrix = layout,
                                   heights = unit(c(3, 200), "points"),
                                   widths = unit(c(80, 220), "points"))



  # for now--just print the grob to the plots window
  grid::grid.newpage()
  grid::grid.draw(output)

  # gtable::gtable_show_layout(output)
}






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

