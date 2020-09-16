library(tidyverse)
#library(ggpubr)
#library(cowplot)
#library(gtable)
library(grid)
library(gridExtra)
devtools::load_all()

myplot <- ggplot(economy_basic, aes(x = interaction(year, variable), y = value, fill = sector)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) + theme_cmap() +
  theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        )

myplot


rect <- grid::rectGrob(width = 1, gp=gpar(fill='black',lwd=0))

title <-  grid::textGrob(label = "title goes here \nbut it's long",
                         name = title,
                         x = unit(5, "points"), # places the title x point in from left margin
                         y = unit(1, "npc") - unit(10, "points"), # places title at top of area, less a x point margin
                         just = c("left", "top"), # x and y specify the left and top corner of the Grob
                         gp = grid::gpar(fontsize=17,
                                         fontfamily=cmapplot_globals$font_title,
                                         fontface=cmapplot_globals$font_title_face,
                                         lineheight=0.93))

subtitle <-  grid::textGrob("subtitle goes here ",
                         x = unit(5, "points"), # places the title x point in from left margin
                         # need to calculate the right number of points based on the title
                         y = unit(1, "npc") - unit(50, "points"), # places title at top of area, less a x point margin
                         just = c("left", "top"), # x and y specify the left and top corner of the Grob
                         gp = grid::gpar(fontsize=11,
                                         fontfamily=cmapplot_globals$font_reg,
                                         fontface=cmapplot_globals$font_reg_face,
                                         lineheight=0.93))

grid.newpage()
grobTree(title, subtitle) %>%
  grid.draw()

#how to use grobHeight to automatically adjust location of subtitle?

# this attempt has the title and subtitle in separate matrix locations
layout = rbind(c(1,1),
               c(2,4),
               c(3,4),
               c(NA,4))


# this works but better to create the sidebar title first
grid.arrange(grobs = list(rect, title, subtitle, myplot),
             layout_matrix = layout,
             heights = unit(c(3, 10, 5, 150), "points"),
             widths = unit(c(80, 220), "points"))


# this attempt has the title and subtitle in a single matrix

# note arrangeGrob is same as grid.arrange but does not draw
layout2 = rbind(c(1,1),
                c(2,3))

grid.arrange(grobs = list(rect, grobTree(title, subtitle), myplot),
             layout_matrix = layout2,
             heights = unit(c(3, 200), "points"),
             widths = unit(c(80, 220), "points"))


# help
# https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
# https://stackoverflow.com/questions/59012782/ggplot2-how-to-align-grobs-to-sides-using-arrangegrob


### OLD





# this is better:

# problem is title placement features are abandoned here. How to place top left in grid?
title_block <- arrangeGrob(title, title, ncol = 1,
             heights = unit(c(3, 3), "lines")) # How many lines of height?


# v3 is best


line <- grid::linesGrob(x = grid::unit(c(0,1), "npc"),
                        y = grid::unit(c(0.92,0.92), "npc"),
                        gp=grid::gpar(col='black',
                                      lwd=3))

line2 <- grid::linesGrob(x = grid::unit(c(0,1), "npc"),
                        y = grid::unit(c(1,1), "inches"),
                        gp=grid::gpar(col='black',
                                      lwd=3))

line3 <- grid::linesGrob(x = grid::unit(c(0,1), "npc"),
                         y = grid::unit(c(3,3), "points"),
                         gp=grid::gpar(col='black',
                                       lwd=3))


grid.arrange(line, line2, line3, nrow = 1)




f <- function(gridlines = c("h", "v", "vh", NA)) {
  print(gridlines)
  gridlines <- match.arg(gridlines)
  print(gridlines)
  (grepl("h", gridlines))
}

# when argument is not explicit, match.arg selects the first
f()

# when argument is explicit, match.arg confirms the value is in the list...
f(gridlines = "h")

# and errors if false:
f(gridlines = "X")


(grepl("h", gridlines))
