#' Allow user to export plot in desired format

prepare_title <- function(title) {

  title <- grid::textGrob(title, x=0.1, y=0.9, just=c("left", "top"),
                          gp = gpar(fontfamily=cmapplot_globals$font_title,
                                    fontsize=18,
                                    fontface="bold",
                                    col="#222222"))
  return(title)
}

prepare_source <- function(source) {

  source <- grid::textGrob(source, x=0.1, y=0.9, just=c("left","top"),
                           gp = gpar(
                             family=cmapplot_globals$font_title,
                             fontsize=10
                           ))

  return(source)
}



#' @export
save_plot <- function(savepath, type) {

  if (type == 'web'){
    # width should be 670 pixels, but units must be in in, cm, or mm
    # depends on the resolution.  assume 72
    dpi = 72
    pxin = dpi
    width = 670/pxin
    units = "in"
    text1 = 11
    text2 = 14
    text3 = 17
    device = 'svg'
  }

  if (type == 'report'){
    # width should be 670 pixels, but units must be in in, cm, or mm
    # depends on the resolution.  assume 72
    dpi = 300
    pxin = 72  # setting at 72 for now
    width = 670/pxin
    units = "in"
    text1 = 11
    text2 = 14
    text3 = 17
    device = 'tiff'
  }

  if (type == 'PowerPoint'){
    # width should be 670 pixels, but units must be in in, cm, or mm
    # depends on the resolution.  assume 72
    dpi = 300
    pxin = 72  # setting at 72 for now
    width = 670/pxin
    units = "in"
    text1 = 11
    text2 = 14
    text3 = 17
    device = 'tiff'
  }

  ggsave(savepath,
         width = width,
         units = units,
         device = device)
}


# I still need to deal with font sizes (including those set in theme_cmap) and text wrapping, and line spacing
title <- prepare_title(title='Change in labor force
size per 1,000
residents, by age,
Chicago and select
Metropolitan
Statistical Areas,
2006-10 to 2013-17')

source <- prepare_source(source='Source: Chicago Metropolitan
Agency for Planning analysis of
American Community Survey
data, five-year estimates,
2006-10 and 2013-17')

# The graph
myplot <- ggplot(grp_over_time, aes(x = year, y = realgrp, color = cluster, label = cluster)) +
  geom_line()
# + theme_cmap()

# putting it together
# like, i don't know if I can make uneven row heights with this, and if not I'd need to somehow group all the text
# I can go back to the viewport thing but would just have to figure out how to save
# plot_grid <-
ggpubr::ggarrange(title, myplot, source,
                  ncol=2, nrow= 2,
                  heights = c(1,1)
                  )

# save
# save_plot('C:/Users/sbuchhorn/Desktop/gg/ggtest3.tiff', 'report')


# EXTRA

# This is using the viewport to draw/arrange

title_vp <- viewport(x=0.234, y=1, width=0.234, height=0.5, just=c("right", "top"))
pushViewport(title_vp)
grid.draw(title)
popViewport()

source_vp <- viewport(x=0.234, y=0.5, width = 0.234, height=0.5, just=c("right", "top"))
pushViewport(source_vp)
grid.draw(source)
popViewport()

chart_vp <- viewport(1, y=1, width=0.7, height=1, just=c("right","top"))
pushViewport(chart_vp)
grid.draw(ggplotGrob(myplot))
popViewport()

ggsave('C:/Users/sbuchhorn/Desktop/gg/ggest5.tiff')


# testing
title <- expression(atop(bold("Change in labor force \n size per 1,000 \n residents, by age, \n Chicago and select \n Metropolitan \n Statistical Areas, \n 2006-10 to 2013-17"),
                         scriptstyle("This is the caption")))

figure <- myplot

ggpubr::annotate_figure(figure,
                left = ggpubr::text_grob(title))


# hmmm like, how can I group the title and source together
# https://stackoverflow.com/questions/58160415/how-can-a-subtitle-be-added-to-an-annotate-figure-plot
# like this is nice but I don't think I can choose the font??
