#' Allow user to export plot in desired format
#'
#' @export
save_plot <- function(savepath, type) {

  if (type == 'web'){
    # width should be 670 pixels, but units must be in in, cm, or mm
    # will depend on the resolution
    dpi = 96
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
    # will depend on the resolution...doesn't make sense at 300 ppi
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
    # will depend on the resolution
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

#' @export
create_footer <- function() {
  footer <- grid::grobTree(grid::textGrob('hello'))
  return(footer)
}

footer <- create_footer()

# EXAMPLE
#   myplot <- ggplot(grp_over_time, aes(x = year, y = realgrp, color = cluster, label = cluster)) +
#   geom_line() + theme_cmap() + geom_text_lastonly()
#
#   plot_grid <- ggpubr::ggarrange(myplot, footer)
#
# # ggsave('C:/Users/sbuchhorn/Desktop/gg/ggtest.png')
# #
# save_plot('C:/Users/sbuchhorn/Desktop/gg/ggtest.tiff', 'PowerPoint')

