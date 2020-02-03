#' Allow user to export plot in desired format
#'
#' @export
finalize_plot <- function(savepath, type, dpi=96) {

  if (type == 'web'){
    # width should be 670 pixels, but units must be in in, cm, or mm
    # will depend on the resolution
    pxin = dpi
    width = 670/pxin
    units = "in"
    text1 = 11
    text2 = 14
    text3 = 17
  }

  ggsave(savepath,
         width = width,
         units = units)
}

# EXAMPLE
  ggplot(grp_over_time, aes(x = year, y = realgrp, color = cluster, label = cluster)) +
  geom_line() + theme_cmap() + geom_text_lastonly()

ggsave('C:/Users/sbuchhorn/Desktop/gg/ggtest.png')

finalize_plot('C:/Users/sbuchhorn/Desktop/gg/ggtest.png', 'web')

# so like, what will this do that ggsave doesn't already do
