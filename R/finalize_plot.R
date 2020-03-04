
save_plot <- function (plot_grid, save_filepath, type, height=NA) {
  # use 72 px/in conversion
  width_pixels = 670

  if (!type %in% c("web","word",'ppt')) {
    stop("Type must be 'web', 'word', or 'ppt'")
  }

  if (type == 'web'){
    device = 'svg'
    dpi = NA
  }

  if (type == 'word'){
    device = 'tiff'
    dpi = 'print'
    if (height > 400){
      warning("max height is 400 px for this type")
    }
  }

  if (type == 'ppt'){
    device = 'jpeg'
    dpi = 'print'
    if (height > 400){
      warning("max height is 400 px for this type")
    }
  }

  grid::grid.draw(plot_grid)

  #save it
  ggplot2::ggsave(filename = save_filepath,
                  plot=plot_grid,
                  width=(width_pixels/72), height=(height/72),
                  bg="white",
                  device=device,
                  dpi=dpi)
}


create_title_block <- function (title, subtitle) {
  title_block <- grid::grobTree(grid::linesGrob(x = grid::unit(c(0,1), "npc"),
                                                y = grid::unit(c(0.997,0.997), "npc"),
                                                gp=grid::gpar(col='black',
                                                              lwd=3)
                                                ),
                                grid::textGrob(title,
                                               x = 0.1, hjust = 0, vjust = 1, y=0.97,
                                               gp = grid::gpar(fontsize=17,
                                                               fontfamily=cmapplot_globals$font_title,
                                                               fontface=cmapplot_globals$font_title_face,
                                                               lineheight=1)),
                                grid::textGrob(subtitle,
                                               x = 0.1, hjust = 0, vjust = 1, y = 0.72,
                                               gp = grid::gpar(fontsize=11,
                                                               fontfamily=cmapplot_globals$font_reg,
                                                               fontface=cmapplot_globals$font_reg_face,
                                                               lineheight=1))
  )

  return(title_block)

}

#' @export
draw_plot <- function(title,
                      subtitle,
                      save_filepath,
                      width_pixels=670,
                      height_pixels=400,
                      dpi) {

  side <- create_title_block(title, subtitle)

  line <- grid::linesGrob(x = grid::unit(c(0,1), "npc"),
                          y = grid::unit(c(0.93,0.93), "npc"),
                          gp=grid::gpar(col='black',
                                        lwd=3))

  newplot <- ggpubr::ggarrange(line, myplot,
                    ncol=1, nrow=2,
                    heights = c(0.045,1))

  plot_grid <- ggpubr::ggarrange(side, newplot,
                    ncol = 2, nrow = 1,
                    widths = c(1,2.9))

  return(plot_grid)
}


# example (load economy_basic)
myplot <- ggplot(economy_basic, aes(x = interaction(year, variable), y = value, fill = sector)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_cmap()

# write in title and subtitle
title <- "Change in labor force size per 1,000 residents, by age, Chicago and select Metropolitan Statistical Areas, 2006-10 to 2013-17"
subtitle <- "Source: Chicago Metropolitan Agency for Planning analysis of National Highway Traffic Safety Administration Corporate Average Fuel Economy Fact Sheets; Illinois Department of Transportation data; 2009 National Household Travel Survey data."
lt2 <- "Revenues from state motor fuel tax (MFT) or road usage charge"

# word wrap
tpieces <- stringi::stri_wrap(lt2, 22, cost_exponent=2, whitespace_only=TRUE)
titlebreak <- stringi::stri_paste_list(list(tpieces), sep="\n")
stpieces <- stringi::stri_wrap(subtitle, 28, cost_exponent=2, whitespace_only=TRUE)
subtitlebreak <- stringi::stri_paste_list(list(stpieces), sep="\n")


draw_plot(titlebreak, subtitlebreak)
# save_plot(plot_grid, "C:/Users/sbuchhorn/desktop/gg/ggtest1.tiff", type='word', height=500)



