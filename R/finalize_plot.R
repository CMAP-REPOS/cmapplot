
save_plot <- function (plot_grid, width, height, save_filepath) {
  grid::grid.draw(plot_grid)
  #save it
  ggplot2::ggsave(filename = save_filepath,
                  plot=plot_grid, width=(width/72), height=(height/72),  bg="white")
}

create_title_block <- function (title, subtitle) {
  title_block <- grid::grobTree(grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(1.1, "npc")),
                                grid::textGrob(title,
                                               x = 0.1, hjust = 0, vjust = 1, y=0.97,
                                               gp = grid::gpar(fontsize=17,
                                                               fontfamily=cmapplot_globals$font_title,
                                                               lineheight=1)),
                                grid::textGrob(subtitle,
                                               x = 0.1, hjust = 0, y = 0.51,
                                               gp = grid::gpar(fontsize=11,
                                                               fontfamily=cmapplot_globals$font_reg,
                                                               lineheight=1))
  )

  return(title_block)

}

#' @export
draw_plot <- function(title,
                      subtitle,
                      save_filepath,
                      width_pixels,
                      height_pixels) {

  side <- create_title_block(title, subtitle)

  ggpubr::ggarrange(side, myplot,
                    ncol = 2, nrow = 1,
                    widths = c(1,2.9))

  # just returning in viewport for now, not saving.
  # save_plot(plot_grid, width_pixels, height_pixels, save_filepath)
}


# example (load economy_basic)
myplot <- ggplot(economy_basic, aes(x = interaction(year, variable), y = value, fill = sector)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_cmap()

title <- "Change in labor force \nsize per 1,000 \nresidents, by age, \nChicago and select \nMetropolitan \nStatistical Areas, \n2006-10 to 2013-17"
subtitle <- "Source: Chicago Metropolitan \nAgency for Planning analysis of \nAmerican Community Survey \ndata, five-year estimates, \n2006-10 and 2013-17."

draw_plot(title, subtitle, "C:/Users/sbuchhorn/desktop/gg/ggtest1.png", 700, 500)
