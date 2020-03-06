#' Arrange and save CMAP ggplot chart
#'
#' Running this function will save your plot with CMAP infographic standards.
#' It will align your title and subtitle to the left, add a horizontal line on top, and save it to your specified location.
#'
#' @param plot The variable name of the plot you have created that you want to work with
#' @param title The text you want to appear in the title block.  You can write the title as one string, or add in newline breaks ('\n') with option autowrap = FALSE.
#' @param subtitle The next you want to appear in the subtitle block.  You can write the subtitle as one string, or add in newline breaks ('\n') with option autowrap = FALSE.
#' @param save_filepath Filepath you want the plot to be saved to.  You can specify an extension to use, or an extension will be added if you specify 'type'
#' @param height The height in pixels for the image.
#' @param type The type of output - use to set extension and give max height warnings.
#' @param sidepercent The rough percent of total width that the left column shoud take up.
#' @param autowrap Wrap title and subtitle text automatically
#' @param titlewraplen The wrap code point length for the title (use with autowrap = TRUE)
#' @param subtitlewraplen The wrap code point length for the subtitle (use with autowrap = TRUE)
#' @param action Choose whether to view or save the plot
#' @param newwindow View plot in a new window if TRUE
#'
#'
#' @examples
#' title <- "Change in labor force size per 1,000 residents, by age, Chicago and select Metropolitan Statistical Areas, 2006-10 to 2013-17"
#' subtitle <- "Source: Chicago Metropolitan Agency for Planning analysis of National Highway Traffic Safety Administration Corporate Average Fuel Economy Fact Sheets; Illinois Department of Transportation data; 2009 National Household Travel Survey data."
#'
#' myplot <- ggplot(economy_basic, aes(x = interaction(year, variable), y = value, fill = sector)) +
#'   geom_col(position = "fill") +
#'   scale_y_continuous(labels = scales::percent) +
#'   theme_cmap()
#'
#'
#' draw_plot(myplot,title, subtitle, action='save', save_filepath="C:/Users/sbuchhorn/Desktop/gg/test1", type='web')
#'
#'

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
    if (grepl('.jpeg',save_filepath) == FALSE) {
      paste(save_filepath, '.jpeg', sep='')
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
                  bg="white")
}


create_title_block <- function (title, subtitle) {
  title_block <- grid::grobTree(
                                grid::textGrob(title,
                                               x = 0.1, hjust = 0, y = 1, vjust = 1,
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
draw_plot <- function(plot,
                      title,
                      subtitle,
                      save_filepath,
                      height_pixels,
                      type,
                      sidepercent=27,
                      autowrap=TRUE,
                      titlewraplen=22,
                      subtitlewraplen=28,
                      action=view,
                      newwindow=FALSE) {

  # word wrap
  if (autowrap == TRUE){
    tpieces <- stringi::stri_wrap(lt2, titlewraplen, cost_exponent=2, whitespace_only=TRUE)
    titlebreak <- stringi::stri_paste_list(list(tpieces), sep="\n")
    stpieces <- stringi::stri_wrap(subtitle, subtitlewraplen, cost_exponent=2, whitespace_only=TRUE)
    subtitlebreak <- stringi::stri_paste_list(list(stpieces), sep="\n")

    side <- create_title_block(titlebreak, subtitlebreak)
  }

  if (autowrap == FALSE){
    side <- create_title_block(title, subtitle)

  }

  line <- grid::linesGrob(x = grid::unit(c(0,1), "npc"),
                          y = grid::unit(c(0.9,0.9), "npc"),
                          gp=grid::gpar(col='black',
                                        lwd=3))

  bottom <- ggpubr::ggarrange(side, plot,
                    ncol=2, nrow=1,
                    widths = c(1, ((100/sidepercent) - 1)))

  plot_grid <- ggpubr::ggarrange(line, bottom,
                    ncol = 1, nrow = 2,
                    heights = c(0.03,1))

  if (action=='view') {
    if (newwindow==TRUE) {
      windows(width=(width_pixels/72))
      return(plot_grid)
    }
    if (newwindow==FALSE) {
      return(plot_grid)
    }
  }

  if (action=='save') {
    save_plot(plot_grid, save_filepath, type=type, height=height_pixels)
  }
}



