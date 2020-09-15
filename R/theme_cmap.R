#'Add CMAP theme to ggplot chart
#'
#'\code{theme_cmap} returns one or more ggplot objects that together construct a
#'plot area in accordance with CMAP design standards.
#'
#'@usage theme_cmap(xlab = NULL, ylab = NULL, hline = NULL, vline = NULL)
#'
#'@param xlab,ylab Char, the string used to label the x and y axes,
#'  respectively. If left as NULL, the default, the axis label will be left off
#'  the graph.
#'@param hline,vline Numeric, the location of a strong horizontal or vertical
#'  line to be added to the plot. Use \code{hline = 0}, for example, to place a
#'  line at y = 0 to differentiate between positive and negative values.
#'
#'@examples
#'
#' \dontrun{
#' ggplot(grp_over_time, aes(x = year, y = realgrp, color = cluster)) +
#'  geom_line() +
#'  scale_x_continuous(breaks = scales::breaks_pretty(11)) +
#'  theme_cmap(hline = 0, ylab = "Percent change")
#'
#' df <- dplyr::filter(traded_emp_by_race, variable %in% c("SpecializedTraded", "UnspecializedTraded"))
#' ggplot(df, aes(x = reorder(Race, -value), y = value, fill = variable)) +
#'   geom_col(position = position_stack(reverse = TRUE)) +
#'   scale_y_continuous(labels = scales::percent) +
#'   theme_cmap(hline = 0, ylab = "This is the y axis")
#' }
#'
#'@export
theme_cmap <- function(xlab = NULL, ylab = NULL, hline = NULL, vline = NULL) {

  # Generate an explicit message to user if Whitney font family is not available
  if (!(cmapplot_globals$use_whitney)){
    message("'Whitney' font family not found. Using a substitute...")
  }

  # Generate list of elements to return.
  elements <- list(

    # The first element is the default theme.
    ggplot2::theme(

      # Default text
      text = ggplot2::element_text(family = cmapplot_globals$font_main,
                                   face = cmapplot_globals$font_main_face,
                                   size = 12,
                                   color="#222222"),

      # Title text
      plot.title = ggplot2::element_text(family=cmapplot_globals$font_title,
                                         face=cmapplot_globals$font_title_face,
                                         size=14),

      # Text elements not displayed
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_blank(),

      #Legend format
      legend.position = "top",
      legend.text.align = 0,
      legend.background = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(family = cmapplot_globals$font_main,
                                          face = cmapplot_globals$font_main_face,
                                          size = 14,
                                          color="#222222"),
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),

      #Axis format
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),

      #Grid lines
      panel.grid.major.y = ggplot2::element_line(size = 0.140359, color="#222222"),
      panel.grid.major.x = ggplot2::element_blank(),

      #Blank background
      panel.background = ggplot2::element_blank(),

      #Strip background
      strip.background = ggplot2::element_rect(fill="white"),

      #Facet wrap text
      strip.text = ggplot2::element_text(size  = 12,  hjust = 0)
    ),

    # The following elements get added based on the presence of specific
    # function arguments. These elements add to or overwrite portions of
    # the default theme.

    # re-introduce x label if specified
    if(!is.null(xlab)){
      ggplot2::theme(axis.title.x = element_text())
    },
    if(!is.null(xlab)){
      ggplot2::xlab(xlab)
    },

    # re-introduce y label if specified
    if(!is.null(ylab)){
      ggplot2::theme(axis.title.y = element_text())
    },
    if(!is.null(ylab)){
      ggplot2::ylab(ylab)
    },

    # add x origin line if specified
    if(!is.null(hline)){
      ggplot2::geom_hline(yintercept = hline, size = 0.4656139, color = "#222222")
    },

    # add y origin line if specified
    if(!is.null(vline)){
      ggplot2::geom_vline(xintercept = vline, size = 0.4656139, color = "#222222")
    }
  )

  # filter out NA elements before returning
  return(magrittr::extract(elements, !is.na(elements)))
}
