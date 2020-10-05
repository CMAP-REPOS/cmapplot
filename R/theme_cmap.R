#'Add CMAP theme to ggplot chart
#'
#'\code{theme_cmap} returns one or more ggplot objects that together construct a
#'plot area in accordance with CMAP design standards.
#'
#'@usage theme_cmap(xlab = NULL, ylab = NULL, hline = NULL, vline = NULL,
#'  gridlines = c("h", "v", "hv", "none"), legend.max.columns = NULL)
#'
#'@param xlab,ylab Char, the string used to label the x and y axes,
#'  respectively. If unspecified, the axis label will be left off the graph.
#'@param hline,vline Numeric, the location of a strong horizontal or vertical
#'  line to be added to the plot. Use \code{hline = 0}, for example, to place a
#'  line at y = 0 to differentiate between positive and negative values.
#'@param gridlines Char, the grid lines to be displayed on the chart. If left as
#'  default, horizontal grid lines will be displayed while vertical grid lines
#'  will be masked. Acceptable values are "h" (horizontal only), "v" (vertical
#'  only), "hv" (both horizontal and vertical), and "none" (neither).
#'@param legend.max.columns Integer, the maximum number of columns in the legend. If no
#'  value is set, the chart will rely on `ggplot`'s default and automatic column
#'  handling behavior, which should work for most cases. Manual adjustment may
#'  be required if legend entries are particularly numerous and/or lengthy. Note
#'  that `ggplot` will still auto-adjust in ways that may mean the total number
#'  of columns is less than the maximum (e.g., if there are five items in a
#'  legend with four columns as the maximum, the output will be one row of three
#'  and another row of two).
#'
#'@examples
#'
#'\dontrun{
#'
#' # The only way to place the origin line (`hline`, `vline`) behind any data geoms
#' # is to or place `theme_cmap()` before the geoms:
#'  ggplot(grp_over_time, aes(x = year, y = realgrp, color = cluster)) +
#'    theme_cmap(hline = 0, ylab = "Percent change") +
#'    geom_line() +
#'    scale_x_continuous(breaks = scales::breaks_pretty(11))
#'
#'
#'  df <- dplyr::filter(traded_emp_by_race, variable %in% c("SpecializedTraded",
#'    "UnspecializedTraded"))
#'
#'  ggplot(df, aes(x = reorder(Race, -value), y = value, fill = variable)) +
#'    geom_col(position = position_stack(reverse = TRUE)) +
#'    scale_y_continuous(labels = scales::percent) +
#'    theme_cmap(hline = 0, ylab = "This is the y axis")
#'
#'  ggplot(df, aes(x = reorder(Race, -value), y = value, fill = variable)) +
#'    geom_col(position = position_stack(reverse = TRUE)) +
#'    coord_flip() +
#'    scale_y_continuous(labels = scales::percent) +
#'    theme_cmap(hline = 0, gridlines = "v")
#' }
#'@export
theme_cmap <- function(
  xlab = NULL, ylab = NULL,
  hline = NULL, vline = NULL,
  gridlines = c("h", "v", "hv", "none"),
  legend.max.columns = NULL
) {

  # Generate an explicit message to user if Whitney font family is not available
  if (!(cmapplot_globals$use_whitney)) {
    message("'Whitney' font family not found. Using a substitute...")
  }

  # Validate gridlines parameter, throw error if invalid
  gridlines <- match.arg(gridlines)

  # Generate list of elements to return.
  elements <- list(

    # The first element is the default theme.
    ggplot2::theme(

      # Default text
      text = ggplot2::element_text(family = cmapplot_globals$font$main$family,
                                   face = cmapplot_globals$font$main$face,
                                   size = cmapplot_globals$font$main$size,
                                   color = cmapplot_globals$colors$blackish),

      # Title text
      plot.title = ggplot2::element_text(family = cmapplot_globals$font$title$family,
                                         face = cmapplot_globals$font$title$face,
                                         size = cmapplot_globals$font$title$size),

      # Caption/source text
      plot.caption = ggplot2::element_text(family = cmapplot_globals$font$note$family,
                                           face = cmapplot_globals$font$note$face,
                                           size = cmapplot_globals$font$note$size),

      # Text elements not displayed
      plot.subtitle = ggplot2::element_blank(),

      # Legend format
      legend.position = "top",
      legend.justification = "left",
      legend.direction = "horizontal",
      legend.text.align = 0,
      legend.margin = margin(cmapplot_globals$plot_constants$padding_legend[1],
                             cmapplot_globals$plot_constants$padding_legend[2],
                             cmapplot_globals$plot_constants$padding_legend[3],
                             cmapplot_globals$plot_constants$padding_legend[4],
                             "bigpts"),
      legend.box.background = ggplot2::element_blank(),
      legend.box = "vertical",
      legend.spacing.y = cmapplot_globals$plot_constants$margin_v5,
      legend.text = ggplot2::element_text(),
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),

      # Axis format
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = cmapplot_globals$colors$blackish),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 5, b = 5)),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),

      # panel placement
      plot.margin = ggplot2::margin(cmapplot_globals$plot_constants$padding_plot[1],
                                    cmapplot_globals$plot_constants$padding_plot[2],
                                    cmapplot_globals$plot_constants$padding_plot[3],
                                    cmapplot_globals$plot_constants$padding_plot[4],
                                    "bigpts"),

      # Blank background
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),

      # No gridlines
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),

      # Strip background
      strip.background = ggplot2::element_rect(fill = "white"),

      # Facet wrap text
      strip.text = ggplot2::element_text(hjust = 0)
    ),

    # The following elements get added based on the presence of specific
    # function arguments. These elements add to or overwrite portions of
    # the default theme.

    # Re-introduce x label, if specified
    if(!is.null(xlab)){
      ggplot2::theme(axis.title.x = element_text())
    },
    if(!is.null(xlab)){
      ggplot2::xlab(xlab)
    },

    # Re-introduce y label, if specified
    if(!is.null(ylab)){
      ggplot2::theme(axis.title.y = element_text())
    },
    if(!is.null(ylab)){
      ggplot2::ylab(ylab)
    },

    # Add x origin line, if specified
    if(!is.null(hline)){
      ggplot2::geom_hline(yintercept = hline,
                          size = ggplot_size_conversion(cmapplot_globals$plot_constants$lwd_originline),
                          color = cmapplot_globals$colors$blackish)
    },

    # Add y origin line, if specified
    if(!is.null(vline)){
      ggplot2::geom_vline(xintercept = vline,
                          size = ggplot_size_conversion(cmapplot_globals$plot_constants$lwd_originline),
                          color = cmapplot_globals$colors$blackish)
    },

    # Re-introduce horizontal gridlines if specified
    if (grepl("h", gridlines)) {
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_line(
          size = ggplot_size_conversion(cmapplot_globals$plot_constants$lwd_gridline),
          color = cmapplot_globals$colors$blackish)
      )
    },

    # Re-introduce vertical gridlines if specified
    if (grepl("v", gridlines)) {
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_line(
          size = ggplot_size_conversion(cmapplot_globals$plot_constants$lwd_gridline),
          color = cmapplot_globals$colors$blackish)
      )
    },

    # only edit legend columns if value is added
    if (!is.null(legend.max.columns)){
        # set maximum number of columns for legend based on either "fill" or "col" to reflect different geom structures
        ggplot2::guides(fill = guide_legend(ncol = legend.max.columns),
                        col  = guide_legend(ncol = legend.max.columns))
    }

  )

  # Filter out NA elements before returning
  return(magrittr::extract(elements, !is.na(elements)))
}
