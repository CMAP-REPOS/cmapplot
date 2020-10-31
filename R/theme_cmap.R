#'Add CMAP theme to ggplot chart
#'
#'\code{theme_cmap} returns one or more ggplot objects that together construct a
#'plot area in accordance with CMAP design standards.
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
#'@param legend.max.columns Integer, the maximum number of columns in the
#'  legend. If no value is set, the chart will rely on `ggplot`'s default and
#'  automatic column handling behavior, which should work for most cases. Manual
#'  adjustment may be required if legend entries are particularly numerous
#'  and/or lengthy. Note that `ggplot` will still auto-adjust in ways that may
#'  mean the total number of columns is less than the maximum (e.g., if there
#'  are five items in a legend with four columns as the maximum, the output will
#'  be one row of three and another row of two).
#'@param overrides Named list, overrides the default drawing attributes defined
#'  in \code{cmapplot_globals$consts} which are drawn by
#'  \code{theme_cmap()} (only a few of them). Units are in bigpts (1/72 of an
#'  inch).
#'@param ... pass additional arguments to \code{ggplot2::theme()} to override
#'  any elements of the default CMAP theme.
#'
#'@section Overrides: In the \code{overrides} argument, the user can modify
#'  the default constants that define certain plot aesthetics. Units of all
#'  plot constants are "bigpts": 1/72 of an inch. Most plot constants (stored in
#'  \code{cmapplot_globals$consts}) are used in \code{finalize_plot()},
#'  but a few are used in this function. Overrides with astirisks are not
#'  "sticky" -- they will need to be re-specified in \code{finalize_plot}.
#'
#'  \itemize{
#'    \item \code{lwd_originline}: the width of any origin lines drawn by
#'    \code{hline} or \code{vline}.
#'    \item \code{lwd_gridline}: the width of gridlines in the plot, if drawn by
#'    \code{gridlines}.
#'    \item \code{margin_legend_i}*: The margin between legends (this only
#'    applies in plots with two or more legends and does not affect legend
#'    spacing on plots with single legends that have multiple rows).
#'    \item \code{margin_legend_b}*: The margin between the bottom of the legend
#'    and the rest of the plot.
#'    \item \code{legend_key_size}*: The size of legend key elements.
#'    \item \code{padding_plot}*: A numeric vector of length 4 (top, right,
#'    bottom, left) that creates padding between the plot and its drawing
#'    extent.
#'    \item \code{padding_legend}*: A numeric vector of length 4 (top, right,
#'    bottom, left) that creates padding around the margin. These numbers can be
#'    negative to reduce space around the legend.
#'  }
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
  axislines = c("none", "x", "y", "xy"),
  legend.max.columns = NULL,
  debug = FALSE,
  right_margin = 20,
  overrides = list(),
  ...
) {

  # Generate an explicit message to user if Whitney font family is not available
  if (!(cmapplot_globals$use_whitney)) {
    message("'Whitney' font family not found. Using a substitute...")
  }

  # create list of plot constants, from globals unless overridden by user
  consts <- utils::modifyList(cmapplot_globals$consts, overrides)

  # The half-line sets up the basic vertical rhythm of the theme.
  consts[["half_line"]] <- cmapplot_globals$fsize$reg / 2

  # Validate parameters, throw error if invalid
  gridlines <- match.arg(gridlines)
  axislines <- match.arg(axislines)

  # create blank list of gg objects and theme attributes to return
  obj <- list()
  attr <- list()


  # create a helper function to more easily add items to the obj list
  add_to_obj <- function(newitem){
    obj <<- append(get("obj", parent.frame()), list(newitem))
    NULL
  }

  # add base theme to object list
  add_to_obj(theme_cmap_base(consts = consts, debug = debug, right_margin = right_margin))

  # introduce x label, if specified
  if(!is.null(xlab)){
    attr[["axis.title.x"]] <- element_text(margin = margin(t = consts$half_line / 2), vjust = 1, inherit.blank = FALSE)
    add_to_obj(ggplot2::xlab(xlab))
  }

  # introduce y label, if specified
  if(!is.null(ylab)){
    attr[["axis.title.y"]] <- element_text(angle = 90, margin = margin(r = consts$half_line / 2), vjust = 1, inherit.blank = FALSE)
    add_to_obj(ggplot2::ylab(ylab))
  }

  # Add x origin line, if specified
  if(!is.null(hline)){
    add_to_obj(ggplot2::geom_hline(yintercept = hline,
                                   size = ggplot_size_conversion(consts$lwd_originline),
                                   color = cmapplot_globals$colors$blackish))
  }

  # Add y origin line, if specified
  if(!is.null(vline)){
    add_to_obj(ggplot2::geom_vline(xintercept = vline,
                                   size = ggplot_size_conversion(consts$lwd_originline),
                                   color = cmapplot_globals$colors$blackish))
  }

  # Introduce horizontal gridlines if specified
  if (grepl("h", gridlines)) {
    add_to_obj(ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(
        size = ggplot_size_conversion(consts$lwd_gridline),
        color = cmapplot_globals$colors$blackish)
    ))
  }

  # Introduce vertical gridlines if specified
  if (grepl("v", gridlines)) {
    add_to_obj(ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(
        size = ggplot_size_conversion(consts$lwd_gridline),
        color = cmapplot_globals$colors$blackish)
    ))
  }

  # Introduce x axis line if specified
  if (grepl("x", axislines)) {
    add_to_obj(ggplot2::theme(
      axis.line.x = ggplot2::element_line(
        size = ggplot_size_conversion(consts$lwd_gridline),
        color = cmapplot_globals$colors$blackish)
    ))
  }

  # Introduce y axis line if specified
  if (grepl("y", axislines)) {
    add_to_obj(ggplot2::theme(
      axis.line.y = ggplot2::element_line(
        size = ggplot_size_conversion(consts$lwd_gridline),
        color = cmapplot_globals$colors$blackish)
    ))
  }

  # only edit legend columns if value is added
  if (!is.null(legend.max.columns)){
      # set maximum number of columns for legend based on either "fill" or "col" to reflect different geom structures
      add_to_obj(ggplot2::guides(fill = guide_legend(ncol = legend.max.columns),
                      col  = guide_legend(ncol = legend.max.columns)
                      )
                 )
  }

  # add any extra args to theme attributes
  attr <- append(attr, list(...))

  # construct final list to return
  append(obj, list(do.call(theme, attr)))

}




# this is a complete theme built from scratch.
# it is modeled off of `ggplot2::theme_grey()`
theme_cmap_base <- function(consts = cmapplot_globals$consts,
                            debug = FALSE,
                            right_margin = 20
) {

  t <- theme(

    # building blocks
    line = element_line(
      colour = cmapplot_globals$colors$blackish,
      size = consts$lwd_gridline,
      linetype = 1, lineend = "butt",
      inherit.blank = TRUE),

    rect = element_rect(
      fill = NA, colour = ifelse(debug, "red", NA),
      size = 0.5, linetype = 1,
      inherit.blank = TRUE),

    text = element_text(
      family = cmapplot_globals$font$regular$family,
      face = cmapplot_globals$fgiont$regular$face,
      size = cmapplot_globals$fsize$reg,
      color = cmapplot_globals$colors$blackish,
      lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
      margin = margin(), debug = debug,
      inherit.blank = TRUE),

    # axis
    axis.line =          element_blank(),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(family = cmapplot_globals$font$light$family,
                                      face = cmapplot_globals$font$light$face,
                                      size = cmapplot_globals$fsize$reg),
    axis.text.x =        element_text(margin = margin(t = consts$half_line / 2), vjust = 1),
    axis.text.x.top =    element_text(margin = margin(b = consts$half_line / 2), vjust = 0),
    axis.text.y =        element_text(margin = margin(r = consts$half_line / 2), hjust = 1),
    axis.text.y.right =  element_text(margin = margin(l = consts$half_line / 2), hjust = 0),
    axis.ticks =         element_blank(),
    axis.ticks.length =  unit(0, "pt"), # determines space btwn axis text & panel even when ticks are off
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title =         element_blank(),

    legend.background =  NULL,
    legend.spacing.x =   grid::unit(consts$half_line, "pt"),
    legend.spacing.y =   grid::unit(consts$margin_legend_i, "bigpts"),
    legend.margin =      margin(l = 0 - consts$half_line),
    legend.key =         element_blank(),
    legend.key.size =    grid::unit(cmapplot_globals$fsize$reg, "pt"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        NULL,
    legend.text.align =  0,
    legend.title =       element_blank(),
    legend.position =    "top",
    legend.direction =   "horizontal",
    legend.justification = "left",
    legend.box =         "vertical",
    legend.box.margin =  margin(0, 0, 0, 0),
    legend.box.background = element_rect(
      fill = NA,
      colour = ifelse(debug, "red", NA)), # this should inherit from rect when NULL but it doesnt
    legend.box.just =    "left",
    legend.box.spacing = grid::unit(consts$margin_legend_b, "bigpts"),

    panel.background =   NULL,
    panel.border =       element_blank(),
    panel.grid =         element_blank(),
    panel.spacing =      unit(consts$half_line, "pt"),
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,

    strip.background =   NULL,
    strip.text =         element_text(hjust = 0),
    strip.text.x =       NULL,
    strip.text.y =       element_text(angle = -90),
    strip.text.y.left =  element_text(angle = 90),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = unit(consts$half_line / 2, "pt"),
    strip.switch.pad.wrap = unit(consts$half_line / 2, "pt"),


    plot.background =    element_blank(),
    plot.title =         element_text(family = cmapplot_globals$font$strong$family,
                                      face = cmapplot_globals$font$strong$face,
                                      size = cmapplot_globals$fsize$big,
                                      hjust = 0, vjust = 1,
                                      margin = margin(b = consts$half_line)),
    plot.title.position = "panel",
    plot.subtitle =      element_blank(),
    plot.caption =       element_text(family = cmapplot_globals$font$light$family,
                                      face = cmapplot_globals$font$light$face,
                                      size = cmapplot_globals$fsize$sml,
                                      hjust = 1, vjust = 1,
                                      margin = margin(t = consts$half_line)),
    plot.caption.position = "panel",
    plot.tag = element_blank(),
    plot.margin = margin(3, 3 + right_margin, 3, 3),

    complete = TRUE
  )

  # make sure all elements are set to NULL if not explicitly defined.
  theme_gray() %+replace% t
}
