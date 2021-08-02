#'Add CMAP theme to ggplot chart
#'
#'Return one or more ggplot objects that together construct a plot area in
#'accordance with CMAP design standards.
#'
#'Using either the \code{xlab} or \code{ylab} argument, but not both, will have
#'undesireable outcomes in a ggplot that also invokes \code{coord_flip()}. Under
#'the hood, \code{theme_cmap(xlab = "foo")} both sets \code{ggplot2::xlab =
#'"foo"} and 'turns on' the ggplot theme element \code{axis.title.x}. With
#'\code{coord_flip()}, the xlab travels with the data (becoming the ylab) but
#'the theme modifier stays on the x axis. To solve this, rewrite your ggplot
#'construction to avoid \code{coord_flip()} or manually turn off and on the
#'correct elements from ggplot2's \code{\link[ggplot2]{theme}} function in the
#'\code{...} of this function.
#'
#'
#'@param xlab,ylab Char, the string used to label the x and y axes,
#'  respectively. If unspecified, the axis label will be left off the graph. See
#'  details for unexpected outcomes when using these arguments along with
#'  \code{coord_flip()}.
#'@param hline,vline Numeric, the location of a strong horizontal or vertical
#'  line to be added to the plot. Use \code{hline = 0}, for example, to place a
#'  line at y = 0 to differentiate between positive and negative values. The
#'  width of this line is determined by
#'  \code{cmapplot_globals$consts$lwd_strongline}. Note that on most displays
#'  the difference between this line and gridlines is impossible to discern in
#'  R. The difference will be visible upon export.
#'@param gridlines Char, the grid lines to be displayed on the chart. If left as
#'  default, horizontal grid lines will be displayed while vertical grid lines
#'  will be masked. Acceptable values are "h" (horizontal only), "v" (vertical
#'  only), "hv" (both horizontal and vertical), and "none" (neither).
#'@param axislines Char, the axis lines to be displayed on the chart. Acceptable
#'  values are "x" (x axis only), "y" (y axis only), "xy" (both axes), and
#'  "none" (neither, the default).
#'@param axisticks Char, the axis ticks to be displayed on the chart. Acceptable
#'  values are "x" (x axis only), "y" (y axis only), "xy" (both axes), and
#'  "none" (neither, the default). Because \code{ggplot2} defaults to moderately
#'  expanding the range of displayed data, this may need to be accompanied by a
#'  call to \code{expand = c(0, 0)} within an appropriate \code{scale_*_*}
#'  argument in order for ticks to appear to touch the outermost gridline(s).
#'@param show.legend Bool, \code{TRUE} is the default. \code{FALSE} to hide the
#'  legend.
#'@param legend.max.columns Integer, the maximum number of columns in the
#'  legend. If no value is set, the chart will rely on `ggplot`'s default and
#'  automatic column handling behavior, which should work for most cases. Manual
#'  adjustment may be required if legend entries are particularly numerous
#'  and/or lengthy. Note that `ggplot` will still auto-adjust in ways that may
#'  mean the total number of columns is less than the maximum (e.g., if there
#'  are five items in a legend with four columns as the maximum, the output will
#'  be one row of three and another row of two).
#'@param debug Bool, Defaults to \code{FALSE}. Set to \code{TRUE} to show
#'  rectangles around all \code{geom_rect()} elements for debugging.
#'@param overrides Named list, overrides the default drawing attributes defined
#'  in \code{cmapplot_globals$consts} which are drawn by
#'  \code{\link{theme_cmap}}. Units are in bigpts (1/72 of an inch).
#'@param ... pass additional arguments to ggplot2's \code{\link[ggplot2]{theme}}
#'  function to override any elements of the default CMAP theme.
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
#'  ggplot(df, aes(y = reorder(Race, -value), x = value, fill = variable)) +
#'    geom_col(position = position_stack(reverse = TRUE)) +
#'    scale_x_continuous(labels = scales::percent) +
#'    theme_cmap(vline = 0, gridlines = "v")
#' }
#'@export
theme_cmap <- function(
  xlab = NULL, ylab = NULL,
  hline = NULL, vline = NULL,
  gridlines = c("h", "v", "hv", "none"),
  axislines = c("none", "x", "y", "xy"),
  axisticks = c("none","x","y","xy"),
  show.legend = TRUE,
  legend.max.columns = NULL,
  debug = FALSE,
  overrides = list(),
  export_format = c("web","brief"),
  ...
) {

  # Initialization --------------------------------------------------

  # Generate an explicit message to user if Whitney font family is not available
  if (!(cmapplot_globals$use_whitney)) {
    message("'Whitney' font family not found. Using a substitute...")
  }

  # create list of plot constants, from globals unless overridden by user
  consts <- utils::modifyList(cmapplot_globals$consts, overrides)

  # Create list of font sizes, selecting the appropriate one based on export type
  fsize <- cmapplot_globals$fsize
  if (export_format == "brief") {
    fsize$S <- fsize$S_brief
    fsize$M <- fsize$M_brief
    fsize$L <- fsize$L_brief
  }

  # The half-line sets up the basic vertical rhythm of the theme.
  consts[["half_line"]] <- fsize$M / 2

  # Validate parameters, throw error if invalid
  gridlines <- match.arg(gridlines)
  axislines <- match.arg(axislines)
  axisticks <- match.arg(axisticks)
  export_format <- match.arg(export_format)

  # Introduce elements based on args ---------------------------------

  # create blank list of gg objects to be returned by this function
  obj <- list()

  # Create a helper function to more easily add items to the obj list
  # This is just a shorthand for:
  # obj <- append(obj, [item] )
  add_to_obj <- function(newitem){
    obj <<- append(get("obj", parent.frame()), list(newitem))
    NULL
  }

  # create a blank list of ggplot2theme elements to return in a
  # theme call at the end of the function. Items in this list should
  # be named with valid ggplot2::theme() argument names.
  attr <- list()

  # x label, if specified
  if(!is.null(xlab)){
    attr[["axis.title.x"]] <- ggplot2::element_text(
      margin = ggplot2::margin(t = consts$half_line / 2),
      vjust = 1,
      inherit.blank = FALSE)

    add_to_obj(ggplot2::xlab(xlab))
  }

  # y label, if specified
  if(!is.null(ylab)){
    attr[["axis.title.y"]] <- ggplot2::element_text(
      angle = 90,
      margin = ggplot2::margin(r = consts$half_line / 2),
      vjust = 1,
      inherit.blank = FALSE)

    add_to_obj(ggplot2::ylab(ylab))
  }

  # x origin line, if specified
  if(!is.null(hline)){
    add_to_obj(ggplot2::geom_hline(
      yintercept = hline,
      size = gg_lwd_convert(consts$lwd_strongline),
      color = cmapplot_globals$colors$blackish))
  }

  # y origin line, if specified
  if(!is.null(vline)){
    add_to_obj(ggplot2::geom_vline(
      xintercept = vline,
      size = gg_lwd_convert(consts$lwd_strongline),
      color = cmapplot_globals$colors$blackish))
  }

  # set legend column max, if specified
  if (!is.null(legend.max.columns)){
      # set for legend based on either "fill" or "col" to reflect different geom structures
      add_to_obj(ggplot2::guides(
        fill = guide_legend(ncol = legend.max.columns),
        col  = guide_legend(ncol = legend.max.columns)))
  }

  # hide legend if specified
  if (!show.legend){
    attr[["legend.position"]] <- "none"
  }

  # horizontal gridlines, if specified
  if (grepl("h", gridlines)) {
    attr[["panel.grid.major.y"]] <- ggplot2::element_line(
      size = gg_lwd_convert(consts$lwd_gridline),
      color = cmapplot_globals$colors$blackish)
  }

  # vertical gridlines, if specified
  if (grepl("v", gridlines)) {
    attr[["panel.grid.major.x"]] <- ggplot2::element_line(
      size = gg_lwd_convert(consts$lwd_gridline),
      color = cmapplot_globals$colors$blackish)
  }

  # x axis line, if specified
  if (grepl("x", axislines)) {
    attr[["axis.line.x"]] <- ggplot2::element_line(
      size = gg_lwd_convert(consts$lwd_gridline),
      color = cmapplot_globals$colors$blackish)
  }

  # y axis line, if specified
  if (grepl("y", axislines)) {
    attr[["axis.line.y"]] <- ggplot2::element_line(
      size = gg_lwd_convert(consts$lwd_gridline),
      color = cmapplot_globals$colors$blackish)
  }

  # x axis ticks, if specified
  if (grepl("x", axisticks)) {
    attr[["axis.ticks.x"]] <- ggplot2::element_line(
      size = gg_lwd_convert(consts$lwd_gridline),
      color = cmapplot_globals$colors$blackish)

    attr[["axis.ticks.length.x"]] <- unit(consts$length_ticks,"bigpts")
  }

  # y axis ticks, if specified
  if (grepl("y", axisticks)) {
    attr[["axis.ticks.y"]] <- ggplot2::element_line(
      size = gg_lwd_convert(consts$lwd_gridline),
      color = cmapplot_globals$colors$blackish)

    attr[["axis.ticks.length.y"]] <- unit(consts$length_ticks,"bigpts")
  }

  # # Policy brief elements, if specified
  # if (export_format == "brief") {
  #   attr[["text"]] <- ggplot2::element_text(
  #     size = cmapplot_globals$fsize$M_brief
  #   )
  #
  #   attr[["axis.text"]] <- ggplot2::element_text(
  #     size = cmapplot_globals$fsize$M_brief
  #   )
  #
  #   attr[["legend.key.size"]] <- grid::unit(cmapplot_globals$fsize$M_brief, "pt")
  # }

  # Construct theme elements -----------------------------------------

  # add base theme to object list
  add_to_obj(theme_cmap_base(consts = consts, fsize = fsize, debug = debug))

  # add `attr` theme elements to the list in a new `ggplot2::theme()` object,
  # so that they override `theme_cmap_base()` if needed
  if(length(attr) > 0) {
    add_to_obj(do.call(theme, attr))
  }

  # add extra theme arguments to a new `ggplot2::theme()` object,
  # so that they override any previous theme arguments
  if(length(list(...)) > 0) {
    add_to_obj(do.call(theme, list(...)))
  }

  # return final list
  return(obj)
}




# this is a complete theme built from scratch.
# it is modeled off of `ggplot2::theme_grey()`
theme_cmap_base <- function(consts = cmapplot_globals$consts,
                            fsize = cmapplot_globals$fsize,
                            debug = FALSE
) {

  t <- theme(

    # building blocks
    line = element_line(
      colour = cmapplot_globals$colors$blackish,
      size = gg_lwd_convert(consts$lwd_gridline),
      linetype = 1, lineend = "butt",
      inherit.blank = TRUE),

    rect = element_rect(
      fill = NA, colour = ifelse(debug, "blue", NA),
      size = 0.5, linetype = 1,
      inherit.blank = TRUE),

    text = element_text(
      family = cmapplot_globals$font$regular$family,
      face = cmapplot_globals$fgiont$regular$face,
      size = fsize$M,
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
                                      size = fsize$M),
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
    legend.spacing.x =   grid::unit(consts$half_line, "bigpts"),
    legend.spacing.y =   grid::unit(consts$margin_legend_i, "bigpts"),
    legend.margin =      margin(l = 0 - consts$half_line),
    legend.key =         element_blank(),
    legend.key.size =    grid::unit(fsize$M, "pt"), # use pts, not bigpts, to match fontsize
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
      colour = ifelse(debug, "blue", NA)), # this should inherit from rect when NULL but it doesnt
    legend.box.just =    "left",
    legend.box.spacing = grid::unit(consts$margin_legend_b, "bigpts"),

    panel.background =   NULL,
    panel.border =       element_blank(),
    panel.grid =         element_blank(),
    panel.spacing =      unit(consts$half_line * 2, "bigpts"),
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
    strip.switch.pad.grid = unit(consts$half_line / 2, "bigpts"),
    strip.switch.pad.wrap = unit(consts$half_line / 2, "bigpts"),


    plot.background =    element_blank(),
    plot.title =         element_text(family = cmapplot_globals$font$strong$family,
                                      face = cmapplot_globals$font$strong$face,
                                      size = fsize$L,
                                      hjust = 0, vjust = 1,
                                      margin = margin(b = consts$half_line)),
    plot.title.position = "panel",
    plot.subtitle =      element_blank(),
    plot.caption =       element_text(family = cmapplot_globals$font$light$family,
                                      face = cmapplot_globals$font$light$face,
                                      size = fsize$S,
                                      hjust = 1, vjust = 1,
                                      margin = margin(t = consts$half_line)),
    plot.caption.position = "panel",
    plot.tag = element_blank(),
    plot.margin = margin(3, consts$margin_panel_r, 3, 3, "bigpts"),

    complete = TRUE
  )

  # make sure all elements are set to NULL if not explicitly defined.
  # This could be set to ggplot2:::theme_all_null(), but it appears to be best
  # practice to use theme_gray() in case ggplot2 devs add new theme args in
  # future releases of that package.
  theme_gray() %+replace% t
}
