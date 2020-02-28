#' Text (Last Only)
#'
#' Label only the last point(s) on a plot. `geom_text_lastonly()` can be used
#' instead of `ggplot2::geom_text()` when only the last point(s) should be
#' labeled. This is accomplished by identifying the maximum value of `x` in
#' `data` and applying afilter to omit records where `x` is less than the
#' maximum.
#'
#' Labels are automatically placed to the right of the final point,
#' and may be partially cut off by the plot limits, unless the `x` scale is
#' expanded, e.g. with `scale_x_continuous(expand=expand_scale(mult=0.10))`.
#'
#' Code was mostly copied from `ggplot2::geom_text()`'s source.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param parse If `TRUE`, the labels will be parsed into expressions and
#'   displayed as described in `?plotmath`.
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from points, particularly on discrete scales.
#'   Cannot be jointy specified with `position`.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function. Cannot be jointy specified with
#'  `nudge_x` or `nudge_y`.
#' @param check_overlap If `TRUE`, text that overlaps previous text in the
#'   same layer will not be plotted. `check_overlap` happens at draw time and in
#'   the order of the data. Therefore data should be arranged by the label
#'   column before calling `geom_label()` or `geom_text()`.
#'
#' @export
geom_text_lastonly <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = NULL,
                      ...,
                      parse = FALSE,
                      nudge_x = 0.25,
                      nudge_y = 0,
                      check_overlap = FALSE,
                      na.rm = FALSE,
                      show.legend = FALSE,
                      inherit.aes = TRUE)
{
  if (is.null(position)) {
    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextLast,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTextLast <- ggproto(
  "GeomTextLast", Geom,
  required_aes = c("x", "y", "label"),

  default_aes = aes(
    colour = "black", size = 3.88, angle = 0, hjust = 0, vjust = 0.5,
    alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE) {
    x_max <- max(unique(data$x))
    data <- data[data$x == x_max,]
    lab <- data$label
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }

    data <- coord$transform(data, panel_params)
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }

    textGrob(
      lab,
      data$x, data$y, default.units = "native",
      hjust = data$hjust, vjust = data$vjust,
      rot = data$angle,
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        fontsize = data$size * .pt,
        fontfamily = cmapplot_globals$font_label, #data$family,
        fontface = cmapplot_globals$font_label_face, #data$fontface,
        lineheight = data$lineheight
      ),
      check.overlap = check_overlap
    )
  },

  draw_key = draw_key_text
)

compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}


# ### TEST PLOT ###
# df <- data.frame(year=2010:2020, var=runif(22), byvar=c(rep("A", 11), rep("B", 11)))
# ggplot(df, aes(x=year, y=var, color=byvar, label=sprintf("%.1f%%", 100*var))) +
#   geom_line() +
#   labs(title="Random lines") +
#   scale_y_continuous("Percentage of absolutely nothing", labels=scales::percent) +
#   scale_x_continuous("Year", expand=expand_scale(mult=c(0.05, 0.10))) +  # Expand x-axis to accomodate label
#   geom_text_lastonly()
