#' Text (Last Only) New
#'
#' Label only the last point(s) on a plot. \code{geom_text_lastonly_new()} can be
#' used instead of \code{ggplot2::geom_text()} when only the last point(s)
#' should be labeled. This is accomplished by identifying the maximum value of
#' \code{x} in \code{data} and applying a filter to omit records where \code{x}
#' is less than the maximum.
#'
#' Labels are placed by default to the right of the final point, and may be
#' partially cut off by the plot limits. There are two known ways to address
#' this: \enumerate{ \item Turn off panel clipping, e.g. with
#' \code{coord_cartesian(clip = "off")}. Substitute the correct coordinate
#' system for your plot--all have a \code{clip} argument available. Note that
#' this will allow all geoms in the plot to draw outside the panel area, which
#' may have unintended consequences. \item Manually expand the \code{x} scale,
#' e.g. with \code{scale_x_continuous(expand=expand_scale(mult=0.10))} or
#' \code{coord_cartesian(xlim = c(min, max))}. }
#'
#' Code was mostly copied from the source of \code{ggrepel::geom_text_repel()} and
#' \code{ggplot2::geom_point()}.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' #' @section Alignment with \code{hjust} or \code{vjust}:
#' The arguments \code{hjust} and \code{vjust} are supported, but they only
#' control the initial positioning, so repulsive forces may disrupt alignment.
#' Alignment with \code{hjust} will be preserved if labels only move up and down
#' by using \code{direction="y"}. For \code{vjust}, use \code{direction="x"}.
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}} or
#'   \code{\link[ggplot2]{aes_}}. If specified and \code{inherit.aes = TRUE} (the
#'   default), is combined with the default mapping at the top level of the
#'   plot. You only need to supply \code{mapping} if there isn't a mapping
#'   defined for the plot.
#' @param data A data frame. If specified, overrides the default data frame
#'   defined at the top level of the plot.
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in ?plotmath
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. There are
#'   three types of arguments you can use here:
#'
#'   \itemize{
#'     \item Aesthetics: to set an aesthetic to a fixed value, like
#'        \code{colour = "red"} or \code{size = 3}.
#'     \item Other arguments to the layer, for example you override the
#'       default \code{stat} associated with the layer.
#'     \item Other arguments passed on to the stat.
#'   }
#' @param nudge_x,nudge_y Horizontal and vertical adjustments to nudge the
#'   starting position of each text label. The units for \code{nudge_x} and
#'   \code{nudge_y} are the same as for the data units on the x-axis and y-axis.
#' @param xlim,ylim Limits for the x and y axes. Text labels will be constrained
#'   to these limits. By default, text labels are constrained to the entire plot
#'   area.
#' @param box.padding Amount of padding around bounding box, as unit or number.
#'   Defaults to 0.25. (Default unit is lines, but other units can be specified
#'   by passing \code{unit(x, "units")}).
#' @param point.padding Amount of padding around labeled point, as unit or
#'   number. Defaults to 0. (Default unit is lines, but other units can be
#'   specified by passing \code{unit(x, "units")}).
#' @param min.segment.length Skip drawing segments shorter than this, as unit or
#'   number. Defaults to 0.5. (Default unit is lines, but other units can be
#'   specified by passing \code{unit(x, "units")}).
#' @param arrow specification for arrow heads, as created by \code{\link[grid]{arrow}}
#' @param force Force of repulsion between overlapping text labels. Defaults
#'   to 1.
#' @param force_pull Force of attraction between a text label and its
#'   corresponding data point. Defaults to 1.
#' @param max.time Maximum number of seconds to try to resolve overlaps.
#'   Defaults to 0.5.
#' @param max.iter Maximum number of iterations to try to resolve overlaps.
#'   Defaults to 10000.
#' @param max.overlaps Exclude text labels when they overlap too many other
#'   things. For each text label, we count how many other text labels or other
#'   data points it overlaps, and exclude the text label if it has too many overlaps.
#'   Defaults to 10.
#' @param direction "both", "x", or "y" -- direction in which to adjust position of labels
#' @param seed Random seed passed to \code{\link[base]{set.seed}}. Defaults to
#'   \code{NA}, which means that \code{set.seed} will not be called.
#' @param verbose If \code{TRUE}, some diagnostics of the repel algorithm are printed
#' @param add_points If \code{TRUE}, points will be added to the plot (for the
#'   labeled data only). Default size=2, color will match line color.
#' @param text_aes,point_aes Named list, additional aesthetics to send to the
#'   text and point geoms, respectively.
#' @param ... Additional aesthetics to send to BOTH the point and text geoms.
#'   Note that if \code{add_points = FALSE}, additional parameters can be passed
#'   to the text geom here, rather than in \code{text_aes}, without breaking.
#'
#' @examples
#' df <- data.frame(year=2010:2020, value=runif(22), var=c(rep("A", 11), rep("B", 11)))
#'
#' # Without points, label formatting or x-axis expansion
#' ggplot(df, aes(x=year, y=value, color=var)) +
#'   geom_line() +
#'   labs(title="Random lines") +
#'   scale_y_continuous("Percentage of absolutely nothing") +
#'   scale_x_continuous("Year") +
#'   geom_text_lastonly_new()
#'
#' # With points, label formatting and x-axis expansion
#' ggplot(df, aes(x=year, y=value, color=var, label=sprintf("%.1f%%", 100*value))) +
#'   geom_line() +
#'   labs(title="Random lines") +
#'   scale_y_continuous("Percentage of absolutely nothing", labels=scales::percent) +
#'   scale_x_continuous("Year", expand=expansion(mult=c(0.05, 0.10))) +
#'   geom_text_lastonly_new(add_points=TRUE, text_aes=list(fontface="bold"), point_aes=list(size=2.5))
#'
#' @export
geom_text_lastonly_new <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = NULL,
                      parse = FALSE,
                      box.padding = 0.25,
                      point.padding = 1e-6,
                      min.segment.length = 0.5,
                      arrow = NULL,
                      force = 1,
                      force_pull = 1,
                      max.time = 0.5,
                      max.iter = 10000,
                      max.overlaps = getOption("ggrepel.max.overlaps", default = 10),
                      nudge_x = 0.25,
                      nudge_y = 0,
                      xlim = c(NA, NA),
                      ylim = c(NA, NA),
                      na.rm = FALSE,
                      check_overlap = FALSE,
                      direction = c("both","y","x"),
                      seed = NA,
                      verbose = FALSE,
                      show.legend = FALSE,
                      inherit.aes = TRUE,
                      add_points = FALSE,
                      text_aes = NULL,
                      point_aes = NULL,
                      ...
                      )
{
  if (is.null(position)) {
    position_lab <- position_nudge_repel(nudge_x, nudge_y)
    position_pt <- position_identity()
  }

  elements <- list(
    if (add_points) {
      layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomPointLast,
        position = position_pt,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = append(
          list(
            na.rm = na.rm,
            ...
          ),
          point_aes
        )
      )
    },
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomTextLastNew,
      position = position_lab,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = append(
        list(
          parse = parse,
          na.rm = na.rm,
          box.padding = to_unit(box.padding),
          point.padding = to_unit(point.padding),
          min.segment.length = to_unit(min.segment.length),
          arrow = arrow,
          force = force,
          force_pull = force_pull,
          max.time = max.time,
          max.iter = max.iter,
          max.overlaps = max.overlaps,
          nudge_x = nudge_x,
          nudge_y = nudge_y,
          xlim = xlim,
          ylim = ylim,
          direction = match.arg(direction),
          seed = seed,
          verbose = verbose,
          ...
        ),
        text_aes
      )
    )
  )
  return(elements)
}


#' Custom ggproto classes
#'
#' The \code{cmapplot} package contains a few custom ggproto objects. For the
#' most part, these are slightly tweaked versions of ggplot2's default proto
#' objects. For more information about these, see
#' \code{\link[ggplot2:ggplot2-ggproto]{ggplot2::ggplot2-ggproto}}.
#'
#' @name customproto
NULL

#' @describeIn customproto Add text to plot for maximum x-value in dataset only.
#' @format NULL
#' @usage NULL
#' @export
GeomTextLastNew <- ggproto(
  "GeomTextLastNew", Geom,
  required_aes = c("x", "y"),

  default_aes = aes(
    colour = "black", size = 3.88, angle = 0, label = NA,
    alpha = NA, family = "", fontface = 1, lineheight = 1.2,
    hjust = 0.5, vjust = 0.5, point.size = 1,
    segment.linetype = 1, segment.colour = "transparent",
    segment.size = 0.75, segment.alpha = NULL,
    segment.curvature = 0, segment.angle = 90, segment.ncp = 1,
    segment.shape = 0.5, segment.square = TRUE, segment.squareShape = 1,
    segment.inflect = FALSE, segment.debug = FALSE,
    bg.colour = NA, bg.r = 0.1,
    # Dummy parameters to match named params in GeomPointLast:
    shape = NA, fill = NA, stroke = NA
  ),

  draw_panel = function(
    data, panel_scales, coord,
    parse = FALSE,
    na.rm = FALSE,
    box.padding = 0.25,
    point.padding = 1e-6,
    min.segment.length = 0.5,
    arrow = NULL,
    force = 1,
    force_pull = 1,
    max.time = 0.5,
    max.iter = 10000,
    max.overlaps = 10,
    nudge_x = 0,
    nudge_y = 0,
    xlim = c(NA, NA),
    ylim = c(NA, NA),
    direction = "y",
    seed = NA,
    verbose = FALSE
  ) {
    # Filter labeled dataset to include maximum x-value only
    x_max <- max(unique(data$x))
    data <- data[data$x == x_max,]

    # Use y-var as label if not otherwise specified
    if (!is.na(data$label[[1]])) {
      lab <- data$label
    } else {
      lab <- data$y
    }
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }

    # if needed rename columns using our convention
    for (this_dim in c("x", "y")) {
      this_orig <- sprintf("%s_orig", this_dim)
      this_nudge <- sprintf("nudge_%s", this_dim)
      if (!this_nudge %in% colnames(data)) {
        data[[this_nudge]] <- data[[this_dim]]
        if (this_orig %in% colnames(data)) {
          data[[this_dim]] <- data[[this_orig]]
          data[[this_orig]] <- NULL
        }
      }
    }

    # Transform the nudges to the panel scales.
    nudges <- data.frame(x = data$nudge_x, y = data$nudge_y)
    nudges <- coord$transform(nudges, panel_scales)

    # Transform the raw data to the panel scales.
    data <- coord$transform(data, panel_scales)

    # The nudge is relative to the data.
    data$nudge_x <- nudges$x - data$x
    data$nudge_y <- nudges$y - data$y

    # Transform limits to panel scales.
    limits <- data.frame(x = xlim, y = ylim)
    limits <- coord$transform(limits, panel_scales)

    # Allow Inf.
    if (length(limits$x) == length(xlim)) {
      limits$x[is.infinite(xlim)] <- xlim[is.infinite(xlim)]
    }
    if (length(limits$y) == length(ylim)) {
      limits$y[is.infinite(ylim)] <- ylim[is.infinite(ylim)]
    }

    # Fill NAs with defaults.
    limits$x[is.na(limits$x)] <- c(0, 1)[is.na(limits$x)]
    limits$y[is.na(limits$y)] <- c(0, 1)[is.na(limits$y)]

    # Warn about limitations of the algorithm
    if (any(abs(data$angle %% 90) > 5)) {
      warn("ggrepel: Repulsion works correctly only for rotation angles multiple of 90 degrees")
    }

    # Convert hjust and vjust to numeric if character
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y, data$x, data$angle)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x, data$y, data$angle)
    }

    ggname("geom_text_lastonly_new", gTree(
      limits = limits,
      data = data,
      lab = data$label,
      box.padding = to_unit(box.padding),
      point.padding = to_unit(point.padding),
      min.segment.length = to_unit(min.segment.length),
      arrow = arrow,
      force = force,
      force_pull = force_pull,
      max.time = max.time,
      max.iter = max.iter,
      max.overlaps = max.overlaps,
      direction = direction,
      seed = seed,
      verbose = verbose,
      cl = "textlastrepel"
    ))

  },

  draw_key = draw_key_text
)

#' grid::makeContent function for the grobTree of textRepelGrob objects
#' @param x A grid grobTree.
#' @export
#' @noRd
makeContent.textlastrepel <- function(x) {

  # The padding around each bounding box.
  box_padding_x <- convertWidth(x$box.padding, "native", valueOnly = TRUE)
  box_padding_y <- convertHeight(x$box.padding, "native", valueOnly = TRUE)

  # The padding around each point.
  if (is.na(x$point.padding)) {
    x$point.padding = unit(0, "lines")
  }

  # Do not create text labels for empty strings.
  valid_strings <- which(not_empty(x$lab))
  invalid_strings <- which(!not_empty(x$lab))
  ix <- c(valid_strings, invalid_strings)
  x$data <- x$data[ix,]
  x$lab <- x$lab[ix]

  # Create a dataframe with x1 y1 x2 y2
  boxes <- lapply(seq_along(valid_strings), function(i) {
    row <- x$data[i, , drop = FALSE]
    tg <- textGrob(
      x$lab[i],
      row$x, row$y, default.units = "native",
      rot = row$angle,
      hjust = row$hjust,
      vjust = row$vjust,
      gp = gpar(
        col = alpha(row$colour, row$alpha),
        fontsize = row$size * .pt,
        fontfamily = row$family,
        fontface = row$fontface,
        lineheight = row$lineheight
      )
    )
    x1 <- convertWidth(grobX(tg, "west"), "native", TRUE)
    x2 <- convertWidth(grobX(tg, "east"), "native", TRUE)
    y1 <- convertHeight(grobY(tg, "south"), "native", TRUE)
    y2 <- convertHeight(grobY(tg, "north"), "native", TRUE)
    c(
      "x1" = x1 - box_padding_x + row$nudge_x,
      "y1" = y1 - box_padding_y + row$nudge_y,
      "x2" = x2 + box_padding_x + row$nudge_x,
      "y2" = y2 + box_padding_y + row$nudge_y
    )
  })

  # Make the repulsion reproducible if desired.
  if (!is.null(x$seed) && is.na(x$seed)) {
    x$seed <- sample.int(.Machine$integer.max, 1L)
  }

  # The points are represented by circles.
  x$data$point.size[is.na(x$data$point.size)] <- 0

  # Beware the magic numbers. I do not understand them.
  # I just accept them as necessary to get the code to work.
  p_width <- convertWidth(unit(1, "npc"), "inch", TRUE)
  p_height <- convertHeight(unit(1, "npc"), "inch", TRUE)
  p_ratio <- (p_width / p_height)
  if (p_ratio > 1) {
    p_ratio <- p_ratio ^ (1 / (1.15 * p_ratio))
  }
  point_size <- p_ratio * convertWidth(
    to_unit(x$data$point.size), "native", valueOnly = TRUE
  ) / 13
  point_padding <- p_ratio * convertWidth(
    to_unit(x$point.padding), "native", valueOnly = TRUE
  ) / 13

  # Repel overlapping bounding boxes away from each other.
  repel <- with_seed_null(x$seed, repel_boxes2(
    data_points     = as.matrix(x$data[,c("x","y")]),
    point_size      = point_size,
    point_padding_x = point_padding,
    point_padding_y = point_padding,
    boxes           = do.call(rbind, boxes),
    xlim            = range(x$limits$x),
    ylim            = range(x$limits$y),
    hjust           = x$data$hjust %||% 0.5,
    vjust           = x$data$vjust %||% 0.5,
    force_push      = x$force * 1e-6,
    force_pull      = x$force_pull * 1e-2,
    max_time        = x$max.time,
    max_iter        = ifelse(is.infinite(x$max.iter), 1e9, x$max.iter),
    max_overlaps    = x$max.overlaps,
    direction       = x$direction,
    verbose         = x$verbose
  ))

  if (any(repel$too_many_overlaps)) {
    warn(
      sprintf(
        "ggrepel: %s unlabeled data points (too many overlaps). Consider increasing max.overlaps",
        sum(repel$too_many_overlaps)
      )
    )
  }

  if (all(repel$too_many_overlaps)) {
    grobs <- list()
    class(grobs) <- "gList"
    return(setChildren(x, grobs))
  }

  grobs <- lapply(seq_along(valid_strings), function(i) {
    if (!repel$too_many_overlaps[i]) {
      row <- x$data[i, , drop = FALSE]
      makeTextRepelGrobs(
        i,
        x$lab[i],
        # Position of text bounding boxes.
        x = unit(repel$x[i], "native"),
        y = unit(repel$y[i], "native"),
        # Position of original data points.
        x.orig = row$x,
        y.orig = row$y,
        rot = row$angle,
        box.padding = x$box.padding,
        point.size = point_size[i],
        point.padding = x$point.padding,
        segment.curvature = row$segment.curvature,
        segment.angle     = row$segment.angle,
        segment.ncp       = row$segment.ncp,
        segment.shape = row$segment.shape,
        segment.square = row$segment.square,
        segment.squareShape = row$segment.squareShape,
        segment.inflect = row$segment.inflect,
        segment.debug = row$segment.debug,
        text.gp = gpar(
          col = scales::alpha(row$colour, row$alpha),
          fontsize = row$size * .pt,
          fontfamily = row$family,
          fontface = row$fontface,
          lineheight = row$lineheight
        ),
        segment.gp = gpar(
          col = scales::alpha(row$segment.colour %||% row$colour, row$segment.alpha %||% row$alpha),
          lwd = row$segment.size * .pt,
          lty = row$segment.linetype %||% 1
        ),
        arrow = x$arrow,
        min.segment.length = x$min.segment.length,
        hjust = row$hjust,
        vjust = row$vjust,
        bg.colour = alpha(row$bg.colour, row$alpha),
        bg.r = row$bg.r
      )
    }
  })

  grobs <- unlist(grobs, recursive = FALSE)
  class(grobs) <- "gList"

  # Put segment grobs before text grobs.
  grob_names <- sapply(grobs, "[[", "name")
  grobs <- grobs[order(!grepl("^segment", grob_names))]

  setChildren(x, grobs)
}

makeTextRepelGrobs <- function(
    i,
    label,
    # Position of text bounding boxes.
    x = unit(0.5, "npc"),
    y = unit(0.5, "npc"),
    # Position of original data points.
    x.orig = NULL,
    y.orig = NULL,
    rot = 0,
    default.units = "npc",
    box.padding = 0.25,
    point.size = 1,
    point.padding = 1e-6,
    segment.curvature = 0,
    segment.angle = 90,
    segment.ncp = 1,
    segment.shape = 0.5,
    segment.square = TRUE,
    segment.squareShape = 1,
    segment.inflect = FALSE,
    segment.debug = FALSE,
    name = NULL,
    text.gp = gpar(),
    segment.gp = gpar(),
    vp = NULL,
    arrow = NULL,
    min.segment.length = 0.5,
    hjust = 0.5,
    vjust = 0.5,
    bg.colour = NA,
    bg.r = .1
) {
  stopifnot(length(label) == 1)

  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  # support any angle by converting to -360..360
  rot <- rot %% 360

  # Instead of the width and height of the Grob we use the dimensions of the
  # character string which are independent of rotation, matching those of
  # a textGrob built with rot = 0.
  # To support rotation height and width need to be expressed in units that
  # are consistent on x and y axes, such as "char".
  string.height <- convertHeight(stringHeight(label), "char")
  string.width <- convertWidth(stringWidth(label), "char")

  rot_radians <- rot * pi / 180

  x_adj <- x - cos(rot_radians) * string.width * (0.5 - hjust) +
    sin(rot_radians) * string.height * (0.5 - vjust)
  y_adj <- y - cos(rot_radians) * string.height * (0.5 - vjust) -
    sin(rot_radians) * string.width * (0.5 - hjust)

  grobs <- shadowtextGrob(
    label = label,
    x = x_adj,
    y = y_adj,
    rot = rot,
    default.units = "native",
    hjust = hjust,
    vjust = vjust,
    gp = text.gp,
    name = sprintf("textrepelgrob%s", i),
    bg.colour = bg.colour,
    bg.r = bg.r
  )
  # the regular textgrob will always be the last one
  tg <- grobs[[length(grobs)]]

  x1 <- convertWidth(grobX(tg, "west"), "native", TRUE)
  x2 <- convertWidth(grobX(tg, "east"), "native", TRUE)
  y1 <- convertHeight(grobY(tg, "south"), "native", TRUE)
  y2 <- convertHeight(grobY(tg, "north"), "native", TRUE)

  point_pos <- c(x.orig, y.orig)

  # Get the coordinates of the intersection between the line from the
  # original data point to the centroid and the rectangle's edges.
  extra_padding_x <- convertWidth(unit(0.25, "lines"), "native", TRUE) / 2
  extra_padding_y <- convertHeight(unit(0.25, "lines"), "native", TRUE) / 2
  text_box <- c(
    x1 - extra_padding_x, y1 - extra_padding_y,
    x2 + extra_padding_x, y2 + extra_padding_y
  )
  #int <- intersect_line_rectangle(point_pos, center, text_box)
  int <- select_line_connection(point_pos, text_box)

  # Check if the data point is inside the label box.
  point_inside_text <- FALSE
  if (text_box[1] <= point_pos[1] && point_pos[1] <= text_box[3] &&
      text_box[2] <= point_pos[2] && point_pos[2] <= text_box[4]) {
    point_inside_text <- TRUE
  }

  # This seems just fine.
  point.padding <- convertWidth(to_unit(point.padding), "native", TRUE) / 2

  point_int <- intersect_line_circle(int, point_pos, (point.size + point.padding))

  # Compute the distance between the data point and the edge of the text box.
  dx <- abs(int[1] - point_int[1])
  dy <- abs(int[2] - point_int[2])
  d <- sqrt(dx * dx + dy * dy)

  # Scale the unit vector by the minimum segment length.
  if (d > 0) {
    mx <- convertWidth(min.segment.length, "native", TRUE)
    my <- convertHeight(min.segment.length, "native", TRUE)
    min.segment.length <- sqrt((mx * dx / d) ^ 2 + (my * dy / d) ^ 2)
  }

  if (
    !point_inside_text &&
    d > 0 &&
    # Distance from label to point edge is greater than minimum.
    (!is.na(min.segment.length) && euclid(int, point_int) > min.segment.length) &&
    # Distance from label to point edge is less than from label to point center.
    euclid(int, point_int) < euclid(int, point_pos) &&
    # Distance from label to point center is greater than point size.
    euclid(int, point_pos) > point.size &&
    # Distance from label to point center is greater than from point edge to point center.
    euclid(int, point_pos) > euclid(point_int, point_pos)
  ) {
    s <- curveGrob(
      x1 = int[1],
      y1 = int[2],
      x2 = point_int[1],
      y2 = point_int[2],
      default.units = "native",
      curvature = segment.curvature,
      angle = segment.angle,
      ncp = segment.ncp,
      shape = segment.shape,
      square = segment.square,
      squareShape = segment.squareShape,
      inflect = segment.inflect,
      debug = segment.debug,
      gp = segment.gp,
      name = sprintf("segmentrepelgrob%s", i),
      arrow = arrow
    )
    grobs[[s$name]] <- s
  }

  grobs
}

# GeomTextLastNew helper functions
# copied from ggplot2
compute_just <- function(just, a, b = a, angle = 0) {
  #  As justification direction is relative to the text, not the plotting area
  #  we need to swap x and y if text direction is rotated so that hjust is
  #  applied along y and vjust along x.
  if (any(grepl("outward|inward", just))) {
    # ensure all angles are in -360...+360
    angle <- angle %% 360
    # ensure correct behaviour for angles in -360...+360
    angle <- ifelse(angle > 180, angle - 360, angle)
    angle <- ifelse(angle < -180, angle + 360, angle)
    rotated_forward <-
      grepl("outward|inward", just) & (angle > 45 & angle < 135)
    rotated_backwards <-
      grepl("outward|inward", just) & (angle < -45 & angle > -135)

    ab <- ifelse(rotated_forward | rotated_backwards, b, a)
    just_swap <- rotated_backwards | abs(angle) > 135
    inward <-
      (just == "inward" & !just_swap | just == "outward" & just_swap)
    just[inward] <- c("left", "middle", "right")[just_dir(ab[inward])]
    outward <-
      (just == "outward" & !just_swap) | (just == "inward" & just_swap)
    just[outward] <- c("right", "middle", "left")[just_dir(ab[outward])]

  }

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

# copied from ggplot2
just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}

# Adapted from shadowtext, at the time of writing located at:
# https://github.com/GuangchuangYu/shadowtext/blob/325d25919b28ccd4184c6363c11c8c26e822dd95/R/shadowtext-grob.R#L28
# This function was modified to always return a gList,
# whether bg.colour is NA or not.
# Each background textgrob is made to have a unique name, otherwise
# it can mess up the plotting order.
shadowtextGrob <- function(
    label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
    hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE,
    default.units = "npc", name = NULL, gp = gpar(col="white"), vp = NULL,
    bg.colour = "black", bg.r = 0.1
) {
  upperGrob <- textGrob(
    label = label, x = x, y = y, hjust = hjust,
    vjust = vjust, rot = rot, default.units = default.units,
    check.overlap = check.overlap, name = name, gp = gp, vp = vp
  )

  if (is.na(bg.colour)) {
    gList(upperGrob)
  } else {
    gp$col <- bg.colour

    theta <- seq(pi/8, 2*pi, length.out=16)
    char <- "X"
    # char <- substring(label[1], 1, 1)
    r <- bg.r[1]

    if (!is.unit(x)) {
      x <- unit(x, default.units)
    }
    if (!is.unit(y)) {
      y <- unit(y, default.units)
    }

    bgList <- lapply(theta, function(i) {
      x <- x + unit(cos(i) * r, "strheight", data = char)
      y <- y + unit(sin(i) * r, "strheight", data = char)
      textGrob(
        label = label, x = x, y = y, hjust = hjust,
        vjust = vjust, rot = rot, default.units = default.units,
        check.overlap = check.overlap, name = paste0(name, "-shadowtext", i), gp = gp, vp = vp
      )
    })

    do.call(gList, c(bgList, list(upperGrob)))
  }
}

#' @describeIn customproto Add points to plot for maximum x-value in dataset only.
#' @format NULL
#' @usage NULL
#' @export
GeomPointLast <- ggproto(
  "GeomPointLast", Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = aes(
    shape = 19, colour = "black", size = 2.0, fill = NA,
    alpha = NA, stroke = 0.5,
    # Dummy parameters to match named params in GeomTextLast:
    angle = NA, hjust = NA, vjust = NA, family = NA,
    fontface = NA, lineheight = NA, label = NA
  ),

  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    # Filter labeled dataset to include maximum x-value only
    x_max <- max(unique(data$x))
    data <- data[data$x == x_max,]

    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }

    coords <- coord$transform(data, panel_params)
    ggname("geom_point",
           pointsGrob(
             coords$x, coords$y,
             pch = coords$shape,
             gp = gpar(
               col = alpha(coords$colour, coords$alpha),
               fill = alpha(coords$fill, coords$alpha),
               # Stroke is added around the outside of the point
               fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
               lwd = coords$stroke * .stroke / 2
             )
           )
    )
  },

  draw_key = draw_key_point
)

# GeomPointLast helper functions
translate_shape_string <- function(shape_string) {
  # strings of length 0 or 1 are interpreted as symbols by grid
  if (nchar(shape_string[1]) <= 1) {
    return(shape_string)
  }

  pch_table <- c(
    "square open"           = 0,
    "circle open"           = 1,
    "triangle open"         = 2,
    "plus"                  = 3,
    "cross"                 = 4,
    "diamond open"          = 5,
    "triangle down open"    = 6,
    "square cross"          = 7,
    "asterisk"              = 8,
    "diamond plus"          = 9,
    "circle plus"           = 10,
    "star"                  = 11,
    "square plus"           = 12,
    "circle cross"          = 13,
    "square triangle"       = 14,
    "triangle square"       = 14,
    "square"                = 15,
    "circle small"          = 16,
    "triangle"              = 17,
    "diamond"               = 18,
    "circle"                = 19,
    "bullet"                = 20,
    "circle filled"         = 21,
    "square filled"         = 22,
    "diamond filled"        = 23,
    "triangle filled"       = 24,
    "triangle down filled"  = 25
  )

  shape_match <- charmatch(shape_string, names(pch_table))

  invalid_strings <- is.na(shape_match)
  nonunique_strings <- shape_match == 0

  if (any(invalid_strings)) {
    bad_string <- unique(shape_string[invalid_strings])
    n_bad <- length(bad_string)

    collapsed_names <- sprintf("\n* '%s'", bad_string[1:min(5, n_bad)])

    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 5, ifelse(n_bad > 6, "s", ""))
    } else {
      ""
    }

    rlang::abort(glue::glue("Can't find shape name:", collapsed_names, more_problems))
  }

  if (any(nonunique_strings)) {
    bad_string <- unique(shape_string[nonunique_strings])
    n_bad <- length(bad_string)

    n_matches <- vapply(
      bad_string[1:min(5, n_bad)],
      function(shape_string) sum(grepl(paste0("^", shape_string), names(pch_table))),
      integer(1)
    )

    collapsed_names <- sprintf(
      "\n* '%s' partially matches %d shape names",
      bad_string[1:min(5, n_bad)], n_matches
    )

    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 5, ifelse(n_bad > 6, "s", ""))
    } else {
      ""
    }

    rlang::abort(glue::glue("Shape names must be unambiguous:", collapsed_names, more_problems))
  }

  unname(pch_table[shape_match])
}
