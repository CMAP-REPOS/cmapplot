#' Text (Last Only)
#'
#' Label only the last point(s) on a plot. \code{geom_text_lastonly()} can be
#' used instead of \code{ggplot2::geom_text()} when only the last point(s)
#' should be labeled. This is accomplished by identifying the maximum value of
#' \code{x} in \code{data} and applying a filter to omit records where \code{x}
#' is less than the maximum.
#'
#' Labels are placed by default to the right of the final point, and may be
#' partially cut off by the plot limits. There are two known ways to address
#' this: \enumerate{\item Turn off panel clipping, e.g. with
#' \code{coord_cartesian(clip = "off")}. Substitute the correct coordinate
#' system for your plot--all have a \code{clip} argument available. Note that
#' this will allow all geoms in the plot to draw outside the panel area, which
#' may have unintended consequences. \item Manually expand the \code{x} scale,
#' e.g. with \code{scale_x_continuous(expand=expand_scale(mult=0.10))} or
#' \code{coord_cartesian(xlim = c(min, max))}.}
#'
#' Code was mostly copied from the source of \code{ggplot2::geom_text()} and
#' \code{ggplot2::geom_point()}.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param parse If \code{TRUE}, the labels will be parsed into expressions and
#'   displayed as described in \code{?plotmath}.
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from points, particularly on discrete scales.
#'   Cannot be jointy specified with \code{position}.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function. Cannot be jointy specified with
#'   \code{nudge_x} or \code{nudge_y}.
#' @param check_overlap If \code{TRUE}, text that overlaps previous text in the
#'   same layer will not be plotted. \code{check_overlap} happens at draw time
#'   and in the order of the data. Therefore data should be arranged by the
#'   label column before calling \code{geom_text_lastonly()}.
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
#'   geom_text_lastonly()
#'
#' # With points, label formatting and x-axis expansion
#' ggplot(df, aes(x=year, y=value, color=var, label=sprintf("%.1f%%", 100*value))) +
#'   geom_line() +
#'   labs(title="Random lines") +
#'   scale_y_continuous("Percentage of absolutely nothing", labels=scales::percent) +
#'   scale_x_continuous("Year", expand=expansion(mult=c(0.05, 0.10))) +
#'   geom_text_lastonly(add_points=TRUE, text_aes=list(fontface="bold"), point_aes=list(size=2.5))
#'
#' @export
geom_text_lastonly <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = NULL,
                      parse = FALSE,
                      nudge_x = 0.25,
                      nudge_y = 0,
                      check_overlap = FALSE,
                      na.rm = FALSE,
                      show.legend = FALSE,
                      inherit.aes = TRUE,
                      add_points = FALSE,
                      text_aes = NULL,
                      point_aes = NULL,
                      ...
                      )
{
  if (is.null(position)) {
    position_lab <- position_nudge(nudge_x, nudge_y)
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
      geom = GeomTextLast,
      position = position_lab,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = append(
        list(
          parse = parse,
          check_overlap = check_overlap,
          na.rm = na.rm,
          ...
        ),
        text_aes
      )
    )
  )
  return(elements)
}


# Define modified text geom for last label(s) only
GeomTextLast <- ggproto(
  "GeomTextLast", Geom,
  required_aes = c("x", "y"),

  default_aes = aes(
    colour = "black", size = 3.88, angle = 0, hjust = 0, vjust = 0.5,
    alpha = NA, family = "", fontface = 1, lineheight = 1.2,
    label = NA,
    # Dummy parameters to match named params in GeomPointLast:
    shape = NA, fill = NA, stroke = NA
  ),

  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE) {
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
        fontfamily = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight
      ),
      check.overlap = check_overlap
    )
  },

  draw_key = draw_key_text
)

# GeomTextLast helper functions
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


# Define modified point geom for last point(s) only
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
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

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
