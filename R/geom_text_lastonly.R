#' Label the last point(s) on a plot. Code mostly copied fromgeom_text source.
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
                      show.legend = NA,
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
        fontfamily = data$family,
        fontface = data$fontface,
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
# windowsFonts(`Whitney Semibold` = "TT Whitney Semibold")
# font_label <- "Whitney Semibold"
# df = data.frame(year=2000:2020, var=runif(21))
# ggplot(df,aes(x=year, y=var, label=sprintf("%.1f%%", 100*var))) +
#  ggtitle("Random line") +
#  scale_x_continuous("Year", expand=expand_scale(mult=c(0.05, 0.10))) +  # Expand x-axis to accomodate label
#  scale_y_continuous("Percentage of absolutely nothing", labels=scales::percent) +
#  geom_line(size=1) +
#  theme_cmap() +
#  geom_text_lastonly(family=font_label)  # Specify font
