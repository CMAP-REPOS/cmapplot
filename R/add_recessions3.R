# attempt at modifying geom_rect

#' @export
#' @rdname geom_tile
geom_recessions <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      linejoin = "mitre",
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRecessions,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

filter_recessions <- function(min, max, xformat){
  print(xformat)
  print(min)
  print(max)
  # first, remove recessions outside of range
  recessions2 <- dplyr::filter(cmapplot::recessions, end_int > min & start_int < max)

  # second, if `min` or `max` fall in  middle of a recession, modify recession to
  # end at specified term.
  # Simultaneously, rename and add variables for geom_rect.
  recessions2 <- dplyr::transmute(recessions2,
                                  xmin = if_else(start_int < min, min, start_int),
                                  xmax = if_else(end_int > max, max, end_int),
                                  ymin = -Inf,
                                  ymax = Inf,
                                  PANEL = 1,
                                  group = -1
                                  )

  #  If x axis data is in date format, convert xmin and xmax to dates
  if (xformat == "date"){
    recessions2 <- dplyr::mutate(recessions2,
                                 xmin1 = lubridate::date_decimal(xmin),
                                 xmax1 = lubridate::date_decimal(xmax)
                            )
  }

  print(recessions2)

  return(recessions2)
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRecessions <- ggproto("GeomRecessions", Geom,
                    default_aes = aes(colour = NA, fill = "#002d49", alpha = 0.11, size = 0.5, linetype = 1, xformat = NULL),

                    #required_aes = c("xmin", "xmax"),

                    # replace `data` with `recessions`, filtered by `data`
                    setup_data = function(data, params) {

                      if (is.null(params$xformat)) params$xformat <- "numeric"

                      filter_recessions(min = min(data$x), max = max(data$x), xformat = params$xformat)
                    },


                    draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {

                      if (!coord$is_linear()) {
                        aesthetics <- setdiff(
                          names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
                        )

                        polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
                          poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
                          aes <- new_data_frame(row[aesthetics])[rep(1,5), ]

                          GeomPolygon$draw_panel(cbind(poly, aes), panel_params, coord)
                        })

                        ggname("bar", do.call("grobTree", polys))
                      } else {
                        coords <- coord$transform(data, panel_params)
                        ggname("geom_rect", rectGrob(
                          coords$xmin, coords$ymax,
                          width = coords$xmax - coords$xmin,
                          height = coords$ymax - coords$ymin,
                          default.units = "native",
                          just = c("left", "top"),
                          gp = gpar(
                            col = coords$colour,
                            fill = alpha(coords$fill, coords$alpha),
                            lwd = coords$size * .pt,
                            lty = coords$linetype,
                            linejoin = linejoin,
                            # `lineend` is a workaround for Windows and intentionally kept unexposed
                            # as an argument. (c.f. https://github.com/tidyverse/ggplot2/issues/3037#issuecomment-457504667)
                            lineend = if (identical(linejoin, "round")) "round" else "square"
                          )
                        ))
                      }
                    },

                    draw_key = draw_key_polygon
)


ggplot(grp_goods, aes(x = year, y = realgrp, color = cluster)) +
  geom_recessions(xformat = "numeric") +
  geom_line() +
  theme_minimal()


ggplot(grp_over_time, aes(x = year, y = realgrp, color = cluster)) +
  geom_recessions() +
  geom_line() +
  theme_minimal()






load("~/GitHub/cmapplot/data/grp_over_time.RData")
load("~/GitHub/cmapplot/data/recessions.RData")



ggplot(recessions) +
  geom_recessions(aes(xmin = start_int, xmax = end_int, ymin = 0, ymax = 1)) +
  theme_minimal()

# help!
