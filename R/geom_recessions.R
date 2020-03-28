#' @export
geom_recessions <- function(xformat = "numeric",
                            text = TRUE,
                            show.legend = FALSE,
                            rect_aes = NULL,
                            label = "   Recession",
                            text_nudge_x = 0.2,
                            text_nudge_y = 0,
                            text_aes = NULL
                            ) {

  list(
    layer(
      data = NULL,
      mapping = NULL,
      stat = "identity",
      geom = GeomRecessions,
      position = "identity",
      show.legend = show.legend,
      inherit.aes = TRUE,
      params = append(
        list(
          xformat = xformat
        ),
        rect_aes
      )
    ),
    if(text){
      layer(
        data = NULL,
        mapping = NULL,
        stat = "identity",
        geom = GeomRecessionsText,
        position = position_nudge(x = text_nudge_x, y = text_nudge_y),
        show.legend = FALSE,
        inherit.aes = TRUE,
        params = append(
          list(
            xformat = xformat,
            label = label
          ),
          text_aes
        )
      )
    }
  )

}



ggplot(grp_over_time, aes(x = year, y = realgrp, color = cluster)) +
  geom_recessions() +
  geom_line() +
  theme_minimal()


filter_recessions <- function(min, max, xformat){

  # filter recessions correctly, based on xformat
  if (xformat == "numeric"){
    recessions2 <- dplyr::rename(recessions, end = end_num, start = start_num)
  } else if (xformat == "date"){
    recessions2 <- dplyr::rename(recessions, end = end_date, start = start_date)
  } else {
    warning("geom_recessions currently only supports x axes in the numeric and date formats. Using numeric")
    recessions2 <- dplyr::rename(recessions, end = end_int, start = start_int)
  }

  # Remove recessions outside of range
  recessions2 <- dplyr::filter(recessions2, end > min & start < max)

  # If `min` or `max` fall in  middle of a recession, modify recession to end at specified term.
  recessions2 <- dplyr::transmute(recessions2,
                                  start = if_else(start < min, min, as.numeric(start)),
                                  end = if_else(end > max, max, as.numeric(end)),
                                  )

  return(recessions2)
}


GeomRecessions <- ggproto("GeomRecessions", Geom,
                    default_aes = aes(colour = NA, fill = "#002d49", alpha = 0.11, size = 0.5, linetype = 1, na.rm = TRUE),

                    required_aes = c("xformat"),

                    # replace `data` with `recessions`, filtered by `data`
                    setup_data = function(data, params) {
                      # set default xformat to numeric if user does not set it
                      if (is.null(params$xformat)) params$xformat <- "numeric"

                      #filter recessions based on date parameters from `data` and return it. This overwrites `data`.
                      data <- filter_recessions(min = min(data$x), max = max(data$x), xformat = params$xformat)

                      # set up data for GeomRect
                      data <- dplyr::transmute(data,
                                               xmin = start,
                                               xmax = end,
                                               ymin = -Inf,
                                               ymax = Inf,
                                               PANEL = 1,
                                               group = -1
                                               )

                      return(data)
                    },

                    # remainder untouched from `geom_rect`:
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


GeomRecessionsText <- ggproto("GeomRecessionsText", Geom,

                    required_aes = c("xformat", "label"),

                    default_aes = aes(
                      colour = "black", size = 3.88, alpha = NA, family = "", fontface = 1, lineheight = 1.2,
                      xformat = NULL, angle = 270, parse = FALSE,
                      check_overlap = FALSE, na.rm = TRUE,
                      hjust = "left", vjust = "bottom"
                    ),

                    # replace `data` with `recessions`, filtered by `data`
                    setup_data = function(data, params) {
                      #filter recessions based on date parameters from `data` and return it. This overwrites `data`.
                      data <- filter_recessions(min = min(data$x), max = max(data$x), xformat = params$xformat)

                      # set up data for GeomRect
                      data <- dplyr::transmute(data,
                                               x = end,
                                               y = Inf,
                                               PANEL = 1,
                                               group = -1
                      )

                      return(data)
                    },

                    # remainder untouched from `geom_text`:
                    draw_panel = function(data, panel_params, coord, parse = FALSE,
                                          na.rm = FALSE, check_overlap = FALSE) {
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






ggplot(grp_over_time, aes(x = year, y = realgrp, color = cluster)) +
  geom_recessions(rect_aes = list(alpha = 1, fill = "blue", color = "red"), text = FALSE) +
  geom_recessionstext(xformat = "numeric", nudge_x = .2) +
  geom_line() +
  theme_minimal()


time_series <- tibble(date_dec = 1900:2020, var = rnorm(121), var2 = rnorm(121)) %>%
pivot_longer(cols = starts_with("var"),
             names_to = "var") %>%
  mutate(date_date = as.Date(lubridate::date_decimal(date_dec))) %>%
  select(date_dec, date_date, var, value)

# A plot with an integer-based x axis
ggplot(data = time_series,
       mapping = aes(x = date_dec, y = value, color = var)) +
  geom_recessions() +
  geom_line() +
  scale_x_continuous("Year") +
  theme_cmap()

# A plot with a date-based x axis
ggplot(data = time_series,
       mapping = aes(x = date_date, y = value, color = var)) +
  geom_recessions(xformat = "date") +
  geom_line() +
  scale_x_date() +
  theme_cmap()



