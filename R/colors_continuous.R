#' Visualizing CMAP color palettes
#'
#' @describeIn viz_palette Interpolates the range of colors a sequential or
#'   divergent palette offers when used on a continuous scale.
#'
#' @examples
#' # Vizualize a sequential or divergent palette with interpolation
#' viz_gradient("green_teal_blue")
#'
#' @export
viz_gradient <- function(pal, ttl = NULL) {

    # if `pal` is a named sequential or divergent CMAP palette...
    if (fetch_pal(pal[1], c("sequential", "divergent"), "exists")) {
        # use the palette as the title (unless a custom title has been provided)
        if (is.null(ttl) | missing(ttl)){ ttl <- pal }
        # and extract the palette colors
        pal <- fetch_pal(pal)
    } else {
        # otherwise, use the object name as the title (unless a custom title has been provided)
        if (is.null(ttl) | missing(ttl)){ ttl <- deparse(substitute(pal)) }
    }

    pal_func <- grDevices::colorRampPalette(pal, space = "Lab")
    graphics::image(seq_len(300), 1, as.matrix(seq_len(300)), col = pal_func(300),
                    main = ttl, xlab = "", ylab = "",
                    xaxt = "n", yaxt = "n",  bty = "n")
}


#' Continuous palette prep function
#'
#' @param palette A CMAP palette name
#' @param reverse Logical; reverse color order?
#'
#' @noRd
cmap_pal_continuous <- function(palette = "blues", reverse = FALSE) {
    pal <- fetch_pal(palette)
    if (reverse) { pal <- rev(pal) }
    return(grDevices::colorRampPalette(pal))
}


#' Internal helper function to rescale. Credit for idea is due to ijlyttle:
#  \url{https://github.com/tidyverse/ggplot2/issues/3738#issuecomment-583750802}
#'
#' @noRd
mid_rescaler2 <- function(mid) {
    function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
        scales::rescale_mid(x, to, from, mid)
    }
}


#' Apply continuous CMAP palettes (gradients) to ggplot2 aesthetics
#'
#' Pick the function depending on the aesthetic of your ggplot object (fill or
#' color). On diverging palettes, a midpoint can be manually adjusted (defaults
#' to 0). See \code{\link{cmap_gradients}} for a listing of available gradients.
#'
#' @param palette String; Choose from 'cmap_gradients' list
#' @param reverse Logical; Reverse color order?
#' @param middle Numeric; Sets midpoint for diverging color palettes. Default =
#'   0.
#' @param ... Additional parameters passed on to the scale type
#'
#' @examples
#' ggplot(dplyr::filter(grp_over_time, cluster=="Biopharmaceuticals"),
#'        aes(x = year, y = realgrp, color = realgrp)) +
#'     geom_line() +
#'     cmap_color_continuous(palette = "red_purple")
#'
#' @describeIn cmap_fill_continuous for fill aesthetic
#'
#' @export
cmap_fill_continuous <- function(palette = "blues",
                                 reverse = FALSE,
                                 middle = 0,
                                 ...) {
    type <- fetch_pal(palette, return = "type")

    if (type == "divergent") {
        ggplot2::scale_fill_gradientn(
            colours = cmap_pal_continuous(palette, reverse = reverse)(256),
            rescaler = mid_rescaler2(middle),
            ...
        )
    } else if (type == "sequential"){
        ggplot2::scale_fill_gradientn(
            colours = cmap_pal_continuous(palette, reverse = reverse)(256),
            ...
        )
    } else {
        NULL
    }
}


#' @describeIn cmap_fill_continuous for color aesthetic
#'
#' @export
cmap_color_continuous <- function(palette = "blues",
                                  reverse = FALSE,
                                  middle = 0,
                                  ...) {
    type <- fetch_pal(palette, return = "type")

    if (type == "divergent") {
        ggplot2::scale_colour_gradientn(
            colours = cmap_pal_continuous(palette, reverse = reverse)(256),
            rescaler = mid_rescaler2(middle),
            ...
        )
    } else if (type == "sequential"){
        ggplot2::scale_colour_gradientn(
            colours = cmap_pal_continuous(palette, reverse = reverse)(256),
            ...
        )
    } else {
        NULL
    }
}

#' @describeIn cmap_fill_continuous for color aesthetic
#' @export
cmap_colour_continuous <- cmap_color_continuous
