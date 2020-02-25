#' CMAP continuous color palettes
#'
#' A sample of comms-approved colors arranged into palettes
#'
#' @examples
#' viz_gradient(cmap_gradients$red_purple)
#'
#' @export
cmap_gradients <- list(

    red_gradient = c("#f8dfdf", "#efa7a7", "#e57272", "#d83636", "#cc0000",
                     "#ac0000", "#8c0000", "#660000", "#3f0000"),

    orange_red = c("#f8ebdf", "#e59a72", "#cc3000", "#8c1000", "#3f0000"),

    orange_gradient = c("#f8ebdf", "#efc9a7", "#e5a872", "#d88236", "#cc5f00",
                        "#ac5000", "#8c4100", "#662f00", "#3f1d00"),

    yellow_orange = c("#f8f4df", "#e5c672", "#cc8200", "#8c4100", "#3f1d00"),

    yellow_gradient = c("#f8f4df", "#efe1a7", "#e5d072", "#d8ba36", "#cca600",
                        "#ac8c00", "#8c7200", "#665300", "#3f3400"),

    yellow_green = c("#f8f4df", "#e5e172", "#b4cc00", "#698c00", "#263f00"),

    green_gradient = c("#eef8df", "#d2efa7", "#b7e572", "#97d836", "#7acc00",
                       "#67ac00", "#548c00", "#3d6600", "#263f00"),

    green_teal = c("#eef8df", "#8de572", "#00cc1f", "#008c4b", "#003f3a"),

    teal_gradient = c("#dff8f6", "#a7efe8", "#72e5db", "#36d8ca", "#00ccb8",
                      "#00ac9c", "#008c7e", "#00665c", "#003f3a"),

    teal_blue = c("#dff8f6", "#72e5e3", "#00becc", "#00778c", "#00303f"),

    blue_gradient = c("#dff2f8", "#a7deef", "#72cae5", "#36b2d8", "#009ccc",
                      "#0084ac", "#006b8c", "#004e66", "#00303f"),

    blue_purple = c("#dff2f8", "#72a6e5", "#001bcc", "#17008c", "#1e003f"),

    purple_gradient = c("#ebdff8", "#c9a7ef", "#aa72e5", "#8436d8", "#6300cc",
                        "#5300ac", "#44008c", "#310066", "#1e003f"),

    red_purple = c("#f8dfdf", "#e5729e", "#cc0099", "#77008c", "#1e003f"),

    gray_gradient = c("#f5f7f8", "#ecf0f2", "#e3e8eb", "#dbe1e4", "#d2d9de",
                      "#c3cdd3", "#b5c1c8", "#a7b5be", "#9daab3", "#8a9ea8",
                      "#7b929d", "#6d8692", "#5e7a87", "#475c66", "#2f3d44",
                      "#181f22", "#000000")
)


#' Visualize CMAP color gradients
#'
#' Displays 25 interpolated colors from the cmap continuous palettes.
#' Modeled after viz_palette from the \href{https://github.com/ropenscilabs/ochRe}{ochRe package}
#'
#' @param pal = select from cmap_gradients list
#' @param ttl = display title (optional)
#'
#' @examples
#' viz_gradient(cmap_gradients$orange_red)
#'
#' @export
viz_gradient <- function(pal, ttl = deparse(substitute(pal))) {

    pal_func <- colorRampPalette(pal, space="Lab")

    image(seq_len(30), 1, as.matrix(seq_len(30)), col = pal_func(30),
          main = ttl,
          xlab = "", ylab = "", xaxt = "n", yaxt = "n",  bty = "n")
}


#' Continuous palette prep function
#'
#' @param palette Choose from 'cmap_gradients' list
#' @param reverse Logical; reverse color order?
#'
#' @export
cmap_pal_gradient <- function(palette = "red_gradient",
                              reverse = FALSE) {

    pal <- cmap_gradients[[palette]]

    if (reverse) {
        pal <- rev(pal)
    }

    return(colorRampPalette(pal))
}


#' Apply continuous CMAP palettes to ggplot2 aesthetics
#'
#' Pick the function depending on the aesthetic of your ggplot object (fill or color)
#'
#' @param palette Choose from 'cmap_gradients' list
#' @param reverse Logical; reverse color order?
#'
#' @examples
#' library(dplyr)
#' grp_over_time %>%
#'    filter(cluster=="Biopharmaceuticals") %>%
#'       ggplot(., aes(x = year, y = realgrp, color = realgrp)) +
#'       geom_line() +
#'       cmap_color_continuous(palette = "red_purple")
#'
#' @export
cmap_fill_continuous <- function(palette = "red_gradient",
                                 reverse = FALSE) {

    scale_fill_gradientn(colours = cmap_pal_gradient(palette,
                                                     reverse = reverse)(256))
}


#' @rdname cmap_fill_continuous
#' @export
cmap_color_continuous <- function(palette = "red_gradient",
                                  reverse = FALSE) {

    scale_colour_gradientn(colours = cmap_pal_gradient(palette,
                                                       reverse = reverse)(256))
}

#' @rdname cmap_fill_continuous
#' @export
cmap_colour_continuous <- cmap_color_continuous
