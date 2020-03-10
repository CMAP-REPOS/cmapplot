#' CMAP continuous color palettes
#'
#' A sample of comms-approved colors arranged into palettes
#'
#' @examples
#' viz_gradient(cmap_gradients$multi_red_purple)
#'
#' @export
cmap_gradients <- list(

    # Single-hue sequential gradients
    seq_reds = c("#efa7a7", "#e57272", "#d83636", "#cc0000", "#ac0000", "#8c0000", "#660000"),

    seq_oranges = c("#efc9a7", "#e5a872", "#d88236", "#cc5f00", "#ac5000", "#8c4100", "#662f00"),

    seq_yellows = c("#efe1a7", "#e5d072", "#d8ba36", "#cca600", "#ac8c00", "#8c7200", "#665300"),

    seq_greens = c("#d2efa7", "#b7e572", "#97d836", "#7acc00", "#67ac00", "#548c00", "#3d6600"),

    seq_teals = c("#a7efe8", "#72e5db", "#36d8ca", "#00ccb8", "#00ac9c", "#008c7e", "#00665c"),

    seq_blues = c("#a7deef", "#72cae5", "#36b2d8", "#009ccc", "#0084ac", "#006b8c", "#004e66"),

    seq_purples = c("#c9a7ef", "#aa72e5", "#8436d8", "#6300cc", "#5300ac", "#44008c", "#310066"),

    seq_grays = c("#e3e8eb", "#dbe1e4", "#d2d9de", "#c3cdd3", "#b5c1c8", "#a7b5be", "#9daab3",
                  "#8a9ea8", "#7b929d", "#6d8692", "#5e7a87", "#475c66", "#2f3d44"),

    # Multi-hue sequential gradients
    multi_yellow_orange_red = c("#efe1a7", "#e5bd72", "#d88236", "#cc3000", "#8c0000"),

    multi_green_teal_blue = c("#d2efa7", "#72e584", "#00ccb8", "#00838c", "#004e66"),

    multi_orange_red = c("#efc9a7", "#e59a72", "#cc3000", "#8c1000", "#660000"),

    multi_yellow_orange = c("#efe1a7", "#e5c672", "#cc8200", "#8c4100", "#662f00"),

    multi_yellow_green = c("#f8f4df", "#e5e172", "#b4cc00", "#698c00", "#3d6600"),

    multi_green_teal = c("#d2efa7", "#8de572", "#00cc1f", "#008c4b", "#00665c"),

    multi_teal_blue = c("#a7efe8", "#72e5e3", "#00becc", "#00778c", "#004e66"),

    multi_red_purple = c("#efa7a7", "#e5729e", "#cc0099", "#77008c", "#310066"),


    # Multi-hue diverging gradients
    div_green_red = c("#548c00", "#67ac00", "#7acc00", "#97d836", "#b7e572", "#e3e8eb",
                      "#e57272", "#d83636", "#cc0000", "#8c0000", "#660000"),

    div_yellow_purple = c("#8c7200", "#ac8c00", "#cca600", "#d8ba36", "#e5d072", "#e3e8eb",
                          "#aa72e5", "#8436d8", "#6300cc", "#5300ac", "#44008c"),

    div_orange_blue = c("#8c4100", "#ac5000", "#cc5f00", "#d88236", "#e5a872", "#e3e8eb",
                        "#72cae5", "#36b2d8", "#009ccc", "#0084ac", "#006b8c"),

    div_red_teal = c("#660000", "#8c0000", "#cc0000", "#d83636", "#e57272", "#e3e8eb",
                     "#72e5db", "#36d8ca", "#00ccb8", "#00ac9c", "#008c7e"),

    div_purple_green = c("#44008c", "#5300ac", "#6300cc", "#8436d8", "#aa72e5", "#e3e8eb",
                         "#b7e572"  , "#97d836", "#7acc00", "#67ac00", "#548c00"),

    div_blue_yellow = c("#006b8c", "#0084ac", "#009ccc", "#36b2d8", "#72cae5", "#e3e8eb",
                        "#e5d072", "#d8ba36", "#cca600", "#ac8c00", "#8c7200"),

    div_teal_orange = c("#008c7e", "#00ac9c", "#00ccb8", "#36d8ca", "#72e5db", "#e3e8eb",
                        "#e5a872", "#d88236", "#cc5f00", "#ac5000", "#8c4100")

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
#' viz_gradient(cmap_gradients$multi_orange_red)
#'
#' @export
viz_gradient <- function(pal, ttl = deparse(substitute(pal))) {
    pal_func <- grDevices::colorRampPalette(pal, space = "Lab")
    graphics::image(seq_len(30), 1, as.matrix(seq_len(30)), col = pal_func(30),
                    main = ttl, xlab = "", ylab = "",
                    xaxt = "n", yaxt = "n",  bty = "n")
}


#' Continuous palette prep function
#'
#' @param palette Choose from 'cmap_gradients' list
#' @param reverse Logical; reverse color order?
#'
#' @export
cmap_pal_continuous <- function(palette = "seq_reds", reverse = FALSE) {
    pal <- cmap_gradients[[palette]]
    if (reverse) {
        pal <- rev(pal)
    }
    return(grDevices::colorRampPalette(pal))
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
#'   filter(cluster=="Biopharmaceuticals") %>%
#'   ggplot(aes(x = year, y = realgrp, color = realgrp)) +
#'     geom_line() +
#'     cmap_color_continuous(palette = "multi_red_purple")
#'
#' @export
cmap_fill_continuous <- function(palette = "seq_reds", reverse = FALSE) {
    ggplot2::scale_fill_gradientn(
        colours = cmap_pal_continuous(palette, reverse = reverse)(256)
    )
}

#' @rdname cmap_fill_continuous
#' @export
cmap_color_continuous <- function(palette = "seq_reds", reverse = FALSE) {
    ggplot2::scale_colour_gradientn(
        colours = cmap_pal_continuous(palette, reverse = reverse)(256)
    )
}

#' @rdname cmap_fill_continuous
#' @export
cmap_colour_continuous <- cmap_color_continuous
