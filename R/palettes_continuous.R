###############################################################################
#
# Continuous palettes
#
#       Short palettes or long? (5 or 9 colors?)
#
#       diagonal along the color guide? Mix hues a little?
#
#       easier naming conventions?
#
#       * Add plot examples
#
###############################################################################


#' CMAP continuous color palettes
#'
#' A sample of comms-approved colors arranged into palettes
#'
#' @examples
#' viz_palette(cmap_gradients$teal_gradient)
#'
#' @export
cmap_gradients <- list(

    red_gradient = c("#3f0000", "#660000", "#8c0000", "#ac0000", "#cc0000",
                     "#d83636", "#e57272", "#efa7a7", "#f8dfdf"),

    orange_gradient = c("#3f1d00", "#662f00", "#8c4100", "#ac5000", "#cc5f00",
                        "#d88236", "#e5a872", "#efc9a7", "#f8ebdf"),

    gold_gradient = c("#3f3400", "#665300", "#8c7200", "#ac8c00", "#cca600",
                      "#d8ba36", "#e5d072", "#efe1a7", "#f8f4df"),

    green_gradient = c("#263f00", "#3d6600", '#548c00', "#67ac00", "#7acc00",
                       "#97d836", "#b7e572", "#d2efa7", "#eef8df"),

    teal_gradient = c("#003f3a", "#00665c", "#008c7e", "#00ac9c", "#00ccb8",
                      "#36d8ca", "#72e5db", "#a7efe8", "#dff8f6"),

    blue_gradient = c("#00303f", "#004e66", "#006b8c", "#0084ac", "#009ccc",
                      "#36b2d8", "#72cae5", "#a7deef", "#dff2f8"),

    purple_gradient = c("#1e003f", "#310066", "#44008c", "#5300ac", "#6300cc",
                        "#8436d8", "#aa72e5", "#c9a7ef", "#ebdff8"),

    grey_gradient = c("#000000", "#181f22", "#2f3d44", "#475c66", "#5e7a87",
                      "#6d8692", "#7b929d", "#8a9ea8", "#9daab3",
                      "#a7b5be", "#b5c1c8", "#c3cdd3", "#d2d9de",
                      "#dbe1e4", "#e3e8eb", "#ecf0f2", "#f5f7f8")
)


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
#' @param palette Choose from 'cmap_palettes' list
#' @param reverse Logical; reverse color order?
#'
#' @export
cmap_fill_continuous <- function(palette = "red_gradient",
                                 reverse = FALSE) {

    scale_fill_gradientn(colours = cmap_pal_gradient(palette,
                                                     reverse = reverse, ...)(256))
}


#' @rdname cmap_fill_continuous
#' @export
cmap_color_continuous <- function(palette = "red_gradient",
                                  reverse = FALSE) {

    scale_colour_gradientn(colours = cmap_pal_gradient(palette,
                                                       reverse = reverse, ...)(256))
}

#' @rdname cmap_fill_continuous
#' @export
cmap_colour_continuous <- cmap_color_continuous

